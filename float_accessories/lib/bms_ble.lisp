@const-start

; Bluetooth BMS (JBD, Daly, LiPower, LiTech, JK, ANT, Stoked Stock) through
; the bms-ble-* extensions of the vesc_express_ble firmware
; (github.com/shambler123/vesc_express_ble). The firmware scans, connects,
; keeps the link up next to the VESC Tool connection and mirrors the values
; into the VESC BMS data. This module only owns the configuration (which
; device, which protocol), runs the scan for the QML page and reports the
; state. On a firmware without the extensions it is inert.

(def bms-ble-available nil)
(def bms-ble-scan-running nil)
; bms_ble_type enum order in the config
(def bms-ble-types '(auto jbd daly lipower litech jk ant ssbms))

(defun bms-ble-int-type (i) (if (and (>= i 0) (< i 8)) (ix bms-ble-types i) 'auto))
(defunret bms-ble-type-int (ty) {
    (looprange i 0 8 (if (eq (ix bms-ble-types i) ty) (return i)))
    0
})

; "A5:C2:37:17:C7:1A" -> (0xA5C237 0x17C71A), (-1 -1) when malformed
(defun bms-ble-mac-to-ints (mac) {
    (var b (map (fn (x) (str-to-i x 16)) (str-split mac ":")))
    (if (!= (length b) 6) (list -1 -1)
        (list (bits-enc-int (bits-enc-int (ix b 2) 8 (ix b 1) 8) 16 (ix b 0) 8)
              (bits-enc-int (bits-enc-int (ix b 5) 8 (ix b 4) 8) 16 (ix b 3) 8)))
})

; "" when no BMS is saved
(defun bms-ble-saved-mac () {
    (var hi (get-config 'bms-ble-mac-a))
    (var lo (get-config 'bms-ble-mac-b))
    (if (or (< hi 0) (< lo 0)) ""
        (str-join (map (fn (i) (str-from-n
            (bits-dec-int (if (< i 3) hi lo) (* 8 (- 2 (mod i 3))) 8) "%02X")) (range 6)) ":"))
})

; Probes the firmware for the extensions, then connects per config
(defun bms-ble-init () {
    (setq bms-ble-available (eq (first (trap (bms-ble-state))) 'exit-ok))
    (if (and bms-ble-available (not-eq (bms-ble-state) 'disabled))
        (bms-ble-apply)
        (setq bms-ble-available nil))
})

; Connect to the saved BMS or drop it, per config. Safe to repeat: the
; firmware keeps an existing link to the same target.
(defun bms-ble-apply ()
    (if bms-ble-available {
        (var mac (bms-ble-saved-mac))
        (if (and (= (get-config 'bms-ble-enabled) 1) (> (str-len mac) 0))
            (bms-ble-connect mac (bms-ble-int-type (get-config 'bms-ble-type)))
            (bms-ble-disconnect))
    }))

; From the QML page: MAC string and protocol symbol of the chosen device.
; Stored at once (the page re-reads the config) and enables the feature.
(defun bms-ble-select (mac ty) {
    (var ints (bms-ble-mac-to-ints mac))
    (if (< (ix ints 0) 0) (send-msg "Invalid MAC") {
        (set-config 'bms-ble-mac-a (ix ints 0))
        (set-config 'bms-ble-mac-b (ix ints 1))
        (set-config 'bms-ble-type (bms-ble-type-int ty))
        (set-config 'bms-ble-enabled 1)
        (ext-facfg-store)
        (bms-ble-apply)
        (send-status "BMS saved")
    })
})

(defun bms-ble-forget () {
    (set-config 'bms-ble-mac-a -1)
    (set-config 'bms-ble-mac-b -1)
    (set-config 'bms-ble-type 0)
    (ext-facfg-store)
    (bms-ble-apply)
    (send-status "BMS forgotten")
})

; Scan in the background and send the result to the page:
; "bms-ble-scan name;mac;rssi;type|..."
(defun bms-ble-do-scan (secs)
    (if (and bms-ble-available (not bms-ble-scan-running)) {
        (setq bms-ble-scan-running t)
        (spawn 100 (fn () {
            (bms-ble-scan secs)
            (sleep 0.5)
            (loopwhile (bms-ble-scanning) (sleep 0.2))
            (var out "bms-ble-scan ")
            (loopforeach d (bms-ble-scan-results)
                (setq out (str-merge out (ix d 0) ";" (ix d 1) ";"
                    (str-from-n (ix d 2)) ";" (to-str (ix d 3)) "|")))
            (send-data out)
            (setq bms-ble-scan-running nil)
        }))
    }))

; Sent with every status poll:
; "bms-ble avail state type V I SOC cells cmin cmax age soh mac saved-type
;  enabled rst stage-before stage heap crash min-heap"
(defun bms-ble-status ()
    (send-data (if bms-ble-available
        (str-join (list "bms-ble 1"
            (to-str (bms-ble-state)) (to-str (bms-ble-type))
            (str-from-n (bms-ble-get 'voltage) "%.2f")
            (str-from-n (bms-ble-get 'current) "%.2f")
            (str-from-n (* 100 (bms-ble-get 'soc)) "%.0f")
            (str-from-n (bms-ble-get 'cell-count) "%d")
            (str-from-n (bms-ble-get 'cell-min) "%.3f")
            (str-from-n (bms-ble-get 'cell-max) "%.3f")
            (str-from-n (bms-ble-get 'age) "%.0f")
            (str-from-n (* 100 (bms-ble-get 'soh)) "%.0f")
            (let ((m (bms-ble-saved-mac))) (if (= (str-len m) 0) "-" m))
            (str-from-n (get-config 'bms-ble-type) "%d")
            (str-from-n (get-config 'bms-ble-enabled) "%d")
            (bms-ble-stats-str)) " ")
        "bms-ble 0")))

; Firmware diagnostics, dashes on a firmware without them. crash is "-" or
; the crash-info fields (reason, description, task, pc, ra, sp, mcause,
; mtval, details) joined by |, spaces replaced by _.
(defun bms-ble-stats-str () {
    (var r (trap (bms-ble-stats)))
    (if (eq (first r) 'exit-ok) {
        (var s (second r))
        (str-join (list (str-from-n (ix s 0)) (str-from-n (ix s 1)) (str-from-n (ix s 2))
            (str-from-n (ix s 3)) (bms-ble-crash-str) (str-from-n (ix s 4))) " ")
    } "- - - - - -")
})

(defun bms-ble-crash-str () {
    (var r (trap (crash-info)))
    (if (and (eq (first r) 'exit-ok) (second r))
        (str-join (map (fn (x) (if (eq (type-of x) type-array)
                                   (str-replace x " " "_")
                                   (str-from-n x "0x%08X")))
                       (take (second r) 9)) "|")
        "-")
})

@const-end
