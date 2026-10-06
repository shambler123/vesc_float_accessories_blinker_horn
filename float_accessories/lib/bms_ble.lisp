;@const-symbol-strings
; Bluetooth BMS (JBD, Daly, LiPower, LiTech) via the bms-ble-* extensions of
; the VESC Express firmware (shambler123/vesc_express_ble). The firmware keeps
; the connection alive and mirrors the values into the VESC BMS data, this file
; only handles configuration, scanning and the status report for the UI.
@const-start
(def bms-ble-available nil)
(def bms-ble-scan-running nil)

(defun bms-ble-type-int (ty)
    (cond ((eq ty 'jbd) 1) ((eq ty 'daly) 2) ((eq ty 'lipower) 3) ((eq ty 'litech) 4) (t 0)))
(defun bms-ble-int-type (i)
    (cond ((= i 1) 'jbd) ((= i 2) 'daly) ((= i 3) 'lipower) ((= i 4) 'litech) (t 'auto)))

; "A5:C2:37:17:C7:1A" -> (0xA5C237 0x17C71A)
(defun bms-ble-mac-to-ints (mac) {
        (var b (map (fn (x) (str-to-i x 16)) (str-split mac ":")))
        (if (!= (length b) 6) (list -1 -1) {
            (list
                (bits-enc-int (bits-enc-int (ix b 2) 8 (ix b 1) 8) 16 (ix b 0) 8)
                (bits-enc-int (bits-enc-int (ix b 5) 8 (ix b 4) 8) 16 (ix b 3) 8))
        })
})

(defun bms-ble-ints-to-mac (hi lo)
    (str-merge
        (str-from-n (bits-dec-int hi 16 8) "%02X") ":"
        (str-from-n (bits-dec-int hi 8 8) "%02X") ":"
        (str-from-n (bits-dec-int hi 0 8) "%02X") ":"
        (str-from-n (bits-dec-int lo 16 8) "%02X") ":"
        (str-from-n (bits-dec-int lo 8 8) "%02X") ":"
        (str-from-n (bits-dec-int lo 0 8) "%02X")))

(defun bms-ble-saved-mac () {
        (var hi (get-config 'bms-ble-mac-hi))
        (var lo (get-config 'bms-ble-mac-lo))
        (if (and (>= hi 0) (>= lo 0)) (bms-ble-ints-to-mac hi lo) "")
})

; Checks whether the firmware has the bms-ble extensions
(defun bms-ble-init () {
        (setq bms-ble-available (eq (first (trap (bms-ble-state))) 'exit-ok))
        (if (and bms-ble-available (not-eq (bms-ble-state) 'disabled))
            (bms-ble-apply)
            (setq bms-ble-available nil))
})

; Connect to the saved BMS or disconnect, depending on the config
(defun bms-ble-apply () {
        (if bms-ble-available {
            (var mac (bms-ble-saved-mac))
            (if (and (= (get-config 'bms-ble-enabled) 1) (> (str-len mac) 0))
                (bms-ble-connect mac (bms-ble-int-type (get-config 'bms-ble-type)))
                (bms-ble-disconnect))
        })
})

(defun bms-ble-set-enabled (en) {
        (set-config 'bms-ble-enabled (to-i en))
        (save-config)
        (bms-ble-apply)
})

; Called from the UI with the MAC string and type symbol of the chosen BMS
(defun bms-ble-select (mac ty) {
        (var ints (bms-ble-mac-to-ints mac))
        (if (< (ix ints 0) 0) (send-msg "Invalid MAC address") {
            (set-config 'bms-ble-mac-hi (ix ints 0))
            (set-config 'bms-ble-mac-lo (ix ints 1))
            (set-config 'bms-ble-type (bms-ble-type-int ty))
            (set-config 'bms-ble-enabled 1)
            (save-config)
            (send-config)
            (bms-ble-apply)
            (send-status (str-merge "BMS " mac " saved"))
        })
})

(defun bms-ble-forget () {
        (set-config 'bms-ble-mac-hi -1)
        (set-config 'bms-ble-mac-lo -1)
        (set-config 'bms-ble-type 0)
        (save-config)
        (send-config)
        (bms-ble-apply)
})

; Scan in the background and send the result list to the UI:
; "bms-ble-scan name;mac;rssi;type|name;mac;rssi;type|..."
(defun bms-ble-do-scan (secs) {
        (if (and bms-ble-available (not bms-ble-scan-running)) {
            (setq bms-ble-scan-running t)
            (spawn 100 (fn () {
                (bms-ble-scan secs)
                (sleep 0.5)
                (loopwhile (bms-ble-scanning) (sleep 0.2))
                (var out "bms-ble-scan ")
                (loopforeach d (bms-ble-scan-results) {
                    (setq out (str-merge out
                        (ix d 0) ";" (ix d 1) ";" (str-from-n (ix d 2)) ";" (to-str (ix d 3)) "|"))
                })
                (send-data out)
                (setq bms-ble-scan-running nil)
            }))
        })
})

; Status line for the UI, sent together with float-stats:
; "bms-ble avail state type V I SOC cells cmin cmax age soh mac saved-type enabled"
(defun bms-ble-status () {
        (if bms-ble-available
            (send-data (str-merge "bms-ble 1 "
                (to-str (bms-ble-state)) " "
                (to-str (bms-ble-type)) " "
                (str-from-n (bms-ble-get 'voltage) "%.2f ")
                (str-from-n (bms-ble-get 'current) "%.2f ")
                (str-from-n (* 100 (bms-ble-get 'soc)) "%.0f ")
                (str-from-n (bms-ble-get 'cell-count) "%d ")
                (str-from-n (bms-ble-get 'cell-min) "%.3f ")
                (str-from-n (bms-ble-get 'cell-max) "%.3f ")
                (str-from-n (bms-ble-get 'age) "%.0f ")
                (str-from-n (* 100 (bms-ble-get 'soh)) "%.0f ")
                (let ((m (bms-ble-saved-mac))) (if (> (str-len m) 0) m "-")) " "
                (str-from-n (get-config 'bms-ble-type) "%d ")
                (str-from-n (get-config 'bms-ble-enabled) "%d")))
            (send-data "bms-ble 0"))
})
@const-end
