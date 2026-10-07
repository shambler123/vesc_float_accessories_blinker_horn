@const-start

; Blinker: amber turn signal on the front and rear strips, drawn with the
; esp_led turn effect (FX-TURN blink style, 1.5 Hz). The other half of the
; strip is dark while it flashes, hence the auto-off after a few flashes.
; Sources: the Control tab, the remote (remote-buttons.lisp) and the auto
; blinker from the roll angle. blinker-invert swaps the sides for all.

(def blinker-state 0)      ; 0 off, 1 left, 2 right
(def blinker-start-time 0)
(def blinker-auto nil)     ; t while the auto blinker owns the signal
(def CYC-BLINKER 667)      ; ms per on/off pair
(def BLINKER-FLASHES 4)    ; auto-off after this many

(defun set-blinker (st) {
    (setq blinker-state st)
    (setq blinker-start-time (systime))
    (setq blinker-auto nil)
})

(defun toggle-blinker (st) (set-blinker (if (= blinker-state st) 0 st)))
(defun blinker-l () (if (= (get-config 'blinker-invert) 1) 2 1))
(defun blinker-r () (if (= (get-config 'blinker-invert) 1) 1 2))

; Auto-off, every LED tick
(defun blinker-tick ()
    (if (and (!= blinker-state 0)
             (> (secs-since blinker-start-time) (* BLINKER-FLASHES (/ CYC-BLINKER 1000.0))))
        (set-blinker 0)))

; Recorded after the drive modes, so it replaces what they drew
(defun led-draw-blinker ()
    (if (!= blinker-state 0) {
        (var mode (if (= blinker-state 1) TURN-LEFT-BLINK TURN-RIGHT-BLINK))
        (seg-want seg-front FX-TURN 0 0x00FF9900u32 0 front-bri mode CYC-BLINKER)
        (seg-want seg-rear FX-TURN 0 0x00FF9900u32 0 rear-bri mode CYC-BLINKER)
    }))

; From the CAN loop. Leaning past the angle starts the signal and holds it;
; below 80 % of the angle it is released. Only clears what it started.
(defun blinker-auto-update ()
    (if (and (= (get-config 'auto-blinker-enabled) 1) (>= can-id 0) (running-state)) {
        (var thresh (get-config 'auto-blinker-angle))
        (var side (cond ((> roll-angle thresh) (blinker-r))
                        ((< roll-angle (- 0 thresh)) (blinker-l))
                        (t 0)))
        (if (!= side 0) {
            (if (!= blinker-state side) (set-blinker side))
            (setq blinker-start-time (systime))
            (setq blinker-auto t)
        } (if (and blinker-auto (< (abs roll-angle) (* thresh 0.8)))
            (set-blinker 0)))
    }))

@const-end
