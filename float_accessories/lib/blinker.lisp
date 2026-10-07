@const-start

; Blinker: amber turn signals on the front and rear strips, drawn with the
; esp_led turn effect (FX-TURN, blink style). The strip splits at its centre
; and the signalling half flashes at 1.5 Hz; the other half is dark for as
; long as the signal shows, which is why a signal turns itself off after a
; few flashes. Three sources set it: the VESC Tool control card, the remote
; (see remote-buttons) and the auto blinker below, which reads the lean
; angle while riding. blinker-invert swaps the sides for all of them.

(def blinker-state 0)        ; 0 off, 1 left, 2 right
(def blinker-start-time 0)   ; systime of the last set, for the auto-off
(def blinker-auto nil)       ; t while the auto blinker owns the signal
(def blinker-flashes 4)      ; auto-off after this many flashes, 0 = until cleared
(def CYC-BLINKER 667)        ; one on/off pair in ms: 1.5 Hz
(def BLINKER-COLOR 0x00FF9900u32)

(defun set-blinker (st) {
    (if (!= st blinker-state)
        (dbg DBG-LED (str-merge "blinker " (str-from-n st "%d"))))
    (setq blinker-state st)
    (setq blinker-start-time (systime))
    (setq blinker-auto nil)
})

(defun toggle-blinker (st) (set-blinker (if (= blinker-state st) 0 st)))

(defun blinker-l () (if (= (get-config 'blinker-invert) 1) 2 1))
(defun blinker-r () (if (= (get-config 'blinker-invert) 1) 1 2))

; Auto-off. Called every LED tick, so the signal clears even when nothing
; else happens on the board.
(defun blinker-tick () {
    (if (and (!= blinker-state 0) (> blinker-flashes 0)
             (> (secs-since blinker-start-time) (* blinker-flashes (/ CYC-BLINKER 1000.0))))
        (set-blinker 0))
})

; Front and rear both show the signal, whichever way the board travels: the
; rider behind sees the rear, the one in front sees the front. Recorded after
; the drive modes so it replaces whatever they drew.
(defun led-draw-blinker () {
    (if (!= blinker-state 0) {
        (var mode (if (= blinker-state 1) TURN-LEFT-BLINK TURN-RIGHT-BLINK))
        (seg-want seg-front FX-TURN 0 BLINKER-COLOR 0 front-bri mode CYC-BLINKER)
        (seg-want seg-rear FX-TURN 0 BLINKER-COLOR 0 rear-bri mode CYC-BLINKER)
    })
})

; Auto blinker, from the CAN loop. Leaning past the angle starts the signal
; and keeps it on while the lean lasts; it is released below 80 % of the
; angle (hysteresis) or by the auto-off. Only a signal the auto blinker
; started is cleared here, so it cannot cancel one set from the remote.
(defun blinker-auto-set (st) {
    (if (!= blinker-state st) (set-blinker st))
    (setq blinker-start-time (systime))
    (setq blinker-auto t)
})

(defun blinker-auto-update () {
    (if (and (= (get-config 'auto-blinker-enabled) 1) (>= can-id 0) (running-state)) {
        (var thresh (get-config 'auto-blinker-angle))
        (cond
            ((> roll-angle thresh) (blinker-auto-set (blinker-r)))
            ((< roll-angle (- 0 thresh)) (blinker-auto-set (blinker-l)))
            ((and blinker-auto (< (abs roll-angle) (* thresh 0.8))) (set-blinker 0))
        )
    })
})

@const-end
