@const-start

; Horn: a tone from the motor, played by the ESC through foc-play-tone. The
; whole sequence runs as one spawned thread on the ESC, so its timing does
; not depend on the CAN link. Remote input is held back while it plays (the
; on-control callback in float_accessories.lisp checks horn-active):
; set-remote-state would cut the tone short.

(def horn-fire-count 0)
(def horn-last-start-time 0)
(def horn-last-duration 0.0)

(defun horn-active ()
    (< (secs-since horn-last-start-time) (+ horn-last-duration 0.3)))

(defun horn-sequence () {
    (var freq (str-from-n (get-config 'horn-freq)))
    (var amp (str-from-n (get-config 'horn-amplitude) "%.1f"))
    (var dur (get-config 'horn-duration))
    (setq horn-last-duration dur)
    (setq horn-last-start-time (systime))
    (can-cmd can-id (str-merge
        "(spawn (fn () {(foc-play-tone 0 " freq " " amp ")"
        "(sleep " (str-from-n dur "%.2f") ")"
        "(foc-play-tone 0 " freq " 0)}))"))
})

; One tone at a time: a second trigger while it plays is dropped, so a held
; button cannot queue a string of them.
(defun trigger-beep () {
    (if (and (>= can-id 0) (not (horn-active))) {
        (setq horn-fire-count (+ horn-fire-count 1))
        (dbg DBG-CAN "horn")
        (horn-sequence)
    })
})

@const-end
