@const-start

; Horn: a tone from the motor, played by the ESC through foc-play-tone. The
; sequence runs as one spawned thread on the ESC, so its timing does not
; depend on the CAN link. One tone at a time, and remote input is held back
; while it plays (horn-active, checked in the on-control callback): a
; set-remote-state would cut the tone short.

(def horn-fire-count 0)
(def horn-last-start-time 0)
(def horn-last-duration 0.0)

(defun horn-active ()
    (< (secs-since horn-last-start-time) (+ horn-last-duration 0.3)))

(defun trigger-beep ()
    (if (and (>= can-id 0) (not (horn-active))) {
        (var f (str-from-n (get-config 'horn-freq)))
        (setq horn-last-duration (get-config 'horn-duration))
        (setq horn-last-start-time (systime))
        (setq horn-fire-count (+ horn-fire-count 1))
        (can-cmd can-id (str-merge
            "(spawn (fn () {(foc-play-tone 0 " f " "
            (str-from-n (get-config 'horn-amplitude) "%.1f")
            ")(sleep " (str-from-n horn-last-duration "%.2f")
            ")(foc-play-tone 0 " f " 0)}))"))
    }))

@const-end
