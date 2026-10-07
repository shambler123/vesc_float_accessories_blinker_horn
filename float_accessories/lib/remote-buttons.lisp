@const-start

; Remote button mapping, fed from the pubmote on-control callback with every
; input packet:
;   X button (jsx goes negative): 1 click = left blinker, 2 clicks = right
;   blinker - a click on the side already showing clears it - and 3 or more
;   = horn. Clicks are counted until the button has been up for 0.4 s.
;   Z button held for 0.8 s = horn.
; jsx reads 0.0 up and -1.0 pressed. A remote that comes up with jsx already
; negative is ignored until it has been seen at 0 once, so a stuck or
; mis-centred stick cannot fire anything on connect.

(def rb-jsx-seen-zero nil)
(def rb-jsx-prev nil)
(def rb-click-count 0)
(def rb-last-release-time 0)
(def rb-z-prev 0)
(def rb-z-start 0)
(def rb-z-fired nil)
(def RB-CLICK-GAP 0.4)
(def RB-HOLD-TIME 0.8)

(defun remote-buttons-update (jsx bt-z) {
    ; Z hold. Fires once per hold, however long it lasts.
    (if (= bt-z 1) {
        (if (= rb-z-prev 0) {
            (setq rb-z-start (systime))
            (setq rb-z-fired nil)
        })
        (if (and (not rb-z-fired) (> (secs-since rb-z-start) RB-HOLD-TIME)) {
            (trigger-beep)
            (setq rb-z-fired t)
        })
    } (setq rb-z-fired nil))
    (setq rb-z-prev bt-z)

    ; X clicks
    (if (>= jsx 0.0) (setq rb-jsx-seen-zero t))
    (if rb-jsx-seen-zero {
        (var pressed (< jsx 0.0))
        (if (and pressed (not rb-jsx-prev))
            (setq rb-click-count (+ rb-click-count 1)))
        (if (and (not pressed) rb-jsx-prev)
            (setq rb-last-release-time (systime)))
        (setq rb-jsx-prev pressed)
        (if (and (> rb-click-count 0) (not pressed)
                 (> (secs-since rb-last-release-time) RB-CLICK-GAP)) {
            (cond
                ((= rb-click-count 1) (toggle-blinker (blinker-l)))
                ((= rb-click-count 2) (toggle-blinker (blinker-r)))
                (t (trigger-beep)))
            (setq rb-click-count 0)
        })
    })
})

@const-end
