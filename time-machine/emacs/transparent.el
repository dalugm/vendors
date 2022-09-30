(defvar my-active-transparency 90
  "A value from the range (0..100), in increasing opacity.
Describes the transparency level of a frame when it's active or selected.
Transparency can be toggled through `toggle-transparency'.")

(defvar my-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity.
Describes the transparency level of a frame when it's inactive or deselected.
Transparency can be toggled through `toggle-transparency'.")

(defvar my-current-opacity (frame-parameter (unless 'display-graphic-p)
                                            'alpha)
  "Record current opacity.")

(defun my-set-transparency (value)
  "Set the VALUE of transparency of the frame window."
  (interactive "nSet transparency (0 is transparent- 100 is opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun my-toggle-transparency (&optional frame)
  "Toggle between transparent and opaque state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let ((alpha (frame-parameter frame 'alpha))
        (setting (cons my-active-transparency
                       my-inactive-transparency)))
    (if (equal alpha setting)
        (my-disable-transparency frame)
      (my-enable-transparency frame setting))))

(defun my-enable-transparency (&optional frame alpha)
  "Enable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame.
ALPHA is a pair of active and inactive transparency values.
The default value for ALPHA is based on
`my-active-transparency' and
`my-inactive-transparency'."
  (interactive)
  (let ((alpha-setting (or alpha
                           (cons my-active-transparency
                                 my-inactive-transparency))))
    (set-frame-parameter frame 'alpha alpha-setting)))

(defun my-disable-transparency (&optional frame)
  "Disable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'alpha '(100 . 100)))

(defun my-increase-transparency (&optional frame)
  "Increase transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter frame 'alpha
                           (cons increased-alpha increased-alpha)))))

(defun my-decrease-transparency (&optional frame)
  "Decrease transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter frame 'alpha
                           (cons decreased-alpha decreased-alpha)))))

(defun my-transient-window-transparency ()
  "Transient version of transparency."
  (interactive)
  (let ((echo-keystrokes nil))
    (message "Transparency: [s]et [t]oggle [-] [=]")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?s] #'my-set-transparency)
       (define-key map [?t] #'my-toggle-transparency)
       (define-key map [?-] #'my-increase-transparency)
       (define-key map [?=] #'my-decrease-transparency)
       map)
     t)))
