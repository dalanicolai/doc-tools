(require 'image-mode)

(defcustom scrap-horizontal-margin 2
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer)

(defcustom scrap-vertical-margin 2
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer)

(defcustom scrap-overlay-face-bg-color "gray"
  "Background color of overlay, i.e. page separation color."
  :type 'color)

(defcustom scrap-step-size 50
  "Scroll step size in pixels units."
  :type '(choice function integer float))

(defcustom scrap-line-length 120
  "Number of characters per line.
The buffer can display only a number of columns that is equal to
a factor of this number."
  :type 'integer)

(defvar-local scrap-page-aspect-ratios nil
  "List of page aspect ratios.
Each element is a cons of the form (width . height). Usually this
should just be a list of the document its intrinsic page sizes.")

(defvar-local scrap-display-page-function nil
  "Function that sets the overlay's display property.
The function receives the page number as a single
argument (PAGE). The function should use `(scrap-overlay
PAGE)' to add the image of the page as the overlay's
display-property.")

(defcustom scrap-thumbs-width 175
  "Width of thumbnails in pixels.
In order to limit memory usage, the maximum width is 'hard-coded'
to 400."
  :type 'integer
  :group 'pdf-thumbs)

(defcustom scrap-thumbs-side 'left
  "Create thumbs window at this side.
Side should be a symbol left or right (default)."
  :type 'symbol
  :group 'pdf-thumbs)

(defcustom scrap-thumbs-show-page-numbers nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom scrap-thumbs-show-page-after nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom scrap-thumbs-mouse-face nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom scrap-thumbs-mouse-face-color "blue"
  "Maximum number of PNG images per buffer."
  :type 'color
  :group 'pdf-thumbs)

(defvar-local scrap-thumbs-columns nil)
(defvar-local scrap-image-data-function nil)
(defvar-local scrap-fit-height nil)

(defvar-local scrap-last-page 0)

;;; window local value utils
(defun scrap-columns (&optional window)
  "Number of columns.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'columns ,val ,window))))
  (image-mode-window-get 'columns window))

(defun scrap-page-sizes (&optional window)
  "Number of page-sizes.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'page-sizes ,val ,window))))
  (image-mode-window-get 'page-sizes window))

(defun scrap-overlays (&optional window)
  "List of overlays that make up a scroll.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'overlays ,val ,window))))
  (image-mode-window-get 'overlays window))

(defun scrap-overlay (&optional page window)
  "Return the overlay that hold page number PAGE.
Implemented as macro to make it setf'able.
TODO update docstring (no macro anymore)"
  (nth (1- (or page (scrap-current-page)))
        (scrap-overlays window)))

(defun scrap-overlay-get (page prop)
  "List of overlays that make up a scroll.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(overlay-put (nth (1- page) (image-roll-overlays)) ,val))))
  (overlay-get (nth (1- page) (image-roll-overlays)) prop))

(defun scrap-image-p (object)
  (eq (car object) 'image))

(defsubst scrap-overlay-selected-window-filter (overlays)
  (cl-remove-if-not
   (lambda (overlay)
     (eq (overlay-get overlay 'window) (selected-window)))
   overlays))

(defsubst scrap-current-page (&optional window)
  (interactive)
  (overlay-get (car (overlays-at (point))) 'page))

(defsubst scrap-current-size ()
  (interactive)
  (overlay-get (car (scrap-overlay-selected-window-filter
                     (overlays-at (point))))
               'size))

(defsubst scrap-page-pos (page &optional window)
  (overlay-start (nth (1- page) (scrap-overlays window))))

(define-derived-mode scrap-mode special-mode "Scrap"

  (setq cursor-type nil)

  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `scrap-redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and add lines from
  ;; `image-mode-reapply-winprops' at the start of `scrap-redisplay'.
  (add-hook 'window-configuration-change-hook 'scrap-redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'scrap-new-window-function nil t)
  (setq image-mode-winprops-alist nil)

  (dolist (m '(global-hl-line-mode))
    (if (fboundp m)
        (funcall m 0))))

(defun scrap-redisplay ()
  ;; if new window then run `image-mode-new-window-functions' and setup new
  ;; window winprops(-alist)
  (image-mode-winprops nil t)

  (let* ((pages scrap-last-page)
         (columns (or (scrap-columns) 1))
         (page-sizes (scrap-desired-page-sizes
                          scrap-aspect-ratios
                          nil
                          columns
                          scrap-horizontal-margin
                          scrap-vertical-margin))
         ;; (make-list pages (if (functionp scrap-demo-page-size)
         ;;                      (funcall scrap-demo-page-size)
         ;;                    scrap-demo-page-size))))
         (n 0))

    (dolist (page-size page-sizes)
      (let* ((page-width (+ (car page-size) (* 2 scrap-horizontal-margin)))
             (overlay-heigth (+ (cdr page-size) (* 2 scrap-vertical-margin)))
             (o (nth n (scrap-overlays))))
        ;; (when scrap-center
        ;;   (overlay-put o 'before-string
        ;;                (when (> (window-pixel-width) page-width)
        ;;                  (propertize " " 'display
        ;;                              `(space :align-to
        ;;                                      (,(floor (/ (- (window-pixel-width) page-width) 2))))))))
        (overlay-put o 'display `(space . (:width (,page-width) :height (,overlay-heigth))))
        ;; (overlay-put o 'face `(:background ,scrap-overlay-face-bg-color))
        (overlay-put o 'face `(:background ,scrap-overlay-face-bg-color))
        (overlay-put o 'size page-size)
        (setq n (1+ n))))
    ;; (redisplay t) ;NOTE does not help for display issue
    (setf (scrap-columns) columns)
    (setf (scrap-page-sizes) page-sizes))
  ;; (sit-for 0.01) ;NOTE does not help for display issue
  (scrap-update))

(defun scrap-new-window-function (winprops)
  (if (not (overlays-at 1))
      (let (overlays
            (pages scrap-last-page)
            (win (car winprops))
            (inhibit-read-only t))

        (erase-buffer)

        ;; here we only add the 'page' and 'window' overlay-properties, we add
        ;; more properties/information as soon as it becomes available in the
        ;; 'scrap-redisplay' function
        (dotimes (i pages)
          (let ((o (make-overlay
                    (prog1 (point) (insert (make-string scrap-line-length (string-to-char " "))))
                    (point))))
            (insert "\n")
            (overlay-put o 'page  (1+ i))
            (overlay-put o 'window win)
            (push o overlays)))
        (delete-char -1)
        (image-mode-window-put 'overlays (nreverse overlays))
        (set-buffer-modified-p nil))

;;       ;; required to make `pdf-view-redisplay-some-windows' call `pdf-view-redisplay'
;;       (when-let (fun scrap-set-redisplay-flag-function)
;;         (funcall fun)))
    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (scrap-overlays))))
      (image-mode-window-put 'overlays ols winprops)))

;; ;; initial `scrap-redisplay' needs to know which page(s) to display
;; (setf (scrap-current-page (car winprops))
;;       (or (scrap-current-page (car winprops)) 1)))
  (goto-char (point-min)))

(defun scrap-desired-page-sizes (&optional aspect-ratios fit-height columns h-margin v-margin)
  (mapcar
   (lambda (s)
     (let* ((win-width (float (window-text-width nil t)))
            (column-width (/ win-width (or columns 1)))
            (v-scale-factor (when (or fit-height scrap-fit-height)
                              (/ (float (window-text-height nil t)) (cdr s))))
            (use-vertical-scaling (when v-scale-factor
                                    (< (* v-scale-factor (car s)) column-width)))
            (scaling-factor (if use-vertical-scaling
                                v-scale-factor
                              (/ (float column-width) (car s))))
            (target-width (- (* scaling-factor (car s))
                             (if use-vertical-scaling 0 (* 2 (or h-margin 0))))))
       (cons (floor target-width)
                   (floor (- (* (/ target-width (car s)) ;we correct the ratio t.i.c. the h-margins
                                (if (proper-list-p s) (cadr s) (cdr s)))
                             (* 2 (or v-margin 0)))))))
   (or aspect-ratios scrap-aspect-ratios)))

(defun scrap-fit-toggle ()
  (interactive)
  (setq scrap-fit-height (not scrap-fit-height))
  (scrap-redisplay))


(defun scrap-set-columns (columns)
  (interactive "p")
  (when (/= (% scrap-line-length columns) 0)
    (user-error
     "The current 'scap-line-length' only supports %s number of
columns"
     (mapconcat #'number-to-string
                (cl-loop for f from 1 to scrap-line-length
                         when (eq (% scrap-line-length f) 0)
                         collect f)
                ",")))
  (let ((scrap-page-sizes (scrap-desired-page-sizes
                           scrap-aspect-ratios nil columns
                           scrap-horizontal-margin scrap-vertical-margin))
        (pos 1)
        (i 0))
    (dolist (o (scrap-overlays))
      (let ((s (nth i scrap-page-sizes)))
        (move-overlay o pos (setq pos (+ pos (/ scrap-line-length columns))))
        (overlay-put o 'size s)
        (overlay-put o 'display `(space . (:width (,(car s)) :height (,(cdr s))))))
      (when (eq (% i columns) (1- columns))
        (setq pos (1+ pos)))
      (setq i (1+ i)))
    (setf (scrap-page-sizes) scrap-page-sizes))
  (setf (scrap-columns) columns)
  (sit-for 0.001)
  (scrap-update))

(defun scrap-update ()
  (interactive)

  (let* ((visible (scrap-visible-pages))
         (displayed (scrap-displayed-images)))
    ;NOTE the condition might only be relevant when new window (not sure)
    (unless (= (length visible) scrap-last-page)
      (dolist (p (cl-set-difference displayed visible))
        (scrap-undisplay-page p))
      (let* ((pages (cl-set-difference visible displayed))
             (non-exisiting-images (cl-remove-if (lambda (p)
                                                   (let* ((display-prop (scrap-overlay-get p 'display)))
                                                     (when (scrap-image-p display-prop)
                                                       (= (car (image-size display-prop)) w))))
                                                 pages)))
        (when non-exisiting-images
          (scrap-display-page non-exisiting-images))))))


(defun scrap-visible-pages ()
  (interactive)
  (sit-for 0.001) ;when overlays are not yet 'filled', and there sizes are still
                  ;small, then overlays-in returns too many overlays
  (djvu-return (mapcar (lambda (o)
                         (overlay-get o 'page))
                       (scrap-overlay-selected-window-filter
                        (overlays-in (min (- (window-start) scrap-line-length) 1)
                                     (+ (window-end) scrap-line-length))))))

(defun scrap-displayed-images ()
  (interactive)
  (let ((im-overlays (seq-filter (lambda (ov)
                                   (eq (car (overlay-get ov 'display))
                                       'image))
                                  (scrap-overlay-selected-window-filter
                                   (overlays-in (point-min) (point-max))))))
    (djvu-return (mapcar (lambda (o)
                           (overlay-get o 'page))
                         im-overlays))))

(defun scrap-undisplay-page (page)
  ;; (print "of hier")
  (pcase-let* ((`(,w . ,h) (nth (1- page) (scrap-page-sizes))))
    (overlay-put (scrap-overlay page)
                 'display `(space . (:width (,w) :height (,h))))))

;; (defun scrap-display-page (pages &optional _)
;;   (let ((page (car pages)))
;;     (pcase-let* ((`(,w . ,h) (nth (1- page) (scrap-page-sizes)))
;;                  (svg (svg-create w h)))
;;       (svg-rectangle svg 0 0 w h :fill "white")
;;       (svg-text svg (number-to-string page)
;;                 :font-size 40
;;                 :fill "red"
;;                 :x 5 :y 40)
;;       (overlay-put (nth (1- page) (scrap-overlays))
;;                    'display (svg-image svg :margin '(1 . 1) :ascent 80)))))
;; 'display (svg-image svg ))))

(defun scrap-display-page (pages &optional force)
  ;; (print "RUNNING" #'external-debugging-output)
  (pcase-let* ((`(,w . ,h) (nth (1- (car pages)) (scrap-page-sizes))))
    (when non-exisiting-images
      (dolist (page non-exisiting-images)
        (let ((scale (/ (float w) (car (nth (1- page) scrap-aspect-ratios))))
              (svg (svg-create w h))
              (data (funcall scrap-image-data-function page w))
              (image nil))
          (unless w (print "NO W" #'external-debugging-output))
          (cond (scrap-djvu-svg-embed
                 (svg-embed svg data "image/x-portable-bitmap" t)
                 ;; (svg-embed svg data "image/tiff" t)
                 (when-let (rects (alist-get page papyrus-current-rectangles))
                   (mapcar (lambda (c)
                             (apply #'svg-rectangle
                                    svg
                                    (append (mapcar (lambda (m)
                                                      (round (* scale m)))
                                                    (papyrus-coords-to-svg
                                                     (cdr (nth (1- page) scrap-aspect-ratios))
                                                     (seq-subseq c 0 4)))
                                            (seq-subseq c 4))))
                           rects)
                   )
                 ;; (svg-rectangle svg 0 0 100 100 :fill "blue")
                 (setq image (svg-image svg :margin `(,scrap-horizontal-margin . ,scrap-vertical-margin)))
                 (image-property image :type))
                (t
                 ;NOTE expects data
                 (setq image (create-image data 'pbm t
                                           :margin `(,scrap-horizontal-margin . ,scrap-vertical-margin)))))
      ;; (when scrap-center
      ;;   (overlay-put o 'before-string
      ;;                (when (> (window-pixel-width) w)
      ;;                  (propertize " " 'display
      ;;                              `(space :align-to
      ;;                                      (,(floor (/ (- (window-pixel-width) w) 2))))))))
          (overlay-put (scrap-overlay page) 'display image))))))
(defun scrap-goto-page-start ()
  (interactive)
  (image-set-window-vscroll 0))

;; NOTE code based on (taken from) `pdf-view-goto-page'.
(defun scrap-goto-page (page &optional window)
  "Go to PAGE in document."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  ;; (unless (and (>= page 1)
               ;; (<= page scrap-last-page))
    ;; (error "No such page: %d" page))
  (unless window
    (setq window
          ;; (if (pdf-util-pdf-window-p)
          (selected-window)
          ;; t)))
          ))
  (save-selected-window
    ;; Select the window for the hooks below.
    (when (window-live-p window)
      (select-window window 'norecord))
    (let ((changing-p
           (not (eq page (scrap-current-page window)))))
      ;; (when changing-p
      ;;   (run-hooks 'scrap-before-change-page-hook)
      ;;   (run-hooks 'scrap-change-page-hook))
      (when (window-live-p window)
        (goto-char (scrap-page-pos page))
        (scrap-update))
      ;; (when changing-p
      ;;   (run-hooks 'scrap-after-change-page-hook))
      ;; (when changing-p
      ;;   (pdf-view-deactivate-region)
      ;;   (force-mode-line-update)
      ;;   (run-hooks 'pdf-view-after-change-page-hook))))
      nil)))

(defun scrap-scroll-forward (&optional screen)
  (interactive)
  (let ((new-vscroll (+ (window-vscroll nil t) (if screen
                                                   (window-text-height nil t)
                                                 scrap-step-size))))
    (cond ((> (+ new-vscroll) (cdr (scrap-current-size)))
           (forward-line)
           (image-set-window-vscroll 0) ;or set to vertical margin
           (scrap-update))
          ((> (+ new-vscroll (window-text-height nil t)) (cdr (scrap-current-size)))
           (cond ((and (= (scrap-columns) 1) (= (scrap-current-page) scrap-last-page))
                  (message "End of buffer"))
                 (t
                  (image-set-window-vscroll new-vscroll)
                  (scrap-update))))
          (t (image-set-window-vscroll new-vscroll)))))

(defun scrap-scroll-backward (&optional screen)
  (interactive)
  (let ((new-vscroll (- (window-vscroll nil t) (if screen
                                                   (window-text-height nil t)
                                                 scrap-step-size))))
    (cond ((< new-vscroll 0)
           (if (<= (scrap-current-page) (scrap-columns))
               (image-set-window-vscroll 0) ;or set to vertical margin
               (message "Beginning of buffer")
             (forward-line -1)
             (image-set-window-vscroll (cdr (scrap-current-size)))
             (scrap-update)))
          ((< new-vscroll (- (window-text-height nil t) (cdr (scrap-current-size))))
           (image-set-window-vscroll new-vscroll)
           (scrap-update))
          (t (image-set-window-vscroll new-vscroll)))))

(defun scrap-scroll-screen-forward ()
  (interactive)
  (scrap-scroll-forward t))

(defun scrap-scroll-screen-backward ()
  (interactive)
  (scrap-scroll-forward t))

(defun scrap-scroll-page-or-row (n)
  (let ((current-range (cons (window-start) (window-end))))
    (forward-line n)
    (redisplay)
    (unless (equal (cons (window-start) (window-end)) current-range)
      (scrap-update))))

(defun scrap-next-page (&optional n)
  (interactive "p")
  (scrap-scroll-page-or-row (or n 1)))

(defun scrap-previous-page (&optional n)
  (interactive "p")
  (scrap-scroll-page-or-row (- (or n 1))))

;;; thumbnails
(defun scrap-thumbs (&optional columns)
  "Show thumbs in a side window.
The number of COLUMNS can be set with a numeric prefix argument."
  (interactive "p")
  (let* ((buffer-name "*thumbs*")
         (buf (get-buffer buffer-name))
         (file (buffer-file-name))
         (output-dir (concat "/tmp/"
                             (file-name-as-directory (file-name-base file))))
         (last-page scrap-last-page)
         (win (selected-window)))
    (or (and buf
             (= (buffer-local-value 'scrap-thumbs-columns buf) columns)) 

        (with-current-buffer (get-buffer-create buffer-name)
          (unless (file-exists-p output-dir)
            (djvu-decode-thumbs file))
          (scrap-thumbs-mode)
          (setq scrap-thumbs-columns columns)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (let* ((columns (or columns 1))
                   (w 100)
                   (h (* w 1.6))
                   (source-svg (svg-create w h)))
              (svg-rectangle source-svg 0 0 w h :fill "white")
              (dotimes (i last-page)
                (let* ((p (1+ i))
                       (svg (copy-sequence source-svg))
                       (im (create-image (print (concat output-dir
                                                  (format "thumb%d.tif" p)))
                                         'tiff
                                         nil
                                         :margin '(2 . 1))))
                  (apply #'insert-button (format "%03d " p)
                         (append (list 'page (number-to-string p)
                                       'win win
                                       'face 'default
                                       'display im
                                       'action (lambda (b)
                                                 (with-selected-window win
                                                   (scrap-goto-page
                                                    (string-to-number (button-get b 'page))
                                                    (button-get b 'win)))))
                                 (if scrap-thumbs-show-page-numbers
                                     (list (if scrap-thumbs-show-page-after
                                               'after-string
                                             'before-string)
                                           (format "%4d " p)) ;either this or help-echo
                                   (list 'help-echo (number-to-string p)))
                                 (when scrap-thumbs-mouse-face
                                   (list 'mouse-face (list :background scrap-thumbs-mouse-face-color))))))
                (when (= (% i columns) (1- columns)) (insert "\n")))
              (goto-char (point-min))

              (unless scrap-thumbs-show-page-numbers
                (add-hook 'post-command-hook #'display-local-help nil t))))))

    (scrap-thumbs-show columns)))

(defun scrap-thumbs-show (columns)
  (let ((win (split-window nil
                           (- (+ (* columns (float (+ scrap-thumbs-width 4 (if scrap-thumbs-show-page-numbers 41 0))))
                                 (- (window-pixel-width)
                                    (window-body-width nil t))))
                           scrap-thumbs-side t)))
    (set-window-buffer win "*thumbs*")
    (set-window-dedicated-p win t)
    (select-window win)))


(define-derived-mode scrap-thumbs-mode special-mode "ScrapThumbs")

(setq scrap-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'scrap-scroll-forward)
        (define-key map (kbd "<down>") 'scrap-scroll-forward)
        (define-key map (kbd "C-p") 'scrap-scroll-backward)
        (define-key map (kbd "<up>") 'scrap-scroll-backward)
        (define-key map (kbd "<wheel-down>") 'scrap-scroll-forward)
        (define-key map (kbd "<wheel-up>") 'scrap-scroll-backward)
        ;; (define-key map (kbd "<mouse-5>") 'scrap-scroll-forward)
        ;; (define-key map (kbd "<mouse-4>") 'scrap-scroll-backward)
        (define-key map "n" 'scrap-next-page)
        (define-key map (kbd "<next>") 'scrap-next-page)
        (define-key map "p" 'scrap-previous-page)
        (define-key map (kbd "<prior>") 'scrap-previous-page)
        (define-key map (kbd "S-<next>") 'scrap-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'scrap-scroll-screen-backward)
        (define-key map [remap goto-line] 'scrap-goto-page)
        (define-key map "f" 'scrap-fit-toggle)
        (define-key map "c" 'scrap-set-columns)
        (define-key map "t" 'scrap-thumbs)
        map))


(provide 'scrap)
;;; scrap.el ends here
