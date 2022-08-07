;; TODO convert namespace to scrap

(require 'image-mode)
(require 'svg)

(defgroup image-roll nil
  "Image roll configurations."
  :group 'applications
  :version "28.1")

(defcustom image-roll-vertical-margin 2
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer)

(defcustom image-roll-overlay-face-bg-color "gray"
  "Background color of overlay, i.e. page separation color."
  :type 'color)

(defcustom image-roll-step-size 50
  "Scroll step size in pixels units."
  :type '(choice function integer float))

(defcustom image-roll-center nil
  "When non-nil, center the roll horizontally in the window."
  :type 'boolean)

(defvar-local image-roll-page-aspect-ratios nil
  "List of page aspect ratios.
Each element is a cons of the form (width . height). Usually this
should just be a list of the document its intrinsic page sizes.")

;; (defvar-local image-roll-columns nil)

(defvar-local image-roll-page-sizes nil
  "Function that should return page-sizes of document.
The function should return a list of conses of the form (WIDTH .
HEIGHT), both numbers.")

(defun image-roll-overlays (&optional window)
  "List of overlays that make up a scroll."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'overlays ,val ,window))))
  (image-mode-window-get 'overlays window))

(defun image-roll-overlays (&optional window)
  "List of overlays that make up a scroll."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'overlays ,val ,window))))
  (image-mode-window-get 'overlays window))

(defun image-roll-current-page ()
  (interactive)
  (overlay-get (car (overlays-at (point))) 'page))

(defun image-roll-displayed-images ()
  (interactive)
  (let ((im-overlays (seq-filter (lambda (ov)
                                   (eq (car (overlay-get ov 'display))
                                       'image))
                                 (overlays-in (point-min) (point-max)))))
    (djvu-return (mapcar (lambda (o)
                           (overlay-get o 'page))
                         im-overlays))))

(defun image-roll-visible-pages ()
  (interactive)
  (sit-for 0.001) ;when overlays are not yet 'filled' then overlays-in returns
                                        ;too many overlays
  (djvu-return (mapcar (lambda (o)
                         (overlay-get o 'page))
                       (overlays-in (window-start) (1- (window-end))))))

(defsubst image-roll-page-to-pos (page)
  (overlay-start (nth (1- page) (image-roll-overlays))))

(defun image-roll-new-window-function (winprops)
  "Function called first after displaying buffer in a new window.
If the buffer is newly created, then it does not contain any
overlay and this function erases the buffer contents, after which
it inserts empty spaces that each hold a overlay. If the buffer
already has overlays (i.e. a second or subsequent window is
created), the function simply copies the overlays and adds the
new window as window overlay-property to each overlay.

This function should be added to image-roll (continuous scroll)
minor mode commands, after erasing the buffer to create the
overlays."
  ;; (print "new win")
  (if (not (overlays-at 1))
      (let (overlays
            (win (car winprops))
            (inhibit-read-only t))

        (erase-buffer)

        ;; here we only add the 'page' and 'window' overlay-properties, we add
        ;; more properties/information as soon as it becomes available in the
        ;; 'image-roll-redisplay' function
        (dotimes (i (length image-roll-page-aspect-ratios))
          (let ((text (or (nth i image-roll-text-contents) " ")))
            (when (= (length text) 0)
              (setq text (make-string 24 (string-to-char " "))))
            (let ((o (make-overlay (prog1 (point) (insert text)) (point)))
                  (p (1+ i)))
              (overlay-put o 'page p)
              (overlay-put o 'window win)
              (push o overlays)
              (insert "\n"))))
        (when (bolp)
          (delete-char -1))
        (image-mode-window-put 'overlays (nreverse overlays))
        (set-buffer-modified-p nil))
    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (image-roll-overlays))))
      (image-mode-window-put 'overlays ols winprops))))

(defun image-roll-desired-page-sizes (&optional aspect-ratios fit-height columns h-margins v-margin)
  (mapcar
   (lambda (s)
     (let* ((win-width (float (window-text-width nil t)))
            (column-width (/ win-width (or columns 1)))
            (v-scale-factor (when fit-height
                              (/ (float (window-text-height nil t)) (cdr s))))
            (use-vertical-scaling (when v-scale-factor
                                    (< (* v-scale-factor (car s)) column-width)))
            (scaling-factor (if use-vertical-scaling
                                v-scale-factor
                              (/ (float column-width) (car s))))
            (target-width (- (* scaling-factor (car s))
                             (if use-vertical-scaling 0 (* 2 (or h-margins 0))))))
       (cons (round target-width)
             (round (- (* (/ target-width (car s)) ;we correct the ratio t.i.c. the h-margins
                          (if (proper-list-p s) (cadr s) (cdr s)))
                       (or v-margin 0))))))
   (or aspect-ratios image-roll-page-aspect-ratios)))

(defun image-roll-display-page (page)
  ;; (print "hier")
  (pcase-let* ((`(,w . ,h) (nth (1- page) image-roll-page-sizes))
               (svg (svg-create w h)))
    (svg-rectangle svg 0 0 w h :fill "white")
    (svg-text svg (number-to-string page)
              :font-size 40
              :fill "red"
              :x 5 :y 40)
    (overlay-put (nth (1- page) (image-roll-overlays))
                 'display (svg-image svg :margin '(1 . 1) :ascent 80))))

(defun image-roll-undisplay-page (page)
  ;; (print "of hier")
  (pcase-let* ((`(,w . ,h) (nth (1- page) image-roll-page-sizes)))
    (overlay-put (nth (1- page) (image-roll-overlays))
                 'display `(space . (:width (,w) :height (,h))))))

(defun image-roll-redisplay (&optional window force no-relative-vscroll)
  "Redisplay the scroll.
Besides that this function can be called directly, it should also
be added to the `window-configuration-change-hook'.

The argument WINDOW is not used in the body, but it exists to
make the function compatible with `pdf-tools' (in which case it
is a substitute for the `pdf-view-redisplay' function)."
  (print (format "REDISPLAY %s" (selected-window))  #'external-debugging-output)
  ;; (display-warning '(image-roll)
  ;;                  (format "redisplay %s" (car (image-mode-winprops)))
  ;;                  :debug "*image-roll-debug-log*")

  ;; NOTE the `(when (listp image-mode-winprops-alist)' from
  ;; `image-mode-reapply-winprops' was removed here (but in the end might turn
  ;; out to be required)

  ;; Beware: this call to image-mode-winprops can't be optimized away, because
  ;; it not only gets the winprops data but sets it up if needed (e.g. it's used
  ;; by doc-view to display the image in a new window).
  (image-mode-winprops nil t)

  (let* ((pages (length image-roll-page-aspect-ratios))
         (page-sizes (if (functionp image-roll-page-sizes)
                         (funcall image-roll-page-sizes)
                       image-roll-page-sizes))
                       ;; (make-list pages (if (functionp image-roll-demo-page-size)
                       ;;                      (funcall image-roll-demo-page-size)
                       ;;                    image-roll-demo-page-size))))
         (n 0))

    (dolist (page-size page-sizes)
      (let* ((page-width (car page-size))
             (overlay-heigth (+ (cdr page-size) (* 2 image-roll-vertical-margin)))
             (o (nth n (image-roll-overlays))))
        ;; (when image-roll-center
        ;;   (overlay-put o 'before-string
        ;;                (when (> (window-pixel-width) page-width)
        ;;                  (propertize " " 'display
        ;;                              `(space :align-to
        ;;                                      (,(floor (/ (- (window-pixel-width) page-width) 2))))))))
        (overlay-put o 'display `(space . (:width (,page-width) :height (,overlay-heigth))))
        (overlay-put o 'face `(:background ,image-roll-overlay-face-bg-color))
        (overlay-put o 'size page-size)
        (setq n (1+ n)))))
  ;; we only need to jump to the right page, the vscroll is conserved and if
  ;; required can be set to 0 before the redisplay

  ;; (when-let (p (image-roll-current-page))
  ;;   (goto-char (image-roll-page-to-pos p))
  (image-roll-goto-page 1)
    ;; (redisplay)
    ;; TODO implement in redisplay, `fractional vscroll' (in page units)
    (image-set-window-vscroll (or (image-mode-window-get 'vscroll)
                                  image-roll-vertical-margin)))

(defun image-roll-update ()
  (interactive)
  (let* ((visible (image-roll-visible-pages))
         (displayed (image-roll-displayed-images)))
    (dolist (p (cl-set-difference displayed visible))
      (image-roll-undisplay-page p))
    (dolist (p (cl-set-difference visible displayed))
      (image-roll-display-page p))))

(defun image-roll-goto-page (page &optional window)
  "Go to PAGE in document."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  (unless (and (>= page 1)
               (<= page (length image-roll-page-aspect-ratios)))
    (error "No such page: %d" page))
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
    ;; (let ((changing-p
    ;;        (not (eq page (image-roll-current-page)))))
      ;; (when changing-p
      ;;   (run-hooks 'image-roll-before-change-page-hook)
      ;;   (setf (image-roll-current-page window) page)
      ;;   (run-hooks 'image-roll-change-page-hook))
      (when (window-live-p window)
        (goto-char (image-roll-page-to-pos page))
        (image-roll-update)
        ;; (image-roll-redisplay)
        )
      ;; (when changing-p
      ;;   (run-hooks 'image-roll-after-change-page-hook))
      ;; (when changing-p
      ;;   (pdf-view-deactivate-region)
      ;;   (force-mode-line-update)
      ;;   (run-hooks 'pdf-view-after-change-page-hook))))
      nil))

(define-derived-mode image-roll-mode special-mode "Image Roll"
  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `image-roll-redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and add lines from
  ;; `image-mode-reapply-winprops' at the start of `image-roll-redisplay'.
  (add-hook 'window-configuration-change-hook 'image-roll-redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'image-roll-new-window-function nil t)
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil)))
;; (add-hook 'window-configuration-change-hook
;;           #'image-mode-reapply-winprops nil t))
;; (image-mode-setup-winprops))

(setq image-roll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'image-roll-scroll-forward)
        (define-key map (kbd "<down>") 'image-roll-scroll-forward)
        (define-key map (kbd "j") 'papyrus-cursor-next-line)
        (define-key map (kbd "k") 'papyrus-cursor-previous-line)
        (define-key map (kbd "l") 'papyrus-cursor-next-word)
        (define-key map (kbd "h") 'papyrus-cursor-previous-word)
        (define-key map (kbd "C-p") 'image-roll-scroll-backward)
        (define-key map (kbd "<up>") 'image-roll-scroll-backward)
        (define-key map (kbd "<mouse-5>") 'image-roll-scroll-forward)
        (define-key map (kbd "<mouse-4>") 'image-roll-scroll-backward)
        (define-key map "n" 'image-roll-next-page)
        (define-key map (kbd "<next>") 'image-roll-next-page)
        (define-key map "p" 'image-roll-previous-page)
        (define-key map (kbd "<prior>") 'image-roll-previous-page)
        (define-key map (kbd "S-<next>") 'image-roll-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'image-roll-scroll-screen-backward)
        (define-key map [remap goto-line] 'image-roll-goto-page)
        map))
