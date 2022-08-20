;; -*- lexical-binding: t; -*-

(require 'image-mode)
(require 'svg)
(require 'cl-lib)

(defun svg-group (&rest args)
  (apply #'dom-node
         'g
         `(,(svg--arguments nil args))))

(defcustom scrap-djvu-svg-embed t
  "Embed the images in svg."
  :type 'boolean
  :group 'scrap)

(defcustom scrap-horizontal-margin 2
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer
  :group 'scrap)

(defcustom scrap-vertical-margin 2
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer
  :group 'scrap)

(defcustom scrap-overlay-face-bg-color "gray"
  "Background color of overlay, i.e. page separation color."
  :type 'color
  :group 'scrap)

(defcustom scrap-step-size 50
  "Scroll step size in pixels units."
  :type '(choice function integer float))

(defcustom scrap-smooth-scrolling t
  "Visually slightly smoother page turn when t.
Also display next/previous pages outside window region.
Little slower on page jumps."
  :type 'boolean
  :group 'scrap)

(defcustom scrap-line-length 120
  "Number of characters per line.
The buffer can display only a number of columns that is equal to
a factor of this number."
  :type 'integer)

(defcustom scrap-thumbs-width 175
  "Width of thumbnails in pixels.
In order to limit memory usage, the maximum width is 'hard-coded'
to 400."
  :type 'integer
  :group 'scrap-thumbs)

(defcustom scrap-thumbs-side 'left
  "Create thumbs window at this side.
Side should be a symbol left or right (default)."
  :type 'symbol
  :group 'scrap-thumbs)

(defcustom scrap-thumbs-show-page-numbers nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'scrap-thumbs)

(defcustom scrap-thumbs-show-page-after nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'scrap-thumbs)

(defcustom scrap-thumbs-mouse-face nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'scrap-thumbs)

(defcustom scrap-thumbs-mouse-face-color "blue"
  "Maximum number of PNG images per buffer."
  :type 'color
  :group 'scrap-thumbs)

(defvar-local scrap-internal-page-sizes nil
  "List of page aspect ratios.
Each element is a cons of the form (width . height). Usually this
should just be a list of the document its intrinsic page sizes.")

(defvar-local scrap-last-page 0)
;; (defvar-local scrap-structured-contents nil)
(defvar-local scrap-image-type nil)
(defvar-local scrap-image-data-function nil)
(defvar-local scrap-thumbs-columns nil)
(defvar-local scrap-fit-height nil)


;;; window local value utils
(defun scrap-window-width (&optional window)
  "Number of columns.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'width ,val ,window))))
  (image-mode-window-get 'width window))

(defun scrap-columns (&optional window)
  "Number of columns.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'columns ,val ,window))))
  (image-mode-window-get 'columns window))

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
  "List of overlays that make up a scroll."
  (overlay-get (nth (1- page) (scrap-overlays)) prop))

(defun scrap-page-size (page)
  "List of overlays that make up a scroll.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(overlay-put (nth (1- page) (scrap-overlays)) 'size ,val))))
  (overlay-get (nth (1- page) (scrap-overlays)) 'size))


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

;; (define-derived-mode scrap-mode special-mode "Scrap"

;;   (setq cursor-type nil)

;;   ;; we don't use `(image-mode-setup-winprops)' because it would additionally
;;   ;; add `image-mode-reapply-winprops' to the
;;   ;; `window-configuration-change-hook', but `scrap-redisplay' already
;;   ;; reapplies the vscroll, so we simply initialize the
;;   ;; `image-mode-winprops-alist' here, and add lines from
;;   ;; `image-mode-reapply-winprops' at the start of `scrap-redisplay'.
;;   (add-hook 'window-configuration-change-hook 'scrap-redisplay nil t)
;;   (add-hook 'image-mode-new-window-functions 'scrap-new-window-function nil t)
;;   (setq image-mode-winprops-alist nil)

;;   (dolist (m '(global-hl-line-mode))
;;     (if (fboundp m)
;;         (funcall m 0))))

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

(when (featurep 'evil)
  (evil-define-key 'motion scrap-mode-map
        "j" 'scrap-scroll-forward
        "k" 'scrap-scroll-backward
        ;; (kbd "<down>") 'scrap-scroll-forward
        ;; (kbd "<up>") 'scrap-scroll-backward
        (kbd "<wheel-down>") 'scrap-scroll-forward
        (kbd "<wheel-up>") 'scrap-scroll-backward
        ;; (kbd "<mouse-5>") 'scrap-scroll-forward
        ;; (kbd "<mouse-4>") 'scrap-scroll-backward
        "J" 'scrap-next-page
        "K" 'scrap-previous-page
        (kbd "<next>") 'scrap-next-page
        (kbd "<prior>") 'scrap-previous-page
        (kbd "C-j") 'scrap-scroll-screen-forward
        (kbd "C-k") 'scrap-scroll-screen-backward
        (kbd "S-<next>") 'scrap-scroll-screen-forward
        (kbd "S-<prior>") 'scrap-scroll-screen-backward
        "G" 'scrap-goto-page
        "f" 'scrap-fit-toggle
        "c" 'scrap-set-columns
        "t" 'scrap-thumbs))

(define-minor-mode scrap-minor-mode
  "Scrap"
  :lighter "Scrap"
  :keymap scrap-mode-map
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
  ;; if new window then, (scrap-columns) calls `image-mode-winprops' which runs
  ;; the `image-mode-new-window-functions' and sets up (a) new winprops(-alist)
  (scrap-debug "REDISPLAY")

  ;; (unless (and (scrap-window-width)
  ;;              (= (scrap-window-width) (window-pixel-width)))
  (setf (scrap-window-width) (window-pixel-width))

  (let* ((columns (or (scrap-columns) 1))
         (page-sizes (scrap-desired-page-sizes
                      scrap-internal-page-sizes
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

        ;; NOTE we would like to store data in the overlay, but the following
        ;; functions seems not to remove that data in time
        ;; (overlay-put o 'data nil) 

        (overlay-put o 'display `(space . (:width (,page-width) :height (,overlay-heigth))))
        ;; (overlay-put o 'face `(:background ,scrap-overlay-face-bg-color))
        (overlay-put o 'face `(:background ,scrap-overlay-face-bg-color))
        (overlay-put o 'size page-size)
        (setq n (1+ n))))
    ;; (redisplay t) ;NOTE does not help for display issue
    (setf (scrap-columns) columns))
  ;; (sit-for 0.01) ;NOTE does not help for display issue
  (scrap-update t))

;; NOTE runs twice on first window creation, once for winprops of 'selected
;; window', and once more for winprops of t holding a collection of winprops of
;; all windows when the first winprops are added (i.e. triggered by the call to
;; `image-mode-window-put')
(defun scrap-new-window-function (winprops)
  (if (not (overlays-at 1))
      (let (overlays
            (pages scrap-last-page)
            (win (print (car winprops) #'external-debugging-output))
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
        (image-mode-window-put 'overlays (nreverse overlays) winprops)
        (set-buffer-modified-p nil))

;;       ;; required to make `pdf-view-redisplay-some-windows' call `pdf-view-redisplay'
;;       (when-let (fun scrap-set-redisplay-flag-function)
    ;;         (funcall fun)))
    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (scrap-overlays))))
      (image-mode-window-put 'overlays ols winprops)


      ;; cursor should be on overlay for `scrap-current-page'
      ;; (setf (scrap-current-page (car winprops))
      ;;       (or (scrap-current-page (car winprops)) 1)))
      (goto-char (point-min)))))

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
   (or aspect-ratios scrap-internal-page-sizes)))

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
                           scrap-internal-page-sizes nil columns
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
      (setq i (1+ i))))
  (setf (scrap-columns) columns)
  (scrap-update))

;; for some reason, despite the overlays already having their size,
;; `overlays-in' over the visible region returns lists all overlays in the
;; buffer when a new window is created (and possibly at other times also). So we
;; test for the condition and at a new-window flag/argument to 'circumvent' the
;; condition. TODO we should then pass the correct page to be displayed.
(defun scrap-update (&optional new-window)
  (interactive)
  (let* ((visible (print (scrap-visible-pages) #'external-debugging-output))
         (displayed (scrap-displayed-images)))
                                        ;NOTE the condition might only be relevant when new window (not sure)
    (cond ((= (length visible) scrap-last-page)
           (scrap-debug "number of visible overlays %s (all)" (length visible))
           (scrap-display-pages '(1 2)))
          (t
           (dolist (p (cl-set-difference displayed visible))
             (scrap-undisplay-page p))
           (let* ((pages (print (cl-set-difference visible displayed) #'external-debugging-output))
                  (non-exisiting-images (cl-remove-if (lambda (p)
                                                        (let* ((display-prop (scrap-overlay-get p 'display)))
                                                          (when (scrap-image-p display-prop)
                                                            (= (car (image-size display-prop)) w))))
                                                      pages)))
             (when non-exisiting-images
               (scrap-display-pages non-exisiting-images #'external-debugging-output)))))))


(defun scrap-visible-pages ()
  (interactive)
  (sit-for 0.001) ;when overlays are not yet 'filled', and their sizes are still
                  ;small, then overlays-in returns too many overlays
  (mapcar (lambda (o)
            (overlay-get o 'page))
          (delq nil (scrap-overlay-selected-window-filter
                     (overlays-in (max (- (window-start) (if scrap-smooth-scrolling scrap-line-length 0)) 1)
                                  (+ (window-end) (if scrap-smooth-scrolling scrap-line-length 0)))))))

(defun scrap-displayed-images ()
  (interactive)
  (let ((im-overlays (seq-filter (lambda (ov)
                                   (eq (car (overlay-get ov 'display))
                                       'image))
                                  (scrap-overlay-selected-window-filter
                                   (overlays-in (point-min) (point-max))))))
    (mapcar (lambda (o)
              (overlay-get o 'page))
            im-overlays)))

(defun scrap-undisplay-page (page)
  ;; (print "of hier")
  (pcase-let* ((`(,w . ,h) (scrap-page-size page)))
    (overlay-put (scrap-overlay page)
                 'display `(space . (:width (,w) :height (,h))))))

(defun scrap-display-pages (pages &optional force)
  ;; (print "RUNNING" #'external-debugging-output)
  (scrap-debug "display pages %s" pages)
  (pcase-let* ((size (scrap-page-size (car pages)))
               (`(,w . ,h) size))
      (dolist (page pages)
        (let* ((internal-size (nth (1- page) scrap-internal-page-sizes))
               (scale (/ (float w) (car internal-size)))
               (svg (svg-create w h))

               ;; NOTE we would like to store the data in the overlay, but the
               ;; overlay-put function in the `scrap-redisplay' function seems
               ;; to not delete the data quickly enough
               (data  ; (or (scrap-overlay-get page 'data) 
                (funcall scrap-image-data-function page w))
               (text-elements (when (eq major-mode 'papyrus-djvu-mode)
                                (djvu-text-elements 'char page)))
               (annots (when (eq major-mode 'papyrus-djvu-mode)
                         (djvu-annots page)))
               (annot-map (mapcar (lambda (a)
                                    (pcase (car a)
                                      ('background (message "Viewer background color should be %s (not (yet) implemented)"
                                                            (car annot)))
                                      ('zoom (message "Zoom value should be %s (not (yet) implemented)" (car annot)))
                                      ('mode (message "Mode value should be %s (not (yet) implemented)" (car annot)))
                                      ('align (message "Horizontal annot vertical align should be %s %s (not (yet) implemented)"
                                                       (nth 1 annot) (nth 2 annot)))

                                    ;; (message "%s %s %s %s" c internal-size 'djvu size))
                                      ('maparea (scrap-annot-to-hotspot a internal-size size))))
                                  annots))
               ;; (map (mapcar (lambda (c)
               ;;                ;; (message "%s %s %s %s" c internal-size 'djvu size))
               ;;                (scrap-text-component-to-hotspot c internal-size 'djvu size))
               ;;              text-elements))
               (image nil))
          (unless w (print "NO W" #'external-debugging-output))
          (when scrap-djvu-svg-embed
            (svg-embed svg data
                       (pcase scrap-image-type
                         ('png "image/png")
                         ('tiff "image/tiff")
                         ('pnm "image/x-portable-bitmap"))
                       t)
            (setq svg (append svg (list (scrap-annots-to-svg annots internal-size size)))))
                 ;; (when-let (rects (alist-get page papyrus-current-rectangles))
                 ;; (mapcar (lambda (c)
                 ;;           (apply #'svg-rectangle
                 ;;                  svg
                 ;;                  (append (mapcar (lambda (m)
                 ;;                                    (round (* scale m)))
                 ;;                                  (papyrus-coords-to-svg
                 ;;                                   (cdr (nth (1- page) scrap-internal-page-sizes))
                 ;;                                   (seq-subseq c 0 4)))
                 ;;                          (seq-subseq c 4))))
                 ;;         rects)
                 ;;   )
                 ;; (svg-rectangle svg 0 0 100 100 :fill "blue")
          (setq image (apply (if scrap-djvu-svg-embed
                                 (apply-partially #'svg-image svg)
                               (apply-partially #'create-image data scrap-image-type t))
                             (list :margin `(,scrap-horizontal-margin . ,scrap-vertical-margin)
                                   :pointer 'arrow
                                   :map annot-map)))
          ;; (when scrap-center
          ;;   (overlay-put o 'before-string
          ;;                (when (> (window-pixel-width) w)
          ;;                  (propertize " " 'display
          ;;                              `(space :align-to
          ;;                                      (,(floor (/ (- (window-pixel-width) w) 2))))))))
          (dolist (hs annot-map)
            (local-set-key
             (vector (nth 1 hs) 'mouse-1)
             (lambda (event)
               (interactive "@e")
               (let ((a (seq-find (lambda (i) (eq (posn-area (nth 1 event)) (nth 1 i)))
                                  annot-map)))
                 (scrap-goto-page (plist-get (nth 2 a) 'help-echo))))))
          (overlay-put (scrap-overlay page) 'data data)
          (overlay-put (scrap-overlay page) 'display image)))))

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
        (scrap-display-pages (list page))
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
    (cond ((> new-vscroll (cdr (scrap-current-size)))
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
                       (im (create-image (concat output-dir
                                                  (format "thumb%d.tif" p))
                                         'tiff
                                         nil
                                         :margin '(2 . 1))))
                  (apply #'insert-button (format "%03d " p)
                         (append (list 'page (number-to-string p)
                                       'win win
                                       'face 'default
                                       'display im
                                       'action (lambda (b)
                                                 (with-selected-window (button-get b 'win)
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

;;; Coords

(defun scrap-coords-normalize (coords size &optional type)
  (setq coords (mapcar #'float coords))
  (pcase-let* ((`(,w . ,h) size)
               (ratios (list (/ (nth 0 coords) w)
                             (/ (nth 1 coords) h)
                             (/ (nth 2 coords) w)
                             (/ (nth 3 coords) h)))
               (`(,x0 ,y0 ,x1 ,y1) ratios)
               (dx x1)
               (dy y1))
    (pcase type
      ('djvu (list x0 (- 1 y1) x1 (- 1 y0)))
      ('svg (list x0 y0 (+ x0 dx) (+ y0 dy)))
      ('djvu-annot (list x0 (- 1 (+ y0 dy)) dx (- 1 y0)))
      (_ ratios))))

(defun scrap-coords-denormalize (coords size &optional type)
  (pcase-let* ((`(,w . ,h) size)
               (`(,x0 ,y0 ,x1 ,y1) coords)
               (dx (- x1 x0))
               (dy (- y1 y0))
               (scale-factors (list w h w h)))
    (cl-mapcar #'*
               (pcase type
                 ('djvu (list x0 (- 1 y1) x1 (- 1 y0)))
                 ('svg (list x0 y0 dx dy))
                 (_ coords))
               scale-factors)))

(defun scrap-coords-convert (coords from-size from-type to-size &optional to-type)
  (mapcar #'round
          (scrap-coords-denormalize
           (scrap-coords-normalize coords from-size from-type)
           to-size to-type)))

;;; Hotspots

(defun scrap-text-coords-to-hotspot-rect (coords from-size from-type to-size)
  (pcase-let* ((`(,w . ,h) to-size)
               (`(,x0 ,y0 ,x1 ,y1) (cl-mapcar (lambda (c1 c2) (round (* c1 c2)))
                                              (scrap-coords-normalize coords from-size from-type)
                                              (list w h w h))))
    `(rect . ((,x0 . ,y0) . (,x1 . ,y1)))))

(defun scrap-hotspot-rect-to-text-coords (area from-size to-size to-type)
  (pcase-let* ((`(rect . ((,x0 . ,y0) . (,x1 . ,y1))) area))
    (scrap-coords-convert (list x0 y0 x1 y1) from-size 'hs to-size to-type)))

(defun scrap-text-component-to-hotspot (component from-size from-type to-size)
  (let ((coords (cl-subseq component 1 5)))
    (list (scrap-text-coords-to-hotspot-rect coords from-size from-type to-size)
          coords
          (list 'pointer 'text 'help-echo (nth 5 component)))))

(defun scrap-annot-to-hotspot (annot from-size to-size)
  (pcase-let ((`(,url ,comment ,area) (cdr annot)))
    (pcase (car area)
      ('rect (pcase-let* ((`(,x0 ,y0 ,x1 ,y1) (scrap-coords-convert
                                               (cdr area) from-size 'djvu-annot to-size)))
               (list `(rect . ((,x0 . ,y0) . (,x1 . ,y1)))

                     (intern (mapconcat 'number-to-string
                                        (mapcar 'truncate (list x0 y0 x1 y1)) "-"))
                     (append (list 'pointer 'hand)
                             (unless (string-blank-p url)
                               (list 'help-echo (string-to-number (substring url 1)))))))))))

;; (scrap-annot-to-hotspot '(maparea "#22" ""
;;                                   (rect 470 3878 1808 92)
;;                                   (opacity 50)
;;                                   (none))
;;                         '(100 . 100)
;;                         'djvu-annot
;;                         '(200 . 200))

(defun scrap-annots-to-svg (annots from-size to-size)
  (let ((svg-annots (svg-group :id 'annotations)))
    (dolist (a annots)
      (let* ((area (nth 3 a))
             (coords (cdr area)))
        (apply #'svg-rectangle
               svg-annots
               (append (scrap-coords-convert coords from-size 'djvu-annot to-size 'svg)
                       (list :fill "yellow"
                             :opacity 0.3)))))
    svg-annots))


;;; debug
(defun scrap-debug (format-string &rest args)
  (apply #'lwarn 'scrap :debug format-string args)
  (car args))

(provide 'scrap)
;;; scrap.el ends here
