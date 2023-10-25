;; -*- lexical-binding: t; -*-

;;; Comments

;; In the precursory package, image-roll.el, we calculated the visible overlays
;; via the point and vscroll positions. However, Emacs already provides the
;; function `overlays-in', to which we can simply pass the window-start and
;; window-end positions. Using this functionality greatly reduced the complexity
;; of the code in the current package.

;; In general we select functionality on major-mode, instead of file-type, as a
;; single file-type (e.g. pdf), can have multiple backends (i.e major-modes,
;; e.g., for pdf, pymupdf, mupdf, poppler etc.)

;; TODO maybe clean up some overlays at some point like in
;; `pdf-view-new-window-function'

(require 'image-mode)
(require 'svg)
(require 'cl-lib)

(defun svg-group (&optional id &rest args)
  (when id (setq args (append (list :id id) args)))
  (apply #'dom-node
         'g
         `(,(svg--arguments nil args))))

(defcustom doc-scroll-overlay-width 1184
  "The width of the cached images."
  :type 'numberp
  :group 'doc-scroll)

(defcustom doc-scroll-svg-embed t
  "Embed the images in svg."
  :type 'boolean
  :group 'doc-scroll)

(defcustom doc-scroll-horizontal-margin 1
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer
  :group 'doc-scroll)

(defcustom doc-scroll-vertical-margin 1
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer
  :group 'doc-scroll)

(defcustom doc-scroll-overlay-face-bg-color "gray"
  "Background color of overlay, i.e. page separation color."
  :type 'color
  :group 'doc-scroll)

(defcustom doc-scroll-step-size 50
  "Scroll step size in pixels units."
  :type '(choice function integer float))

(defcustom doc-scroll-smooth-scrolling t
  "Visually slightly smoother page turn when t.
Also display next/previous pages outside window region.
Little slower on page jumps."
  :type 'boolean
  :group 'doc-scroll)

(defcustom doc-scroll-line-length 120
  "Number of characters per line.
The buffer can display only a number of columns that is equal to
a factor of this number."
  :type 'integer)

(defcustom doc-scroll-after-change-page-hook nil
  "Hook run after changing to and displaying another page.

See also `doc-scroll-change-page-hook' and
`doc-scroll-before-change-page-hook'."
  :type 'hook)

(defcustom doc-scroll-thumbs-width 175
  "Width of thumbnails in pixels.
In order to limit memory usage, the maximum width is 'hard-coded'
to 400."
  :type 'integer
  :group 'doc-scroll-thumbs)

(defcustom doc-scroll-thumbs-side 'left
  "Create thumbs window at this side.
Side should be a symbol left or right (default)."
  :type 'symbol
  :group 'doc-scroll-thumbs)

(defcustom doc-scroll-thumbs-show-page-numbers nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'doc-scroll-thumbs)

(defcustom doc-scroll-thumbs-show-page-after nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'doc-scroll-thumbs)

(defcustom doc-scroll-thumbs-mouse-face nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'doc-scroll-thumbs)

(defcustom doc-scroll-thumbs-mouse-face-color "blue"
  "Maximum number of PNG images per buffer."
  :type 'color
  :group 'doc-scroll-thumbs)

(defvar-local doc-scroll-internal-page-sizes nil
  "List of page aspect ratios.
Each element is a cons of the form (width . height). Usually this
should just be a list of the document its intrinsic page sizes.")

(defvar-local doc-scroll-last-page 0)
(defvar-local doc-scroll-contents nil)
(defvar-local doc-scroll-structured-text nil)
(defvar-local doc-scroll-image-type nil)
(defvar-local doc-scroll-image-data-function nil)
(defvar-local doc-scroll-thumbs-columns nil)
(defvar-local doc-scroll-fit-height nil)
(defvar-local doc-scroll-imenu-index nil)

(defvar-local doc-scroll-annots nil
  "Container for annots.
Its structure is a list with elements of the structure
(PAGE . (MODIFIED . PAGE-ANNOTS)).

This variable is only used when there is no server that keeps
track of the state of the document.")

(defvar-local doc-scroll-active-region nil)

(defvar-local doc-scroll-info-function nil)

(defun doc-scroll-set-pdf-mode (mode)
  (interactive (list
                (intern
                 (completing-read "Select mode: "
                                  '(pdf-view-mode
                                    doc-scroll-mupdf-mode
                                    doc-backend-pymupdf-mode
                                    doc-view-mode-maybe)))))

  ;; remove pdf-view-mode from magic-mode alist
  (if-let (e (rassoc 'pdf-view-mode magic-mode-alist))
      (setq magic-mode-alist (remove e magic-mode-alist)))

  (push (cons "\\.pdf\\'" mode) auto-mode-alist))

(defun doc-scroll-set-djvu-mode (mode)
  (interactive (list
                (intern
                 (completing-read "Select mode: "
                                  '(doc-backend-djvu-mode
                                    djvu-init-mode
                                    doc-view-mode-maybe)))))
  (push (cons "\\.djvu\\'" mode) auto-mode-alist))

;;; window local value utils
(defun doc-scroll-window-width (&optional winprops)
  "Number of columns.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'width ,val ,winprops))))
  (image-mode-window-get 'width winprops))

(defun doc-scroll-columns (&optional winprops)
  "Number of columns.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'columns ,val ,winprops))))
  (image-mode-window-get 'columns winprops))

(defun doc-scroll-overlays (&optional winprops)
  "List of overlays that make up a scroll.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'overlays ,val ,winprops))))
  (image-mode-window-get 'overlays winprops))

(defun doc-scroll-overlay (&optional page winprops)
  "Return the overlay that hold page number PAGE.
Implemented as macro to make it setf'able.
TODO update docstring (no macro anymore)"
  (nth (1- (or page (doc-scroll-page-at-point)))
       (doc-scroll-overlays winprops)))

(defun doc-scroll-overlay-get (page prop)
  "List of overlays that make up a scroll."
  (overlay-get (nth (1- page) (doc-scroll-overlays)) prop))


(defun doc-scroll-swiper-matches (page &optional winprops)
  "List of overlays that make up a scroll.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(overlay-put (nth (1- ,page) (doc-scroll-overlays ,winprops)) 'swiper-matches ,val))))
  (overlay-get (nth (1- page) (doc-scroll-overlays winprops)) 'swiper-matches))

;;; utils
(defvar-local doc-scroll-info-commands '(doc-scroll-cache-folder-size))

(defun doc-scroll-info (function &optional arg)
  "Return size of file its cache folder.
Folder contains thumb and page images."
  (interactive (list (intern-soft (completing-read "Select info type: "
                                                   (append doc-scroll-info-commands
                                                           (list doc-scroll-info-function))))
                     current-prefix-arg))
  (if (eq function doc-scroll-info-function)
      (call-interactively function)
    (pp (call-interactively function))))

(defun doc-scroll-image-p (object)
  (eq (car object) 'image))

(defun doc-scroll-overlay-size (page)
  "List of overlays that make up a scroll."
  (overlay-get (nth (1- page) (doc-scroll-overlays)) 'size))

(defun doc-scroll-internal-size (page)
  (nth (1- page) doc-scroll-internal-page-sizes))

(defun doc-scroll-overlay-selected-window-filter (overlays)
  (cl-remove-if-not
   (lambda (overlay)
     (eq (overlay-get overlay 'window) (selected-window)))
   overlays))

(defun doc-scroll-page-at-point (&optional window)
  (interactive)
  (overlay-get (car (overlays-at (point))) 'page))

(defun doc-scroll-current-page (&optional winprops)
  "List of overlays that make up a scroll.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'page ,val ,winprops))))
  (image-mode-window-get 'page winprops))

(defun doc-scroll-current-size ()
  (interactive)
  (overlay-get (car (doc-scroll-overlay-selected-window-filter
                     (overlays-at (point))))
               'size))

(defun doc-scroll-page-pos (page &optional window)
  (overlay-start (nth (1- page) (doc-scroll-overlays window))))

(defun doc-scroll-vscroll-to-fscroll (vscroll)
  (/ (float vscroll)
     (cdr (doc-scroll-overlay-size (or (doc-scroll-current-page) 1)))))

(defun doc-scroll-set-window-fscroll (vscroll)
  "Set vscroll in units of current page height."
  (let ((fscroll (doc-scroll-vscroll-to-fscroll vscroll)))
    (setf (image-mode-window-get 'fscroll) fscroll)
    (set-window-vscroll (selected-window) vscroll t)))

(defun doc-scroll-fscroll-to-vscroll (fscroll &optional page)
  (* fscroll
     (cdr (doc-scroll-overlay-size (if page
                                       (cdr (doc-scroll-overlay-size page))
                                     (doc-scroll-current-page))))))

(defun doc-scroll-cache-folder-size (&optional _)
  (interactive "P")
  (car (split-string (shell-command-to-string
                      (format "du -sh '%s'"
                              (print (concat "/tmp/doc-tools/"
                                             (file-name-as-directory
                                              (file-name-base (buffer-file-name))))))))))

;; (define-derived-mode doc-scroll-mode special-mode "DS"

;;   (setq cursor-type nil)

;;   ;; we don't use `(image-mode-setup-winprops)' because it would additionally
;;   ;; add `image-mode-reapply-winprops' to the
;;   ;; `window-configuration-change-hook', but `doc-scroll-redisplay' already
;;   ;; reapplies the vscroll, so we simply initialize the
;;   ;; `image-mode-winprops-alist' here, and add lines from
;;   ;; `image-mode-reapply-winprops' at the start of `doc-scroll-redisplay'.

;;   (add-hook 'window-configuration-change-hook 'doc-scroll-redisplay nil t)
;;   (add-hook 'image-mode-new-window-functions 'doc-scroll-new-window-function nil t)
;;   (setq image-mode-winprops-alist nil)

;;   (dolist (m '(global-hl-line-mode))
;;     (if (fboundp m)
;;         (funcall m 0))))

(setq doc-scroll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'doc-scroll-scroll-forward)
        (define-key map (kbd "<down>") 'doc-scroll-scroll-forward)
        (define-key map (kbd "C-p") 'doc-scroll-scroll-backward)
        (define-key map (kbd "<up>") 'doc-scroll-scroll-backward)
        (define-key map (kbd "<wheel-down>") 'doc-scroll-scroll-forward)
        (define-key map (kbd "<wheel-up>") 'doc-scroll-scroll-backward)
        ;; (define-key map (kbd "<mouse-5>") 'doc-scroll-scroll-forward)
        ;; (define-key map (kbd "<mouse-4>") 'doc-scroll-scroll-backward)
        (define-key map "n" 'doc-scroll-next-page)
        (define-key map (kbd "<next>") 'doc-scroll-next-page)
        (define-key map "p" 'doc-scroll-previous-page)
        (define-key map (kbd "<prior>") 'doc-scroll-previous-page)
        (define-key map (kbd "S-<next>") 'doc-scroll-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'doc-scroll-scroll-screen-backward)
        (define-key map [remap goto-line] 'doc-scroll-goto-page)
        (define-key map "f" 'doc-scroll-fit-toggle)
        (define-key map "c" 'doc-scroll-set-columns)
        (define-key map "t" 'doc-scroll-thumbs)
        (define-key map "T" 'doc-scroll-page-text)
        (define-key map "i" 'doc-scroll-info)
        (define-key map "y" 'doc-scroll-kill-new)
        (define-key map (kbd "C-s") 'doc-scroll-search)
        ;; (define-key map [down-mouse-1] 'doc-scroll-select-region)
        ;; (define-key map [S-down-mouse-1] 'doc-scroll-select-region-free)
        map))

(defun doc-scroll-select-region-free (event)
  (interactive "@e")
  (doc-scroll-select-region event t))

(defun doc-scroll-select-region (event &optional free)
  "Draw objects interactively via a mouse drag EVENT. "
  (interactive "@e")
  (let* ((start (event-start event))
         ;; (page (print (posn-area start)))
         (page (image-property (posn-image start) :page))
         (start-point (doc-scroll-coords-point-scale page (posn-object-x-y start)))
         (text (funcall (pcase major-mode
                          ('doc-backend-djvu-mode #'doc-backend-djvu-structured-text)
                          ('doc-backend-pymupdf-mode #'doc-backend-pymupdf-structured-text))
                        page))
         first-element
         result)
    ;; NOTE we either must pass page to the 'doc-backend-djvu-get-regions' and pdf
    ;; version functions, or we should correct the coords for djvu here, we
    ;; choose the second option. DON'T SET THIS IN THE TRACK MOUSE WHILE LOOP
    (when (eq major-mode 'doc-backend-djvu-mode)
      (setq start-point (cons (car start-point)
                              (- (cdr (doc-scroll-internal-size page)) (cdr start-point)))))
    (track-mouse
      (while (not (memq (car event) '(drag-mouse-1 S-drag-mouse-1)))
        (setq event (read-event))
        (let* ((end (event-end event))
               (end-point (doc-scroll-coords-point-scale page (posn-object-x-y end))))
          (when (eq major-mode 'doc-backend-djvu-mode)
            ;; NOTE see note above 'track-mouse' about correcting coords
            (setq end-point (cons (car end-point)
                                  (- (cdr (doc-scroll-internal-size page)) (cdr end-point)))))
          ;; (doc-scroll-debug "%s %s" start-point end-point)
          ;; (doc-scroll-debug "%s"
          ;; (print "hoi")
          (setq doc-scroll-active-region
                (cons page
                      (if free
                          (pcase-let ((`(,x0 . ,y0) start-point)
                                      (`(,x1 . ,y1) end-point))
                            (list (list (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1))))
                        (funcall (pcase major-mode
                                   ('doc-backend-pymupdf-mode #'doc-backend-pymupdf-word-at-point)
                                   ('doc-backend-djvu-mode #'doc-backend-djvu-get-regions))
                                 text start-point end-point))))
          (doc-scroll-display-page page))))))

(defun doc-scroll-kill-new ()
  (interactive)
  (kill-new (doc-scroll-active-region-text))
  (setq doc-scroll-active-region nil)
  (doc-scroll-update t))

(defun doc-scroll-active-region-text ()
  (mapconcat (lambda (e)
               (nth 4 e))
             (cdr doc-scroll-active-region)
             "\n"))

(defun doc-scroll-active-regions (regions)
  (mapcar (lambda (r) (if (nthcdr 4 r) (butlast r) r)) regions))

(defun doc-scroll-page-text (&optional arg)
  (interactive "P")
  (pp (doc-djvu-structured-text 'plain (if arg
                                           (read-number "Page: ")
                                         (doc-scroll-page-at-point)))
      (pop-to-buffer (get-buffer-create "*djvu-text*"))))


                                        ;numbers
;; (let* ((start (event-start event)))
;;   (print start)))

(when (featurep 'evil)
  (evil-define-key 'motion doc-scroll-mode-map
    "j" 'doc-scroll-scroll-forward
    "k" 'doc-scroll-scroll-backward
    (kbd "<down>") 'doc-scroll-scroll-forward
    (kbd "<up>") 'doc-scroll-scroll-backward
    (kbd "<wheel-down>") 'doc-scroll-scroll-forward
    (kbd "<wheel-up>") 'doc-scroll-scroll-backward
    ;; (kbd "<mouse-5>") 'doc-scroll-scroll-forward
    ;; (kbd "<mouse-4>") 'doc-scroll-scroll-backward
    "J" 'doc-scroll-next-page
    "K" 'doc-scroll-previous-page
    (kbd "<next>") 'doc-scroll-next-page
    (kbd "<prior>") 'doc-scroll-previous-page
    (kbd "C-j") 'doc-scroll-scroll-screen-forward
    (kbd "C-k") 'doc-scroll-scroll-screen-backward
    (kbd "S-<next>") 'doc-scroll-scroll-screen-forward
    (kbd "S-<prior>") 'doc-scroll-scroll-screen-backward
    "G" 'doc-scroll-goto-page
    "f" 'doc-scroll-fit-toggle
    "c" 'doc-scroll-set-columns
    "t" 'doc-scroll-thumbs
    "T" 'doc-scroll-page-text
    "/" 'doc-scroll-search
    "n" 'doc-scroll-search-next
    "i" 'doc-scroll-info
    "y" 'doc-scroll-kill-new
    "o" 'imenu-list-smart-toggle
    [down-mouse-1] 'doc-scroll-select-region))

(when (featurep 'imenu-list)
  (evil-define-key 'motion imenu-list-major-mode-map
    "o" 'imenu-list-smart-toggle))

(define-minor-mode doc-scroll-minor-mode
  "DS"
  :lighter "DS"
  :keymap doc-scroll-mode-map
  (setq cursor-type nil)
  (blink-cursor-mode -1)

  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `doc-scroll-redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and add lines from
  ;; `image-mode-reapply-winprops' at the start of `doc-scroll-redisplay'.
  (add-hook 'window-configuration-change-hook 'doc-scroll-redisplay-all nil t)
  (add-hook 'image-mode-new-window-functions 'doc-scroll-new-window-function nil t)
  (setq image-mode-winprops-alist nil)

  (dolist (m '(global-hl-line-mode))
    (if (fboundp m)
        (funcall m 0)))

  ;; (setq-local mode-line-format
  (setq-local mode-line-position
              `(" P" (:eval (number-to-string (doc-scroll-page-at-point)))
                ;; Avoid errors during redisplay.
                "/" (:eval (number-to-string doc-scroll-last-page))))

  (when (featurep 'imenu-list)
    (setq-local imenu-list-focus-after-activation t)))

(defun doc-scroll-redisplay-all ()
  (dolist (win (get-buffer-window-list))
    (doc-scroll-redisplay nil (doc-scroll-debug "%s" win))))

(defun doc-scroll-redisplay (&optional force window)
  ;; if new window then, (doc-scroll-columns) calls `image-mode-winprops' which runs
  ;; the `image-mode-new-window-functions' and sets up (a) new winprops(-alist)
  (doc-scroll-debug "REDISPLAY")

  (when (or (not (and (doc-scroll-window-width)
                      (= (doc-scroll-window-width) (window-pixel-width))))
            force)
    (setf (doc-scroll-window-width) (window-pixel-width))

    (let* ((columns (or (doc-scroll-columns) 1))
           ;; (overlay-sizes (doc-scroll-desired-page-sizes
           ;;              doc-scroll-internal-page-sizes
           ;;              nil
           ;;              columns
           ;;              doc-scroll-horizontal-margin
           ;;              doc-scroll-vertical-margin))
           (overlay-sizes (doc-scroll-desired-overlay-sizes
                           doc-scroll-internal-page-sizes
                           columns))
           ;; (make-list pages (if (functionp doc-scroll-demo-page-size)
           ;;                      (funcall doc-scroll-demo-page-size)
           ;;                    doc-scroll-demo-page-size))))
           (n 0))

      (dolist (overlay-size overlay-sizes)
        (let* ((page-width (+ (car overlay-size) (* 2 doc-scroll-horizontal-margin)))
               (overlay-heigth (+ (cdr overlay-size) (* 2 doc-scroll-vertical-margin)))
               (o (nth n (doc-scroll-overlays))))
          ;; (when doc-scroll-center
          ;;   (overlay-put o 'before-string
          ;;                (when (> (window-pixel-width) page-width)
          ;;                  (propertize " " 'display
          ;;                              `(space :align-to
          ;;                                      (,(floor (/ (- (window-pixel-width) page-width) 2))))))))

          ;; NOTE we would like to store data in the overlay, but the following
          ;; functions seems not to remove that data in time
          ;; (overlay-put o 'data nil)
          (overlay-put o 'face `(:background ,doc-scroll-overlay-face-bg-color))
          (when (or (not (doc-scroll-columns)) (= (doc-scroll-columns) 1))
            (overlay-put o 'before-string
                         (when (> (window-pixel-width) (car overlay-size))
                           (propertize " " 'display
                                       `(space :align-to
                                               (,(floor (/ (- (window-pixel-width) (car overlay-size)) 2))))))))
          (overlay-put o 'display `(space . (:width (,page-width) :height (,overlay-heigth))))
          ;; (overlay-put o 'face `(:background ,doc-scroll-overlay-face-bg-color))
          (overlay-put o 'size overlay-size)
          (setq n (1+ n))))
      ;; (redisplay t) ;NOTE does not help for display issue
      (setf (doc-scroll-columns) columns))
    ;; (sit-for 0.01) ;NOTE does not help for display issue
    (doc-scroll-goto-pos (or (doc-scroll-current-page) 1)
                         (or (image-mode-window-get 'fscroll) 0))
    (doc-scroll-update)))
;; (run-with-timer 0.01 nil #'doc-scroll-update)))

;; NOTE this function is called via `doc-scroll-columns' in `doc-scroll-redisplay'. The
;; function runs twice on first window creation, once for winprops of 'selected
;; window', and once more for winprops of t holding a collection of winprops of
;; all windows when the first winprops are added (i.e. triggered by the call to
;; `image-mode-window-put')
(defun doc-scroll-new-window-function (winprops)
  (doc-scroll-debug "NEW_WINDOW")
  (if (not (overlays-at 1))
      (let (overlays
            (pages doc-scroll-last-page)
            (win (car winprops))
            (inhibit-read-only t))

        (erase-buffer)

        ;; here we only add the 'page' and 'window' overlay-properties, we add
        ;; more properties/information as soon as it becomes available in the
        ;; 'doc-scroll-redisplay' function
        (dotimes (i pages)
          (let ((o (make-overlay
                    ;; (prog1 (point) (insert (make-string doc-scroll-line-length (string-to-char " "))))
                    ;; (point))))
                    (point)
                    (progn (insert (make-string doc-scroll-line-length (string-to-char " ")))
                      (point)))))
            (insert "\n")
            (overlay-put o 'page  (1+ i))
            (overlay-put o 'window win)
            (push o overlays)))
        (delete-char -1)

        ;; this function also calls `doc-scroll-new-window-function'
        (image-mode-window-put 'overlays (nreverse overlays) winprops)
        (set-buffer-modified-p nil)

        ;; need to place point at some overlay, before continuing 'filling the
        ;; overlays' in the `doc-scroll-redisplay' function
        ;; NOTE, when implementing some 'restore' functionality, then jump to
        ;; the correct 'point' here (the overlays, i.e. `doc-scroll-page-pos', are
        ;; not yet available).
        ;; (setf (doc-scroll-current-page) (doc-scroll-))
        (goto-char (point-min))
        ;; (goto-char (doc-scroll-page-pos (or (doc-scroll-current-page) 1)))
        )

    ;; ;; required to make `pdf-view-redisplay-some-windows' call `pdf-view-redisplay'
    ;; (when-let (fun doc-scroll-set-redisplay-flag-function)
    ;;   (funcall fun)))
    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (doc-scroll-overlays)))
          (pos (image-mode-window-get 'page)))
      (image-mode-window-put 'overlays ols winprops)


      ;; cursor should be on overlay for `doc-scroll-page-at-point'
      ;; (setf (doc-scroll-page-at-point (car winprops))
      ;;       (or (doc-scroll-page-at-point (car winprops)) 1))))))
      ;; (goto-char (doc-scroll-page-pos (or (doc-scroll-current-page) 1)))
      )))

(defun test (page)
  (goto-char (doc-scroll-page-pos page))
  (doc-scroll-debug "HIER %s" (image-mode-window-get 'overlays)))

(defun doc-scroll-desired-page-sizes (&optional aspect-ratios fit-height columns h-margin v-margin)
  (mapcar (lambda (s)
            (let* ((win-width (float (window-pixel-width)))
                   (column-width (min (/ win-width (or columns 1)) doc-scroll-overlay-width))
                   (y-scale-factor (when (or fit-height doc-scroll-fit-height)
                                     (/ (float (window-text-height nil t)) (cdr s))))
                   (use-vertical-scaling (when y-scale-factor
                                           (< (* y-scale-factor (car s)) column-width)))
                   (scaling-factor (if use-vertical-scaling
                                       y-scale-factor
                                     (/ (float column-width) (car s))))
                   (target-width (- (* scaling-factor (car s))
                                    (if use-vertical-scaling 0 (* 2 (or h-margin 0))))))
              (cons (floor target-width)
                    (floor (- (* (/ target-width (car s)) ;we correct the ratio t.i.c. the h-margins
                                 (if (proper-list-p s) (cadr s) (cdr s)))
                              (* 2 (or v-margin 0)))))))
          (or aspect-ratios doc-scroll-internal-page-sizes)))

(defun doc-scroll-desired-overlay-sizes (&optional aspect-ratios columns)
  (mapcar (lambda (s)
            (let* ((win-width (float (window-body-width nil t)))
                   (column-width (min (/ win-width (or columns 1)) doc-scroll-overlay-width))
                   (y-scale-factor (when doc-scroll-fit-height
                                     (/ (float (window-text-height nil t)) (cdr s))))
                   ;; only use the vertical scaling if otherwise page height is
                   ;; larger then window height (i.e. if column-width >
                   ;; fit-height page width)
                   (use-vertical-scaling (when y-scale-factor
                                           (< (* y-scale-factor (car s)) column-width)))
                   (scaling-factor (if use-vertical-scaling
                                       y-scale-factor
                                     (/ (float column-width) (car s))))
                   (target-width (* scaling-factor (car s))))
              (cons (floor target-width)
                    (floor (* scaling-factor
                              (if (proper-list-p s) (cadr s) (cdr s)))))))
          (or aspect-ratios doc-scroll-internal-page-sizes)))

(defun doc-scroll-fit-toggle ()
  (interactive)
  (setq doc-scroll-fit-height (not doc-scroll-fit-height))
  (doc-scroll-redisplay t))


;; Instead of refilling the buffer, we simply shift the overlays. In this way,
;; the same buffer can be displayed with a different number of columns in
;; different windows
(defun doc-scroll-set-columns (columns)
  (interactive "p")
  (when (/= (% doc-scroll-line-length columns) 0)
    (user-error
     "The current 'scap-line-length' only supports %s number of
columns"
     (mapconcat #'number-to-string
                (cl-loop for f from 1 to doc-scroll-line-length
                         when (eq (% doc-scroll-line-length f) 0)
                         collect f)
                ",")))
  (let ((current-page (doc-scroll-page-at-point))
        ;; (doc-scroll-overlay-sizes (doc-scroll-desired-page-sizes
        ;;                    doc-scroll-internal-page-sizes nil columns
        ;;                    doc-scroll-horizontal-margin doc-scroll-vertical-margin))
        (doc-scroll-overlay-sizes (doc-scroll-desired-overlay-sizes
                                   doc-scroll-internal-page-sizes columns))
        (pos 1)
        (i 0))
    (dolist (o (doc-scroll-overlays))
      (let ((s (nth i doc-scroll-overlay-sizes)))
        (move-overlay o pos (setq pos (+ pos (/ doc-scroll-line-length columns))))
        (overlay-put o 'size s)
        (overlay-put o 'before-string nil)
        (overlay-put o 'display `(space . (:width (,(car s)) :height (,(cdr s))))))
      (when (eq (% i columns) (1- columns))
        (setq pos (1+ pos)))
      (setq i (1+ i)))
    (setf (doc-scroll-columns) columns)
    (doc-scroll-goto-page current-page)))

;; for some reason, despite the overlays already having their size,
;; `overlays-in' over the visible region returns all overlays in the
;; buffer when a new window is created (and possibly at other times also). So we
;; test for the condition and at a new-window flag/argument to 'circumvent' the
;; condition. TODO we should then pass the correct page to be displayed.
(defun doc-scroll-update (&optional current)
  (interactive)
  (let* ((visible (doc-scroll-debug "%s" (doc-scroll-visible-pages)))
         (displayed (doc-scroll-displayed-images))
                                        ;NOTE the condition might only be relevant when new window (not sure)
         (n-visible (length visible)))
    (cond ((or (= n-visible doc-scroll-last-page)
               (= n-visible 0))
           (doc-scroll-debug "number of visible overlays %s (all)" (length visible))
           (doc-scroll-display-pages '(1 2)))
          (t
           (dolist (p (cl-set-difference displayed visible))
             (doc-scroll-undisplay-page p))
           (let* ((pages (cl-set-difference visible displayed))
                  (non-exisiting-images (cl-remove-if (lambda (p)
                                                        (let* ((display-prop (doc-scroll-overlay-get p 'display)))
                                                          (when (doc-scroll-image-p display-prop)
                                                            (= (car (image-size display-prop)) w))))
                                                      pages)))
             (if current
                 (doc-scroll-display-pages displayed)
               (when (doc-scroll-debug "%s" non-exisiting-images)
                 (doc-scroll-display-pages non-exisiting-images))))))))


(defun doc-scroll-visible-pages ()
  (interactive)
  (sit-for 0.001) ;when overlays are not yet 'filled', and their sizes are still
                                        ;small, then overlays-in returns too many overlays
  (mapcar (lambda (o)
            (overlay-get o 'page))
          (delq nil (doc-scroll-overlay-selected-window-filter
                     (overlays-in (max (- (window-start) (if doc-scroll-smooth-scrolling doc-scroll-line-length 0)) 1)
                                  (+ (window-end) (if doc-scroll-smooth-scrolling doc-scroll-line-length 0)))))))

(defun doc-scroll-displayed-images ()
  (interactive)
  (let ((im-overlays (seq-filter (lambda (ov)
                                   (eq (car (overlay-get ov 'display))
                                       'image))
                                 (doc-scroll-overlay-selected-window-filter
                                  (overlays-in (point-min) (point-max))))))
    (mapcar (lambda (o)
              (overlay-get o 'page))
            im-overlays)))

(defun doc-scroll-undisplay-page (page)
  ;; (print "of hier")
  (pcase-let* ((`(,w . ,h) (doc-scroll-overlay-size page)))
    (overlay-put (doc-scroll-overlay page) 'data nil) ;; TODO improve/implement
    ;; caching algorithm
    (overlay-put (doc-scroll-overlay page)
                 'display `(space . (:width (,w) :height (,h))))))

(defvar doc-scroll-process nil)

;; NOTE pdf-tools style async (see next for more logical async version)
;; (defun doc-scroll-display-pages-async (pages &optional force)
;;   ;; (print "RUNNING" #'external-debugging-output)
;;   (pcase-let* ((size (doc-scroll-overlay-size (car pages)))
;;                (`(,w . ,h) size)
;;                ;; (scale (/ (float w) (car (nth (1- (car pages)) doc-scroll-internal-page-sizes))))
;;                ;; (svg (svg-create w h))
;;                ;; (data nil))
;;                )
;;     (unless doc-scroll-process
;;       (setq doc-scroll-process
;;             ;; (start-process "ddjvu" "imdata" "ddjvu"
;;             ;;                        (format "-size=%dx%d" w 5000)
;;             ;;                        "-format=tiff"
;;             ;;                        "-quality=50"
;;             ;;                        (format "-pages=%d" (car pages))
;;             ;;                        (buffer-file-name)
;;             ;;                        "/tmp/doc-scroll-image")
;;             (start-process "mutool draw" "imdata" "mutool"
;;                            "draw"
;;                            "-o" "/tmp/doc-scroll-image"
;;                            "-F" "png"
;;                            "-w" (number-to-string w)
;;                            (buffer-file-name)
;;                            (number-to-string (car pages)))
;;             )
;;       (set-process-sentinel doc-scroll-process

;;                             (lambda (p e)
;;                               (doc-scroll-display-page (car pages)
;;                                                   (with-temp-buffer
;;                                                     (set-buffer-multibyte nil)
;;                                                     (insert-file-contents-literally "/tmp/doc-scroll-image")
;;                                                     (buffer-string))
;;                                                   size)
;;                               (setq doc-scroll-process nil)
;;                               (setq pages (cdr pages))
;;                               (when pages
;;                                 (doc-scroll-display-pages pages)))))))

;; NOTE More logical async version (probably)
;; (defun doc-scroll-display-pages (pages &optional force)
;;   ;; (print "RUNNING" #'external-debugging-output)
;;   (dolist (p pages)
;;     (let* ((size (doc-scroll-overlay-size p))
;;            (width (car size))
;;                ;; (scale (/ (float w) (car (nth (1- p) doc-scroll-internal-page-sizes))))
;;                ;; (svg (svg-create w h))
;;                ;; (data nil))
;;                )
;;     (unless doc-scroll-process
;;       (setq doc-scroll-process
;;             (pcase major-mode
;;               ('doc-scroll-doc-djvu-mode (start-process "ddjvu" "imdata" "ddjvu"
;;                                                  (format "-size=%dx%d" width 5000)
;;                                                  "-format=tiff"
;;                                                  "-quality=50"
;;                                                  (format "-pages=%d" p)
;;                                                  (buffer-file-name)
;;                                                  "/tmp/doc-scroll-image"))
;;               ('doc-scroll-mupdf-mode (start-process "mutool draw" "imdata" "mutool"
;;                                                   "draw"
;;                                                   "-o" "/tmp/doc-scroll-image"
;;                                                   "-F" "png"
;;                                                   "-w" (number-to-string width)
;;                                                   (buffer-file-name)
;;                                                   (number-to-string p)))))
;;       (set-process-sentinel doc-scroll-process

;;                             (lambda (_ _)
;;                               (doc-scroll-display-page p
;;                                                   (with-temp-buffer
;;                                                     (set-buffer-multibyte nil)
;;                                                     (insert-file-contents-literally "/tmp/doc-scroll-image")
;;                                                     (buffer-string))
;;                                                   size)
;;                               (setq doc-scroll-process nil)
;;                               (setq pages (cdr pages))
;;                               (when pages
;;                                 ;; (run-with-timer 0.01 nil
;;                                 ;;                 #'doc-scroll-display-pages pages))))))))
;;                                  (doc-scroll-display-pages pages))))))))

(defun doc-scroll-display-pages (pages &optional force)
  ;; (print "RUNNING" #'external-debugging-output)
  (doc-scroll-debug "display pages %s" pages)
  (dolist (page pages)
    ;; (let ((scale (/ (float w) (car internal-size)))

    ;; NOTE we would like to store the data in the overlay, but the
    ;; overlay-put function in the `doc-scroll-redisplay' function seems
    ;; not to delete the data quickly enough

    ;; (let ((data (pymupdf-epc-page-svg-data page t)))
    (let ((data (pcase major-mode
                  ('doc-scroll-epdf-mode (funcall doc-scroll-image-data-function
																									page
																									doc-scroll-overlay-width)))))
      ;; (base64-decode-string
      ;; (pymupdf-epc-page-base64-image-data page
      ;; doc-scroll-overlay-width))))
                                        ; (or (doc-scroll-overlay-get page 'data))
      ;;  (when (eq major-mode 'doc-backend-pymupdf-mode)
      ;;    (funcall doc-scroll-image-data-function page width))))
      ;; (doc-scroll-display-page page nil)))
      (doc-scroll-display-page page data))))

(defun doc-scroll-display-page (page &optional data)
  (let* ((overlay-size (doc-scroll-overlay-size page))
         (internal-size (nth (1- page) doc-scroll-internal-page-sizes))

         ;; image-width should be doc-scroll-overlay-width, as we `internally
         ;; scale' to the overlay-size minus margins later
         (scaling (/ (float doc-scroll-overlay-width) (car internal-size)))
         (image-size (cons doc-scroll-overlay-width
                           (round (* scaling (cdr internal-size)))))
         (svg (svg-create (car image-size)
                          (cdr image-size)))
         (text-elements (when (eq major-mode 'doc-backend-djvu-mode)
                          (doc-djvu-text-elements 'char page)))
         (annots (when (eq major-mode 'doc-backend-djvu-mode)
                   (doc-scroll-annots page)))
         (annot-map (mapcar (lambda (a)
                              (pcase (car a)
                                ('background (message "Viewer background color should be %s (not (yet) implemented)"
                                                      (car annot)))
                                ('zoom (message "Zoom value should be %s (not (yet) implemented)" (car annot)))
                                ('mode (message "Mode value should be %s (not (yet) implemented)" (car annot)))
                                ('align (message "Horizontal annot vertical align should be %s %s (not (yet) implemented)"
                                                 (nth 1 annot) (nth 2 annot)))

                                ;; (message "%s %s %s %s" c internal-size 'djvu size))
                                ('maparea (doc-scroll-annot-to-hotspot a internal-size image-size))))
                            annots))
         (swiper-matches (doc-scroll-swiper-matches page))
         ;; (map (mapcar (lambda (c)
         ;;                ;; (message "%s %s %s %s" c internal-size 'djvu size))
         ;;                (doc-scroll-text-component-to-hotspot c internal-size 'djvu size))
         ;;              text-elements))
         (image-mime-type (pcase doc-scroll-image-type
                            ('png "image/png")
                            ('tiff "image/tiff")
                            ('pnm "image/x-portable-bitmap")))
         image)
    (when doc-scroll-svg-embed
      ;; (when (eq major-mode 'doc-backend-djvu-mode)
      (if data
          (if (eq major-mode 'doc-backend-pymupdf-mode)
              (doc-scroll-pdf-epc-svg-embed-base64 svg data "image/png")
            (svg-embed svg data
                       image-mime-type
                       t))
        (svg-embed svg
                   (concat "/tmp/doc-tools/"
                           (file-name-as-directory
                            (file-name-base (buffer-file-name)))
                           "pages/"
                           (format "page-%d.%s" page (symbol-name doc-scroll-image-type)))
                   image-mime-type
                   nil))
      (setq svg (append svg
                        (list (doc-scroll-annots-to-svg annots internal-size image-size))
                        (when swiper-matches
                          (list (doc-scroll-matches-to-svg swiper-matches internal-size image-size)))
                        (when (and doc-scroll-active-region (= page (car doc-scroll-active-region)))
                          (list (doc-scroll-regions-to-svg (cdr doc-scroll-active-region) internal-size image-size)))
                        (when doc-scroll-search-state
                          (let ((n (car doc-scroll-search-state))
                                (matches (copy-sequence (cdr doc-scroll-search-state)))) ;TODO calculate 'to svg' here
                            (setf (nth n matches) (append (cl-subseq (nth n matches) 0 5) (list :fill "green")))
                            (let ((page-matches (cl-remove-if-not (lambda (m)
                                                                    (= (car m) page))
                                                                  matches)))
                              (list (doc-scroll-matches-to-svg (mapcar #'cdr page-matches) internal-size image-size))))))))
    ;; (setq image (apply (if (eq major-mode 'doc-backend-djvu-mode) ;doc-scroll-svg-embed
    (setq image (apply (if doc-scroll-svg-embed
                           (apply-partially #'svg-image svg)
                         (apply-partially #'create-image data 'png t))
                       (list :page page ;; used for mouse event page info
                             :width (- (car overlay-size) (* 2 doc-scroll-horizontal-margin))
                             :margin (cons doc-scroll-horizontal-margin doc-scroll-vertical-margin)
                             :map
                             (append annot-map
                                     `(((rect . ((0 . 0) . (,(car image-size) . ,(cdr image-size))))
                                        ;; `(((rect . ((0 . 0) . (100 . 100)))
                                        ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                                        ,page
                                        (pointer arrow)))
                                     (when (eq major-mode 'doc-scroll-epdf-mode)
				       (pdf-links-hotspots-function page image-size))))))
    ;; (image-flush image)
    (when (= (doc-scroll-columns) 1)
      (overlay-put (doc-scroll-overlay page) 'before-string
                   (when (> (window-pixel-width) (car overlay-size))
                     (propertize " " 'display
                                 `(space :align-to
                                         (,(floor (/ (- (window-pixel-width) (car overlay-size)) 2))))))))
    (dolist (hs annot-map)
      (local-set-key
       (vector (nth 1 hs) 'mouse-1)
       (lambda (event)
         (interactive "@e")
         (let ((a (seq-find (lambda (i) (eq (posn-area (nth 1 event)) (nth 1 i)))
                            annot-map)))
           (doc-scroll-goto-page (plist-get (nth 2 a) 'help-echo)))))
      (local-set-key
       (vector (nth 1 hs) t)
       'doc-scroll-select-region))
    (when data
      (overlay-put (doc-scroll-overlay page) 'data data))
    (overlay-put (doc-scroll-overlay page) 'display image)))

(defun doc-scroll-goto-page-start ()
  (interactive)
  (doc-scroll-set-window-fscroll 0))

;; NOTE code based on (taken from) `pdf-view-goto-page'.
(defun doc-scroll-goto-page (page &optional window)
  "Go to PAGE in document."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  ;; (unless (and (>= page 1)
  ;; (<= page doc-scroll-last-page))
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
           (not (eq page (doc-scroll-page-at-point window)))))
      ;; (when changing-p
      ;;   (run-hooks 'doc-scroll-before-change-page-hook)
      ;;   (run-hooks 'doc-scroll-change-page-hook))
      (when (window-live-p window)
        (doc-scroll-display-pages (list page))
        (goto-char (doc-scroll-page-pos page))
        (doc-scroll-update)
        )
      (when changing-p
        (run-hooks 'doc-scroll-after-change-page-hook))
      ;; (when changing-p
      ;;   (pdf-view-deactivate-region)
      ;;   (force-mode-line-update)
      ;;   (run-hooks 'pdf-view-after-change-page-hook))))
      nil)))

(defun doc-scroll-goto-pos (page vscroll &optional window)
  "Go to PAGE in document."
  ;; first adjusting the scroll and then go to page displays smoothly
  (doc-scroll-set-window-fscroll vscroll)
  (doc-scroll-goto-page page window))

(defun doc-scroll-update-page-winprop ()
  (image-mode-window-put 'page (doc-scroll-page-at-point)))

(add-hook 'doc-scroll-after-change-page-hook #'doc-scroll-update-page-winprop)

(defun doc-scroll-scroll-forward (&optional screen)
  (interactive)
  (let ((new-vscroll (+ (window-vscroll nil t) (if screen
                                                   (window-text-height nil t)
                                                 doc-scroll-step-size))))
    (cond ((> new-vscroll (cdr (doc-scroll-current-size)))
           (forward-line)
           (doc-scroll-set-window-fscroll 0) ;or set to vertical margin
           (doc-scroll-update)
           (run-hooks 'doc-scroll-after-change-page-hook))
          ((> (+ new-vscroll (window-text-height nil t)) (cdr (doc-scroll-current-size)))
           (cond ((and (= (doc-scroll-columns) 1) (= (doc-scroll-page-at-point) doc-scroll-last-page))
                  (message "End of buffer"))
                 (t
                  (doc-scroll-set-window-fscroll new-vscroll)
                  (doc-scroll-update))))
          (t (doc-scroll-set-window-fscroll new-vscroll)))))

(defun doc-scroll-scroll-backward (&optional screen)
  (interactive)
  (let ((new-vscroll (- (window-vscroll nil t) (if screen
                                                   (window-text-height nil t)
                                                 doc-scroll-step-size))))
    (cond ((< new-vscroll 0)
           (cond ((<= (doc-scroll-page-at-point) (doc-scroll-columns))
                  (doc-scroll-set-window-fscroll 0) ;or set to vertical margin
                  (message "Beginning of buffer"))
                 (t
                  (forward-line -1)
                  (doc-scroll-set-window-fscroll (cdr (doc-scroll-current-size)))
                  (doc-scroll-update)
                  (run-hooks 'doc-scroll-after-change-page-hook))))
          ((< new-vscroll (- (window-text-height nil t) (cdr (doc-scroll-current-size))))
           (doc-scroll-set-window-fscroll new-vscroll)
           (doc-scroll-update))
          (t (doc-scroll-set-window-fscroll new-vscroll)))))

(defun doc-scroll-scroll-screen-forward ()
  (interactive)
  (doc-scroll-scroll-forward t))

(defun doc-scroll-scroll-screen-backward ()
  (interactive)
  (doc-scroll-scroll-forward t))

(defun doc-scroll-scroll-page-or-row (n)
  (let ((current-range (cons (window-start) (window-end))))
    (forward-line n)
    (redisplay)
    (unless (equal (cons (window-start) (window-end)) current-range)
      (doc-scroll-update))))

(defun doc-scroll-next-page (&optional n)
  (interactive "p")
  (doc-scroll-scroll-page-or-row (or n 1)))

(defun doc-scroll-previous-page (&optional n)
  (interactive "p")
  (doc-scroll-scroll-page-or-row (- (or n 1))))

;;; thumbnails
(defun doc-scroll-thumbs (&optional columns)
  "Show thumbs in a side window.
The number of COLUMNS can be set with a numeric prefix argument."
  (interactive "p")
  (let* ((buffer-name "*thumbs*")
         (buf (get-buffer buffer-name))
         (file (buffer-file-name))
         (output-dir (concat "/tmp/doc-tools/"
                             (file-name-as-directory (file-name-base file))
                             "thumbs/"))
         (last-page doc-scroll-last-page)
         (win (selected-window))
         (mode major-mode))
    (or (and buf
             (buffer-local-value 'doc-scroll-thumbs-columns buf)
             (= (buffer-local-value 'doc-scroll-thumbs-columns buf) columns))

        (with-current-buffer (get-buffer-create buffer-name)
          (unless (file-exists-p output-dir)
            (funcall (pcase mode
                       ('doc-backend-djvu-mode #'doc-djvu-decode-thumbs)
                       (_ #'doc-mupdf-create-thumbs))
                     file))
          (doc-scroll-thumbs-mode)
          (setq doc-scroll-thumbs-columns columns)
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
                                                 (format "thumb%d." p)
                                                 (pcase mode
                                                   ('doc-backend-djvu-mode "tif")
                                                   (_ "png")))
                                         (pcase mode
                                           ('doc-backend-djvu-mode 'tiff)
                                           (_ 'png))
                                         nil
                                         :margin '(2 . 1))))
                  (apply #'insert-button (format "%03d " p)
                         (append (list 'page (number-to-string p)
                                       'win win
                                       'face 'default
                                       'display im
                                       'action (lambda (b)
                                                 (with-selected-window (button-get b 'win)
                                                   (doc-scroll-goto-page
                                                    (string-to-number (button-get b 'page))
                                                    (button-get b 'win)))))
                                 (if doc-scroll-thumbs-show-page-numbers
                                     (list (if doc-scroll-thumbs-show-page-after
                                               'after-string
                                             'before-string)
                                           (format "%4d " p)) ;either this or help-echo
                                   (list 'help-echo (number-to-string p)))
                                 (when doc-scroll-thumbs-mouse-face
                                   (list 'mouse-face (list :background doc-scroll-thumbs-mouse-face-color))))))
                (when (= (% i columns) (1- columns)) (insert "\n")))
              (goto-char (point-min))

              (setq-local mode-line-format
                          `(" P" (:eval (button-get (button-at (point)) 'page))
                            ;; Avoid errors during redisplay.
                            "/" ,(number-to-string last-page)))
              ;; (unless doc-scroll-thumbs-show-page-numbers
              ;;   (add-hook 'post-command-hook #'display-local-help nil t))
              ))))

    (doc-scroll-thumbs-show columns)))

(defun doc-scroll-thumbs-show (columns)
  (let ((win (split-window nil
                           (- (+ (* columns (float (+ doc-scroll-thumbs-width 4 (if doc-scroll-thumbs-show-page-numbers 41 0))))
                                 (- (window-pixel-width)
                                    (window-body-width nil t))))
                           doc-scroll-thumbs-side t)))
    (set-window-buffer win "*thumbs*")
    (set-window-dedicated-p win t)
    (select-window win)))


(define-derived-mode doc-scroll-thumbs-mode special-mode "DS-Thumbs")

;;; Coords

(defun doc-scroll-coords-normalize (coords size &optional type)
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
      ('djvu-annot (list x0 (- 1 (+ y0 dy)) (+ x0 dx) (- 1 y0)))
      ('djvu-line (list x0 (- 1 y0) x1 (- 1 y1)))
      (_ ratios))))

(defun doc-scroll-coords-denormalize (coords size &optional type)
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

(defun doc-scroll-coords-convert (coords from-size from-type to-size &optional to-type)
  (mapcar #'round
          (doc-scroll-coords-denormalize
           (doc-scroll-coords-normalize coords from-size from-type)
           to-size to-type)))

(defun doc-scroll-coords-to-svg (page coords)
  (let ((internal-size (doc-scroll-internal-size page))
        (size (doc-scroll-overlay-size page)))
    (doc-scroll-coords-convert coords internal-size
                               (pcase major-mode
                                 ('doc-backend-djvu-mode 'djvu)
                                 ('doc-scroll-mupdf-mode 'pdf))
                               size 'svg)))

(defun doc-scroll-coords-point-scale (page coords)
  (pcase-let ((`(,iw . ,ih) (doc-scroll-internal-size page))
              (`(,w . ,h) (doc-scroll-overlay-size page)))
    (cons (* (/ (float iw) w) (car coords))
          (* (/ (float ih) h) (cdr coords)))))

;;; Hotspots

(defun doc-scroll-text-coords-to-hotspot-rect (coords from-size from-type to-size)
  (pcase-let* ((`(,w . ,h) to-size)
               (`(,x0 ,y0 ,x1 ,y1) (cl-mapcar (lambda (c1 c2) (round (* c1 c2)))
                                              (doc-scroll-coords-normalize coords from-size from-type)
                                              (list w h w h))))
    `(rect . ((,x0 . ,y0) . (,x1 . ,y1)))))


(defun doc-scroll-hotspot-rect-to-text-coords (area from-size to-size to-type)
  (pcase-let* ((`(rect . ((,x0 . ,y0) . (,x1 . ,y1))) area))
    (doc-scroll-coords-convert (list x0 y0 x1 y1) from-size 'hs to-size to-type)))

(defun doc-scroll-text-component-to-hotspot (component from-size from-type to-size)
  (let ((coords (cl-subseq component 1 5)))
    (list (doc-scroll-text-coords-to-hotspot-rect coords from-size from-type to-size)
          coords
          (list 'pointer 'text 'help-echo (nth 5 component)))))

(defun doc-scroll-annot-to-hotspot (annot from-size to-size)
  (pcase-let ((`(,url ,comment ,area) (cdr annot)))
    (pcase (car area)
      ('rect (pcase-let* ((`(,x0 ,y0 ,x1 ,y1) (doc-scroll-coords-convert
                                               (cdr area) from-size 'djvu-annot to-size)))
               (list `(rect . ((,x0 . ,y0) . (,x1 . ,y1)))

                     (intern (mapconcat 'number-to-string
                                        (mapcar 'truncate (list x0 y0 x1 y1)) "-"))
                     (append (list 'pointer 'hand)
                             (unless (string-blank-p url)
                               (list 'help-echo (string-to-number (substring url 1)))))))))))

;; (doc-scroll-annot-to-hotspot '(maparea "#22" ""
;;                                   (rect 470 3878 1808 92)
;;                                   (opacity 50)
;;                                   (none))
;;                         '(100 . 100)
;;                         '(200 . 200))

(defun doc-scroll-annots-to-svg (annots from-size to-size)
  (let ((svg-annots (svg-group 'annotations)))
    (dolist (a annots)
      (let* ((text (nth 2 a))
             (area (nth 3 a))
             (type (car area))
             (internal-coords (cdr area))
             (coords (doc-scroll-coords-convert internal-coords
                                                from-size
                                                (pcase type
                                                  ('line 'djvu-line)
                                                  (_ 'djvu-annot))
                                                to-size
                                                (when (memq type '(rect text))
                                                  'svg)))
             (fill (car (alist-get (pcase type ('rect 'hilite) ('line 'lineclr) ('text 'backclr))
                                   (nthcdr 4 a))))
             (opacity (pcase type ('line 0.8) ('text 0.5) (_ (if fill 0.3 1)))))
        (apply (pcase type
                 ('line #'svg-line)
                 (_ #'svg-rectangle))
               svg-annots
               (append coords
                       (list :fill (or fill "none")
                             :opacity opacity
                             :stroke-color (if (memq type '(line text))
                                               "black"
                                             (or fill "red")))))
        (when (eq type 'text)
          (let ((text-size (round (* 100 (/ (float (car to-size))
                                            (car from-size))))))
            (svg-text
             svg-annots text
             :font-size (number-to-string text-size)
             ;; :font-weight "bold"
             :stroke "black"
             :fill "black"
             :font-family "Cursive"
             ;; :letter-spacing "2pt"
             :x (+ (nth 0 coords) 5)
             :y (+ (nth 1 coords) text-size)
             :stroke-width 1)))))
    svg-annots))

(defun doc-scroll-matches-to-svg (matches from-size to-size)
  (let ((svg-matches (svg-group 'matches)))
    (dolist (m matches)
      (apply #'svg-rectangle
             svg-matches
             (append (doc-scroll-coords-convert (cl-subseq m 0 4)
                                                from-size
                                                (pcase major-mode
                                                  ('doc-backend-djvu-mode 'djvu)
                                                  (_ 'pdf))
                                                to-size 'svg)
                     (list :fill (or (plist-get m :fill) "yellow")
                           :opacity (or (plist-get m :opacity) 0.3)))))
    svg-matches))

(defun doc-scroll-regions-to-svg (regions from-size to-size)
  (let ((svg-regions (svg-group 'regions)))
    (dolist (r (doc-scroll-active-regions regions))
      (apply #'svg-rectangle
             svg-regions
             (append (doc-scroll-coords-convert r
                                                from-size
                                                (pcase major-mode
                                                  ('doc-backend-djvu-mode 'djvu)
                                                  (_ 'pdf))
                                                to-size 'svg)
                     (list :fill (or (plist-get r :fill) "gray")
                           :opacity (or (plist-get r :opacity) 0.3)))))
    svg-regions))

;;; annots
(defun doc-scroll-annots (page)
  (declare (gv-setter (lambda (val)
                        `(let ((c (alist-get ,page doc-scroll-contents)))
                           (if-let (a (assq 'annots c))
                               (setcdr a ,val)
                             (setf (nth 1 c) (cons 'annots ,val)))))))
  (if (not (eq major-mode 'doc-backend-pymupdf-mode))
      (if-let (a (cddr (alist-get page doc-scroll-annots)))
          a
        (alist-get 'annots
                   (alist-get page doc-scroll-contents)))))
;;; search
(defvar-local doc-scroll-search-state nil)

(defun doc-scroll-search (word)
  (interactive "sEnter search pattern: ")
  ;; (unless doc-scroll-contents
  ;;   (setq doc-scroll-contents (funcall doc-scroll-contents-function)))
  (let ((results (funcall (pcase major-mode
                            ('doc-backend-djvu-mode
                             #'doc-djvu-search-word)
                            ((or 'doc-backend-pymupdf-mode
                                 'doc-scroll-mupdf-mode)
                             #'doc-poppler-search-word))
                          word)))
    (setq doc-scroll-search-state (cons 0 results))
    (let* ((page (caadr doc-scroll-search-state))
           (coords (doc-scroll-coords-to-svg page (cl-subseq (cadr doc-scroll-search-state) 1 5))))
      (doc-scroll-goto-pos page (max (- (nth 1 coords) (/ (window-pixel-height) 2)) 0)))))

(defun doc-scroll-search-next ()
  (interactive)
  (unless (nth (cl-incf (car doc-scroll-search-state)) (cdr doc-scroll-search-state))
    (setf (car doc-scroll-search-state) 0))
  (let* ((n (car doc-scroll-search-state))
         (m (nth n (cdr doc-scroll-search-state)))
         (page (car m))
         ;; (coords (doc-scroll-coords-djvu-to-svg page (cl-subseq m 1 5))))
         (coords (doc-scroll-coords-to-svg page (cl-subseq m 1 5))))
    ;; first adjusting the scroll and then go to page displays smoothly
    (doc-scroll-goto-pos page (max (- (nth 1 coords) (/ (window-pixel-height) 2)) 0))))

;;; outline
(defun doc-scroll-outline ()
  (interactive)
  (funcall doc-scroll-outline-function))

;;; debug
(defun doc-scroll-debug (format-string &rest args)
  (apply #'lwarn 'doc-scroll :debug format-string args)
  (car args))

;;; translate
(when (featurep 'libretrans)
  (defun doc-scroll-translate-at-point ()
    (interactive)
    (libretrans-translate (doc-scroll-active-region-text))))

(provide 'doc-scroll)
;;; doc-scroll.el ends here
