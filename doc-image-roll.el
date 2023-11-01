(defgroup image-roll nil
	"Image roll")

(defcustom image-roll-overlay-max-width 850
  "The width of the cached images."
  :type 'numberp
  :group 'image-roll)

(defcustom image-roll-columns 1
	"Number of columns"
	  :type 'integer
  :group 'image-roll)

(defcustom image-roll-line-length 120
  "Number of characters per line.
The buffer can display only a number of columns that is equal to
a factor of this number."
  :type 'integer)

(defcustom image-roll-x-margin 1
  "Vertical margin around image (pixels), i.e. image separation height.
Because the margin is added to both sides of each image, the image
separation height is twice this value."
  :type 'integer
  :group 'image-roll)

(defcustom image-roll-y-margin 1
  "Vertical margin around image (pixels), i.e. image separation height.
Because the margin is added to both sides of each image, the image
separation height is twice this value."
  :type 'integer
  :group 'image-roll)

(defcustom image-roll-overlay-face-bg-color "gray"
  "Background color of overlay, i.e. image separation color."
  :type 'color
  :group 'image-roll)

(defcustom image-roll-step-size 50
  "Scroll step size in pixels units."
  :type '(choice function integer float))

(defcustom image-roll-smooth-scrolling t
  "Visually slightly smoother image turn when t.
Also display next/previous images outside window region.
Little slower on image jumps."
  :type 'boolean
  :group 'image-roll)

(defvar-local image-roll-image-file-sizes nil
  "List of image aspect ratios.
Each element is a cons of the form (width . height). Usually this
should just be a list of the document its intrinsic image sizes.")
(defvar-local image-roll-fit-height nil)
(defvar-local image-roll-length 0)

;;; Debugging
(setq warning-minimum-log-level :debug)

(defun image-roll-debug (format-string &rest args)
  (apply #'lwarn 'image-roll :debug format-string args)
  (car args))

;;; Generalized functions
(defun image-roll-columns (&optional winprops)
  "Number of columns.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'columns ,val ,winprops))))
  (image-mode-window-get 'columns winprops))

(defun image-roll-overlays (&optional winprops)
  "List of overlays that make up a scroll.
Setf-able function."
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'overlays ,val ,winprops))))
  (image-mode-window-get 'overlays winprops))

;; (defclass image ()
;; 	(overlay image dimensions))
	
(defun image-roll-overlay (&optional n winprops)
  "Return the overlay that hold n number PAGE.
Implemented as macro to make it setf'able.
TODO update docstring (no macro anymore)"
  (nth (1- (or n (image-roll-image-at-point)))
       (image-roll-overlays winprops)))

(defun image-roll-overlay-get (n prop)
  "List of overlays that make up a scroll."
  (overlay-get (nth (1- n) (image-roll-overlays)) prop))

(defun image-roll-image-at-point (&optional window)
  (interactive)
  (overlay-get (car (overlays-at (point))) 'image))

(defun image-roll-current-image (&optional winprops)
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'image ,val ,winprops))))
  (image-mode-window-get 'image winprops))

(defun image-roll-overlay-size (&optional winprops)
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put 'overlay-size ,val ,winprops))))
  (image-mode-window-get 'overlay-size winprops))

;; (defun image-roll-overlay-image (n)
;; 	(let ((spec (overlay-get (image-roll-overlay n) 'display)))
;; 		(if (imagep spec)
;; 				spec
;; 			(message "Overlay does not display an image")
;; 			nil)))

(defun image-roll-image-display-size (&optional window)
  (interactive)
  (overlay-get (car (overlays-at (point))) 'display-size))

(defun image-roll-image-file-size (image)
  (nth (1- image) image-roll-image-file-sizes))

(defun image-roll-image-p (object)
  (eq (car object) 'image))


;;; Initialize
(setq image-roll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'image-roll-scroll-forward)
        (define-key map (kbd "<down>") 'image-roll-scroll-forward)
        (define-key map (kbd "C-p") 'image-roll-scroll-backward)
        (define-key map (kbd "<up>") 'image-roll-scroll-backward)
        (define-key map (kbd "<wheel-down>") 'image-roll-scroll-forward)
        (define-key map (kbd "<wheel-up>") 'image-roll-scroll-backward)
        ;; (define-key map (kbd "<mouse-5>") 'image-roll-scroll-forward)
        ;; (define-key map (kbd "<mouse-4>") 'image-roll-scroll-backward)
        (define-key map "n" 'image-roll-next-image)
        (define-key map (kbd "<next>") 'image-roll-next-image)
        (define-key map "p" 'image-roll-previous-image)
        (define-key map (kbd "<prior>") 'image-roll-previous-image)
        (define-key map (kbd "S-<next>") 'image-roll-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'image-roll-scroll-screen-backward)
        (define-key map [remap goto-line] 'image-roll-goto-image)
        (define-key map "f" 'image-roll-fit-toggle)
        (define-key map "c" 'image-roll-set-columns)
        (define-key map "t" 'image-roll-thumbs)
        map))


(define-minor-mode image-roll-mode
  "Image Roll "
  :lighter "IRoll "
  :keymap image-roll-mode-map

	(when (member major-mode '(doc-backend-pymupdf-mode))
		(doc-scroll-mode))

	;; (setq evil-force-cursor t)
	;; (setq-local evil-motion-state-cursor nil)
	;; (setq-local evil-default-cursor nil)
	(toggle-truncate-lines 1)
	(setq cursor-type nil)

  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `image-roll-redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and add lines from
  ;; `image-mode-reapply-winprops' at the start of `image-roll-redisplay'.
  ;; (add-hook 'window-configuration-change-hook 'image-roll-redisplay-all nil t)
  (add-hook 'window-configuration-change-hook 'image-roll-redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'image-roll-new-window-function nil t)
  (setq image-mode-winprops-alist nil)

  (dolist (m '(global-hl-line-mode))
    (if (fboundp m)
        (funcall m 0)))

  ;; (setq-local mode-line-format
	(setq-local mode-line-position
							`(" P" (:eval (number-to-string (image-roll-image-at-point)))
								;; Avoid errors during redisplay.
								"/" (:eval (number-to-string image-roll-length))))
	)

(defun image-roll-new-window-function (winprops)
  (image-roll-debug "NEW_WINDOW")
  (if (not (overlays-at 1))
      (let (overlays
            (images image-roll-image-sizes)
            (win (car winprops))
            (inhibit-read-only t))

        (erase-buffer)

        ;; here we only add the 'image' and 'window' overlay-properties, we add
        ;; more properties/information as soon as it becomes available in the
        ;; 'image-roll-redisplay' function
        (seq-do-indexed (lambda (size i)
													(let ((o (make-overlay
																		;; (prog1 (point) (insert (make-string image-roll-line-length (string-to-char " "))))
																		;; (point))))
																		(point)
																		(progn (insert (make-string image-roll-line-length (string-to-char " ")))
																					 (point)))))
														(insert "\n")
														(overlay-put o 'image  (1+ i))
														(overlay-put o 'window win)
														(push o overlays)))
												images)
        (delete-char -1)

        ;; this function also calls `image-roll-new-window-function'
        (image-mode-window-put 'overlays (nreverse overlays) winprops)
        (set-buffer-modified-p nil)

        ;; need to place point at some overlay, before continuing 'filling the
        ;; overlays' in the `image-roll-redisplay' function
        ;; NOTE, when implementing some 'restore' functionality, then jump to
        ;; the correct 'point' here (the overlays, i.e. `image-roll-image-pos', are
        ;; not yet available).
        ;; (setf (image-roll-current-image) (image-roll-))
        (goto-char (point-min))
        ;; (goto-char (image-roll-image-pos (or (image-roll-current-image) 1)))
        )

    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (image-roll-overlays)))
          (pos (image-mode-window-get 'image)))
      (image-mode-window-put 'overlays ols winprops)


      ;; cursor should be on overlay for `image-roll-image-at-point'
      ;; (setf (image-roll-image-at-point (car winprops))
      ;;       (or (image-roll-image-at-point (car winprops)) 1))))))
      ;; (goto-char (image-roll-image-pos (or (image-roll-current-image) 1)))
      )))

;;; (Re)display
(defun image-roll-redisplay (&optional force window)
  ;; if new window then, (image-roll-columns) calls `image-mode-winprops' which runs
  ;; the `image-mode-new-window-functions' and sets up (a) new winprops(-alist)
  ;; (image-roll-debug "REDISPLAY")

  ;; (when (or (not (and (image-roll-window-width)
  ;;                     (= (image-roll-window-width) (window-pixel-width))))
  ;;           force)
  ;;   (setf (image-roll-window-width) (window-pixel-width))

  (let* ((columns (or (image-roll-columns) 1))
         (overlay-size (image-roll-desired-overlay-size))
         (n 0))

    (seq-do-indexed
		 (lambda (size i)
       (let* ((overlay-width (car overlay-size))
							(overlay-heigth (cdr overlay-size))
							(o (nth n (image-roll-overlays))))

         ;; NOTE we would like to store data in the overlay, but the following
         ;; functions seems not to remove that data in time
         ;; (overlay-put o 'data nil)
         (overlay-put o 'face `(:background ,image-roll-overlay-face-bg-color))
         (when (or (not (image-roll-columns)) (= (image-roll-columns) 1))
           (overlay-put o 'before-string
												(when (> (window-pixel-width) (car overlay-size))
													(propertize " " 'display
																			`(space :align-to
																							(,(floor (/ (- (window-pixel-width) (car overlay-size)) 2))))))))
         (overlay-put o 'display `(space . (:width (,overlay-width) :height (,overlay-heigth))))
         ;; (overlay-put o 'face `(:background ,image-roll-overlay-face-bg-color))
         ;; (overlay-put o 'size overlay-size)
         (overlay-put o 'display-size (image-roll-debug "%s" (calculate-image-roll-image-display-size size overlay-size)))
         (setq n (1+ n))))
		 image-roll-image-sizes)
    ;; (redisplay t) ;NOTE does not help for display issue
    (setf (image-roll-columns) columns)
    (setf (image-roll-overlay-size) overlay-size))
  ;; (sit-for 0.01) ;NOTE does not help for display issue
  (image-roll-goto-pos (or (image-roll-current-image) 1)
                       (or (window-vscroll nil t) 0))
  (image-roll-update))

(defun image-roll-image-size-mode ()
	(let (freqs
				mode
				(max-freq 0))
		(dolist (s image-roll-image-sizes)
			(if-let (entry (assoc s freqs))
					(when (> (cl-incf (cdr entry)) max-freq)
						(setq mode s))
				(push (cons s 1) freqs)))
		(if (= max-freq 1) nil mode)))

(defun image-roll-image-size-average ()
	(let ((total-x 0)
				(total-y 0))
		(dolist (s image-roll-image-sizes)
			(cl-incf total-x (car s))
			(cl-incf total-y (cdr s)))
		(cons (/ total-x image-roll-length)
					(/ total-y image-roll-length))))

(defun image-roll-desired-overlay-size ()
	(let* ((win-width (float (window-pixel-width)))
				 (win-height (float (window-pixel-height)))
				 (aspect-ratio (if (= image-roll-length 1)
													 (car image-roll-image-sizes)
												 (or (image-roll-image-size-mode)
														 (image-roll-image-size-average))))
				 (width (unless image-roll-fit-height
									(min (/ win-width image-roll-columns)
											 image-roll-overlay-max-width)))
				 (factor (if width
										 (/ (float width) (car aspect-ratio))
									 (/ (float win-height) (cdr aspect-ratio)))))
		(if width
				(cons (floor width)
							(+ (floor (* factor (cdr aspect-ratio)))
								 (* 2 image-roll-y-margin)))
			(cons (floor (* factor (car aspect-ratio)))
						(floor (- win-height
											(* 2 image-roll-y-margin)))))))

(defun calculate-image-roll-image-display-size (image-file-size overlay-size)
	(let* ((ow (- (car overlay-size) (if (= image-roll-columns 1)
																							 0
																						 (* 2 image-roll-x-margin))))
				 (oh (- (cdr overlay-size) (* 2 image-roll-y-margin)))
				 (iw (car image-file-size))
				 (ih (cdr image-file-size))
				 (x-scale (/ (float ow) iw))
				 (y-scale (/ (float oh) ih)))
		(if (< y-scale x-scale)
				(cons (* y-scale iw) oh)
			(cons ow (* x-scale ih)))))

(defun image-roll-update (&optional current)
  (interactive)
  (let* ((visible (image-roll-debug "visibla images %s" (image-roll-visible-images)))
         (displayed (image-roll-displayed-images))
                                        ;NOTE the condition might only be relevant when new window (not sure)
         (n-visible (length visible)))
    (cond ((or (= n-visible image-roll-length)
               (= n-visible 0))
           (image-roll-debug "number of visible overlays %s (all)" (length visible))
           (image-roll-display-images '(1)))
          (t
           (dolist (p (cl-set-difference displayed visible))
             (image-roll-undisplay-image p))
           (let* ((images (cl-set-difference visible displayed))
                  (non-exisiting-images (cl-remove-if (lambda (p)
                                                        (let* ((display-prop (image-roll-overlay-get p 'display)))
                                                          (when (image-roll-image-p display-prop)
                                                            (= (car (image-size display-prop)) w))))
                                                      images)))
             (if current
                 (image-roll-display-images displayed)
               (when (image-roll-debug "%s" non-exisiting-images)
                 (image-roll-display-images non-exisiting-images))))))))

(defun image-roll-overlay-selected-window-filter (overlays)
  (cl-remove-if-not
   (lambda (overlay)
     (eq (overlay-get overlay 'window) (selected-window)))
   overlays))

(defun image-roll-visible-images ()
  (interactive)
  ;; (sit-for 0.001) ;when overlays are not yet 'filled', and their sizes are still
  ;;                                       ;small, then overlays-in returns too many overlays
	(redisplay)
  (mapcar (lambda (o)
            (overlay-get o 'image))
          (delq nil (image-roll-overlay-selected-window-filter
                     (overlays-in (max (- (window-start) (if image-roll-smooth-scrolling image-roll-line-length 0)) 1)
																	;; passing the UPDATE argument to
																	;; window-end is important here, to
																	;; make sure that it gets determined
																	;; after a redisplay
                                  (+ (window-end nil t) (if image-roll-smooth-scrolling image-roll-line-length 0)))))))

(defun image-roll-displayed-images ()
  (interactive)
  (let ((im-overlays (seq-filter (lambda (ov)
                                   (eq (car (overlay-get ov 'display))
                                       'image))
                                 (image-roll-overlay-selected-window-filter
                                  (overlays-in (point-min) (point-max))))))
    (mapcar (lambda (o)
              (overlay-get o 'image))
            im-overlays)))

(defun image-roll-undisplay-image (n)
  ;; (print "of hier")
  (pcase-let* ((`(,w . ,h) (image-roll-overlay-size)))
    (overlay-put (image-roll-overlay n) 'data nil) ;; TODO improve/implement
    ;; caching algorithm
    (overlay-put (image-roll-overlay n)
                 'display `(space . (:width (,w) :height (,h))))))

(defun image-roll-goto-pos (image vscroll &optional window)
  "Go to PAGE in document."
  ;; first adjusting the scroll and then go to image displays smoothly
  (image-roll-set-window-fscroll vscroll)
  (image-roll-goto-image image window))

(defun image-roll-goto-image (image &optional window)
  "Go to PAGE in document."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  ;; (unless (and (>= image 1)
  ;; (<= image image-roll-len))
  ;; (error "No such image: %d" image))
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
           (not (eq image (image-roll-image-at-point window)))))
      ;; (when changing-p
      ;;   (run-hooks 'image-roll-before-change-image-hook)
      ;;   (run-hooks 'image-roll-change-image-hook))
      (when (window-live-p window)
        (image-roll-display-images (list image))
        (goto-char (image-roll-image-pos image))
        ;; (image-roll-update)
        )
      (when changing-p
        (run-hooks 'image-roll-after-change-image-hook))
      ;; (when changing-p
      ;;   (pdf-view-deactivate-region)
      ;;   (force-mode-line-update)
      ;;   (run-hooks 'pdf-view-after-change-image-hook))))
      nil)))

(defun image-roll-image-pos (image &optional window)
  (overlay-start (nth (1- image) (image-roll-overlays window))))

(defun image-roll-vscroll-to-fscroll (vscroll)
  (/ (float vscroll)
     (cdr (image-roll-overlay-size))))

(defun image-roll-set-window-fscroll (vscroll)
  "Set vscroll in units of current image height."
  (let ((fscroll (image-roll-vscroll-to-fscroll vscroll)))
    (setf (image-mode-window-get 'fscroll) fscroll)
    (set-window-vscroll (selected-window) vscroll t)))

(defun image-roll-fscroll-to-vscroll (fscroll &optional image)
  (* fscroll
     (cdr (image-roll-overlay-size))))

(defun image-roll-display-images (images &optional force)
  ;; (print "RUNNING" #'external-debugging-output)
  (image-roll-debug "display images %s" images)
  (dolist (n images)
    (image-roll-display-image n)))

;; (defun image-roll-display-image (n &optional data)
;;   (let* ((overlay-size (image-roll-overlay-size n))
;;          (image-size (nth (1- n) image-roll-image-sizes))

;; 				 (max-h (round (- (cdr overlay-size) (* 2 image-roll-y-margin))))
;; 				 ;; if more than one column then we add vertical gaps by
;; 				 ;; making the images smaller by some margin
;; 				 (single-column (eq (image-roll-columns) 1))
;; 				 (max-w (unless single-column
;; 									(round (- (car overlay-size) (* 2 image-roll-x-margin)))))

;; 				 (o (image-roll-overlay n))
;; 				 (image-file (concat "/tmp/doc-tools/"
;; 														 (file-name-as-directory
;; 															(file-name-base (buffer-file-name)))
;; 														 "pages/"
;; 														 (format "page-%d.%s" n 'png)))
;; 				 ;; TODO currently we assume source files are existing images
;; 				 ;; (for documents these are in some subdir of /tmp/doc-tools)
;; 				 (source-image-size (image-size (create-image image-file) t))
;; 				 (svg (svg-create (car source-image-size) (cdr source-image-size)))
;; 				 image)

;; 		;; NOTE uisng the png images directly for some reason results
;; 		;; in slow scrolling
;; 		;; (setq image (create-image image-file nil nil

;; 		;; (svg-rectangle svg 0 0 100 100 :fill "red")

;; 		(svg-embed svg
;; 							 image-file
;; 							 "image/png"
;; 							 nil)
;; 		(when doc-scroll-mode
;; 			(when doc-scroll-search-mode
;; 				(doc-scroll-svg-search-results svg n))
;; 			(when doc-scroll-select-mode
;; 			;; (when (and doc-scroll-select-mode
;; 			;; 					 (= (car doc-scroll-active-region) n))
;; 				(setq svg (doc-scroll-svg-active-region svg n))))
;; 		;; we'd like to get the displayed size, so we store the display
;; 		;; w and h here. Also Emacs provides no function to get
;; 		;; image-size from a file, only from a spec
;; 		(setq image (svg-image svg
;; 													 :image n ;; used for mouse event n info
;; 													 :max-width max-w
;; 													 :max-height max-h
;; 													 :margin (cons (if single-column 0 image-roll-x-margin)
;; 																				 image-roll-y-margin)
;; 													 :pointer 'arrow))

;; 		;; calculate and add display-size to image spec
;; 		(let ((display-size (if single-column
;; 														(image-size image t)
;; 													(pcase-let* ((`(,x . ,y) (image-size image t))
;; 																			 (scale-x (/ (float max-w) x))
;; 																			 (scale-y (/ (float max-h) y))
;; 																			 (scale (min scale-x scale-y)))
;; 														(cons (* x scale) (* y scale))))))
;; 			(setf (image-property image :display-size) display-size))

;; 		;; 	;; TODO we could add hotspots, but it makes reading events
;; 		;; 	;; tricky, we must unread the events (or  (see
;; 		;; 	;; `pdf-view-text-regions-hotspots-function' and
;; 		;; 	;; `pdf-util-image-map-mouse-event-proxy')
;; 		;; 	(when (fboundp 'doc-backend-pymupdf-mode)
;; 		;; 		(if (overlay-get o 'text)
;; 		;; 				(let ((hot-spots (when-let (text (overlay-get o 'text))
;; 		;; 													 (doc-scroll-hot-spots
;; 		;; 														(mapcar (lambda (w)
;; 		;; 																			(doc-scroll-coords-scale (seq-take w 4)
;; 		;; 																															 image-size
;; 		;; 																															 display-size))
;; 		;; 																		text)))))
;; 		;; 					(setf (image-property image :map) hot-spots))
;; 		;; 			(doc-pymupdf-epc-structured-text n nil t))))
;; 		(overlay-put (image-roll-overlay n)
;; 								 'text
;; 								 (doc-pymupdf-epc-structured-text n))
;; 		;; (image-flush n)
;; 		;; (when (= (image-roll-columns) 1)
;; 		(overlay-put o 'before-string
;; 								 (when (> (window-pixel-width) (car overlay-size))
;; 									 (propertize " " 'display
;; 															 `(space :align-to
;; 																			 (,(floor (/ (- (window-pixel-width)
;; 																											(* (car overlay-size) (image-roll-columns)))
;; 																									 2))))))) ; no error when negative :)
;; 		(overlay-put o 'display image)))

(defun image-roll-display-image (n &optional data)
  (let* ((overlay-size (image-roll-overlay-size))
				 (overlay-width (car overlay-size))
				 (overlay-height (ldbg (cdr overlay-size)))
         (image-size (nth (1- n) image-roll-image-sizes))
				 (image-file-size (nth (1- n) image-roll-image-file-sizes))
				 (image-display-size (image-roll-overlay-get n 'display-size))

				 (max-h (round (- overlay-height (* 2 image-roll-y-margin))))
				 ;; if more than one column then we add vertical gaps by
				 ;; making the images smaller by some margin
				 (single-column (eq (image-roll-columns) 1))
				 ;; emacs will automatically use smaller width to preserve the
				 ;; aspect ratio
				 (w (round (- overlay-width
											(if single-column
													0
												(* 2 image-roll-x-margin)))))
				 (o (image-roll-overlay n))
				 (image-file (if doc-scroll-mode
												 (concat "/tmp/doc-tools/"
																 (file-name-as-directory
																	(file-name-base (buffer-file-name)))
																 "pages/"
																 (format "page-%d.%s" n 'png))
											 (nth (1- n)
														(directory-files image-dir t (image-file-name-regexp)))))
				 (image-mime-type (if  doc-scroll-mode
															"image/png"
														(let ((im-type (image-type image-file)))
															 (pcase im-type
																	 ('png "image/png")
																 ('jpeg "image/jpeg")))))

				 ;; TODO currently we assume source files are existing images
				 ;; (for documents these are in some subdir of /tmp/doc-tools)
				 ;; (source-image-size (image-size (create-image image-file) t))
				 (svg (svg-create (ldbg  (car image-file-size)) (ldbg  (cdr image-file-size))))
				 image)

		;; NOTE uisng the png images directly for some reason results
		;; in slow scrolling
		;; (setq image (create-image image-file nil nil

		;; (svg-rectangle svg 0 0 100 100 :fill "red")

		(svg-embed svg
							 image-file
							 image-mime-type
							 nil)
		(when doc-scroll-mode
			(when doc-scroll-search-mode
				(doc-scroll-svg-search-results svg n))
			(when (and doc-scroll-select-mode
								 (= (car doc-scroll-active-region) n))
				(setq svg (doc-scroll-svg-active-region svg n))))
		;; we'd like to get the displayed size, so we store the display
		;; w and h here. Also Emacs provides no function to get
		;; image-size from a file, only from a spec
		(setq image (svg-image svg
													 :image n ;; used for mouse event n info
													 :width w
													 :max-height max-h
													 :margin (cons (round (/ (float (- overlay-width (car image-display-size))) 2))
																				 (round (/ (float (- overlay-height (cdr image-display-size))) 2)))
													 :pointer 'arrow))

		;; calculate and add display-size to image spec
		;; (let ((display-size (if single-column
		;; 												(image-size image t)
		;; 											(pcase-let* ((`(,x . ,y) (image-size image t))
		;; 																	 (scale-x (/ (float max-w) x))
		;; 																	 (scale-y (/ (float max-h) y))
		;; 																	 (scale (min scale-x scale-y)))
		;; 												(cons (* x scale) (* y scale))))))
		;; 	(setf (image-property image :display-size) display-size))

		;; 	;; TODO we could add hotspots, but it makes reading events
		;; 	;; tricky, we must unread the events (or  (see
		;; 	;; `pdf-view-text-regions-hotspots-function' and
		;; 	;; `pdf-util-image-map-mouse-event-proxy')
		;; 	(when (fboundp 'doc-backend-pymupdf-mode)
		;; 		(if (overlay-get o 'text)
		;; 				(let ((hot-spots (when-let (text (overlay-get o 'text))
		;; 													 (doc-scroll-hot-spots
		;; 														(mapcar (lambda (w)
		;; 																			(doc-scroll-coords-scale (seq-take w 4)
		;; 																															 image-size
		;; 																															 display-size))
		;; 																		text)))))
		;; 					(setf (image-property image :map) hot-spots))
		;; 			(doc-pymupdf-epc-structured-text n nil t))))
		;; (overlay-put (image-roll-overlay n)
		;; 						 'text
		;; 						 (doc-pymupdf-epc-structured-text n))
		;; (image-flush n)
		;; (when (= (image-roll-columns) 1)
		(overlay-put o 'before-string
								 (when (> (window-pixel-width) overlay-width)
									 (propertize " " 'display
															 `(space :align-to
																			 (,(floor (/ (- (window-pixel-width)
																											(* overlay-width (image-roll-columns)))
																									 2))))))) ; no error when negative :)
		(overlay-put o 'display image)))

(defun image-roll-scroll-forward (&optional screen)
  (interactive)
  (let ((new-vscroll (+ (window-vscroll nil t) (if screen
                                                   (window-text-height nil t)
                                                 image-roll-step-size))))
    (cond ((> new-vscroll (cdr (image-roll-overlay-size)))
           (forward-line)
           (image-roll-set-window-fscroll 0) ;or set to vertical margin
           (image-roll-update)
           (run-hooks 'image-roll-after-change-image-hook))
          ((> (+ new-vscroll (window-text-height nil t)) (cdr (image-roll-overlay-size)))
           (cond ((and (= (image-roll-columns) 1) (= (image-roll-image-at-point) image-roll-length))
                  (message "End of buffer"))
                 (t
                  (image-roll-set-window-fscroll new-vscroll)
                  (image-roll-update))))
          (t (image-roll-set-window-fscroll new-vscroll)))))

(defun image-roll-scroll-backward (&optional screen)
  (interactive)
  (let ((new-vscroll (- (window-vscroll nil t) (if screen
                                                   (window-text-height nil t)
                                                 image-roll-step-size))))
    (cond ((< new-vscroll 0)
           (cond ((<= (image-roll-image-at-point) (image-roll-columns))
                  (image-roll-set-window-fscroll 0) ;or set to vertical margin
                  (message "Beginning of buffer"))
                 (t
                  (forward-line -1)
                  (image-roll-set-window-fscroll (cdr (image-roll-overlay-size)))
                  (image-roll-update)
                  (run-hooks 'image-roll-after-change-image-hook))))
          ((< new-vscroll (- (window-text-height nil t) (cdr (image-roll-overlay-size))))
           (image-roll-set-window-fscroll new-vscroll)
           (image-roll-update))
          (t (image-roll-set-window-fscroll new-vscroll)))))

(defun image-roll-scroll-image-or-row (n)
  (let ((current-range (cons (window-start) (window-end))))
    (forward-line n)
    (redisplay)
    (unless (equal (cons (window-start) (window-end)) current-range)
      (image-roll-update))))

(defun image-roll-next-image (&optional n)
  (interactive "p")
  (image-roll-scroll-image-or-row (or n 1)))

(defun image-roll-previous-image (&optional n)
  (interactive "p")
  (image-roll-scroll-image-or-row (- (or n 1))))

(defun image-roll-goto-image-start ()
  (interactive)
  (image-roll-set-window-fscroll 0))

(defun image-roll-fit-toggle ()
  (interactive)
  (setq image-roll-fit-height (not image-roll-fit-height))
  (image-roll-redisplay t))

(define-derived-mode image-roll-test-mode special-mode "PDFtest "
		(image-roll-mode)
		(setq-local
		 ;; image-dir "/tmp/doc-tools/Peter Seibel - Practical Common Lisp-Apress (2005)/pages"
		 image-dir "/home/dalanicolai/Pictures/Noha"
		 ;; image-dir "/home/dalanicolai/Pictures/Screenshots"
		 image-roll-images (directory-files image-dir
																				t
																				(image-file-name-regexp)
																				)
		 image-roll-image-file-sizes
		 ;; using `file" shell command
		 (ldbg  (let* ((default-directory image-dir)
						(files (directory-files default-directory
																		nil
																		(image-file-name-regexp)))
						(metadata (apply #'process-lines "file" files))
						(hits (seq-filter (apply-partially #'string-match-p "[[:digit:]]+ *x *[[:digit:]]+")
															metadata)))
			 (mapcar (lambda (x)
								 (string-match ", *\\([[:digit:]]+ *x *[[:digit:]]+\\)" x)
								 (let ((size (mapcar #'string-to-number (string-split (match-string 1 x) "x"))))
									 (cons (car size) (cadr size))))
							 hits)))

		 image-roll-image-sizes image-roll-image-file-sizes
		 ;; using `identify"
		 ;; (mapcar (lambda (f)
		 ;; 					 (pcase-let ((`(,x ,y) (mapcar #'string-to-number
		 ;; 																				 (split-string
		 ;; 																					(nth 2 (split-string
		 ;; 																									(shell-command-to-string
		 ;; 																									 (format "identify %s" (shell-quote-argument f)))))
		 ;; 																					"x"))))
		 ;; 						 (cons x y)))
		 ;; 				 (directory-files image-dir t (image-file-name-regexp)))
		 ;; using `file'
		 ;; (mapcar (lambda (f)
		 ;; 					 (let ((size (split-string
		 ;; 												(nth 17 (split-string
		 ;; 																 (shell-command-to-string
		 ;; 																	(format "file %s" (shell-quote-argument f)))))
		 ;; 												"x")))
		 ;; 						 (when (= (length size) 2)
		 ;; 							 (cons (string-to-number (car size))
		 ;; 										 (string-to-number (substring (cadr size) 0 -1))))))
		 ;; 				 (directory-files "~/Pictures/Noha" t (image-file-name-regexp)))
		 image-roll-length (length image-roll-image-sizes)))

(when (featurep 'evil)
  (evil-define-key 'motion image-roll-mode-map
    "j" 'image-roll-scroll-forward
    "k" 'image-roll-scroll-backward
    (kbd "<down>") 'image-roll-scroll-forward
    (kbd "<up>") 'image-roll-scroll-backward
    (kbd "<wheel-down>") 'image-roll-scroll-forward
    (kbd "<wheel-up>") 'image-roll-scroll-backward
    ;; (kbd "<mouse-5>") 'image-roll-scroll-forward
    ;; (kbd "<mouse-4>") 'image-roll-scroll-backward
    "J" 'image-roll-next-image
    "K" 'image-roll-previous-image
    (kbd "<next>") 'image-roll-next-image
    (kbd "<prior>") 'image-roll-previous-image
    (kbd "C-j") 'image-roll-scroll-screen-forward
    (kbd "C-k") 'image-roll-scroll-screen-backward
    (kbd "S-<next>") 'image-roll-scroll-screen-forward
    (kbd "S-<prior>") 'image-roll-scroll-screen-backward
    "G" 'image-roll-goto-image
    "f" 'image-roll-fit-toggle
    "c" 'image-roll-set-columns))
