;;; -*- lexical-binding: t; -*-

(require 'image-mode)

;; (load-file "/home/dalanicolai/git/doc-tools/doc-scroll.el")
(load-file "/home/dalanicolai/git/doc-tools-pymupdf/doc-pymupdf-epc.el")

(when (featurep 'undo-tree)
  (add-to-list 'undo-tree-incompatible-major-modes 'doc-scroll-mode))

(defvar doc-scroll-incompatible-modes '(visual-line-mode
                                        global-hl-line-mode))

(defvar-local doc-scroll-step-size 80)

(cl-pushnew (cons "\\.pdf\\'" 'doc-scroll-mode) auto-mode-alist)

(defun doc-pymupdf-kill-server ()
  ;; (ldbg "STOP EPC")
  (epc:stop-epc doc-pymupdf-epc-server))

(setq doc-scroll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'doc-scroll-forward)
        (define-key map (kbd "<down>") 'doc-scroll-forward)
        (define-key map (kbd "C-p") 'doc-scroll-backward)
        (define-key map (kbd "<up>") 'doc-scroll-backward)
        (define-key map (kbd "<wheel-down>") 'doc-scroll--scroll-forward)
        (define-key map (kbd "<wheel-up>") 'doc-scroll-scroll-backward)
        ;; (define-key map (kbd "<mouse-5>") 'doc-scroll--scroll-forward)
        ;; (define-key map (kbd "<mouse-4>") 'doc-scroll-scroll-backward)
        (define-key map "n" 'doc-scroll-next-page)
        (define-key map (kbd "<next>") 'doc-scroll-next-page)
        (define-key map "p" 'doc-scroll-previous-page)
        (define-key map (kbd "<prior>") 'doc-scroll-previous-page)
        (define-key map (kbd "S-<next>") 'doc-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'doc-scroll-screen-backward)
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

;; (add-hook 'doc-scroll-mode-hook (lambda nil (doc-pymupdf-epc-structured-text 'words)))

(define-derived-mode doc-scroll-mode special-mode "DT"
;; (define-derived-mode doc-scroll-mode special-mode "DT"
  (let ((default-directory "/home/dalanicolai/git/doc-tools-pymupdf/"))
    (setq doc-pymupdf-epc-server (epc:start-epc "python" '("doc-pymupdf-epc-server.py"))))
  (doc-pymupdf-epc-init)
  (add-hook 'kill-buffer-hook #'doc-pymupdf-kill-server nil t)


  (dolist (m doc-scroll-incompatible-modes)
    (funcall m -1))

  (add-hook 'window-configuration-change-hook 'doc-scroll-redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'doc-scroll-new-window-function nil t)

  (setq-local doc-scroll-number-of-pages (doc-pymupdf-epc-number-of-pages)
              doc-scroll-internal-page-sizes (doc-pymupdf-epc-page-sizes))

  (setq image-mode-winprops-alist nil)
  (image-mode-winprops)

  (set-buffer-modified-p nil)
  (ldbg "doc-scroll-mode"))

(defun doc-scroll-create-overlays (number
                                   &optional columns hspace vspace text
                                   &rest overlay-props)
  (setq columns (or columns 1))
  (dolist (m doc-scroll-incompatible-modes)
    (funcall m -1))
  (toggle-truncate-lines 1) ; also disables visual-mode
  (let (overlays)
    ;; (ldbg "OVERLAYS")
    (dotimes (i number)
      (let* ((n (1+ i))
             (o (make-overlay
                 (point)
                 (progn (insert (cond ((stringp text) text)
                                      ((functionp text) (funcall text n))
                                      (t " ")))
                        (point))
                 (unless (= (% columns 3) 0)
                   (insert (make-string (or hspace 0) (string-to-char " ")))))))
        (when (and (= (% n columns) 0)
                   (not (= n number)))
          (insert "\n")
          (insert (make-string (or vspace 0) (string-to-char "\n"))))
        (overlay-put o 'i i)
        (dotimes (j (/ (length overlay-props) 2))
          (let ((m (* j 2)))
            (overlay-put o (nth m overlay-props) (nth (+ m 1) overlay-props))))
        (push o overlays)))
    (goto-char (point-min))
    ;; (setq-local doc-scroll-columns columns) ; TODO make a winprop
    ;; (doc-scroll-mode)
    (nreverse overlays)))

(defun doc-scroll-page-overlay (page)
  (nth (1- page) (image-mode-window-get 'overlays)))
  
(defsubst doc-scroll-current-overlay ()
  (seq-find (lambda (o)
              (eq (overlay-get o 'window) (selected-window)))
            (overlays-at (point))))

(defun doc-scroll-current-page ()
  (1+ (overlay-get (doc-scroll-current-overlay) 'i)))

(defun doc-scroll-current-overlay-height ()
  (cdr (overlay-get (doc-scroll-current-overlay) 'size)))

(defun doc-scroll-overlay-base-width (columns hspace)
  (let ((win-width (- (nth 2 (window-inside-pixel-edges))
                      (nth 0 (window-inside-pixel-edges)))))
    (/ (- win-width (* (1- columns) (* hspace (frame-char-width))))
       columns)))

(defun doc-scroll-overlay-base-height (base-width)
  (let* ((widest-page-w (apply #'max (mapcar #'car doc-scroll-internal-page-sizes)))
         (ratio (/ base-width widest-page-w)))
    (floor (* ratio (apply #'max (mapcar #'cdr doc-scroll-internal-page-sizes))))))

(defun doc-scroll-visible-overlays ()
  (let* ((visible (overlays-in (window-start) (window-end nil t)))
         (start (apply #'min (mapcar (lambda (o) (overlay-get o 'i)) visible)))
         (end (apply #'max (mapcar (lambda (o) (overlay-get o 'i)) visible)))
         ;; include previous/next rows for 'smoother' displaying
         (new-start (max (- start (image-mode-window-get 'columns)) 0))
         (new-end (min (+ end (image-mode-window-get 'columns)) (1- doc-scroll-number-of-pages))))
         ;; start and end should be limited to index start/end page
    ;; (seq-subseq overlays (max new-start 0) (1+ (min new-end (length overlays))))))
    (seq-subseq (image-mode-window-get 'overlays) new-start (1+ new-end))))

(defun doc-svg-embed-base64 (svg data image-type &rest args)
  "Insert IMAGE into the SVG structure.
IMAGE should be a file name if DATAP is nil, and a binary string
otherwise.  IMAGE-TYPE should be a MIME image type, like
\"image/jpeg\" or the like."
  (svg--append
   svg
   (dom-node
    'image
    `((xlink:href . ,(concat "data:" image-type ";base64," data))
      ,@(svg--arguments svg args)))))

(defun doc-scroll-display-image (overlay data &optional svg)
  (let ((image (if svg
		   (let* ((size (overlay-get overlay 'size))
			  (svg (svg-create (car size) (cdr size))))
		     (doc-svg-embed-base64 svg data "image/png")
		     (svg-rectangle svg 0 0 200 200 :fill "red")
		     (svg-image svg))
		 (create-image data 'png t))))
    (overlay-put overlay 'display image)
    (when svg
      (overlay-put overlay 'data data))))

(defun doc-scroll-redisplay-svg (page &optional rectangle)
  (let* ((overlay (doc-scroll-page-overlay page))
	 (data (overlay-get overlay 'data))
	 (image (let* ((size (overlay-get overlay 'size))
		       (svg (svg-create (car size) (cdr size))))
		  (doc-svg-embed-base64 svg data "image/png")
		  (when doc-scroll-cursor-mode
		    (apply #'svg-rectangle
			   `(,svg ,@rectangle
				  :fill "red" :opacity 0.5)))
		  (svg-image svg))))
    (overlay-put overlay 'display image)))

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

(defun doc-scroll-cursor-element (&optional index)
  (let ((structured-text (nth (1- (car index))
			      doc-scroll-structured-contents)))
    (seq-find (lambda (e)
		(equal (or (cdr index) (cdr (image-mode-window-get 'cursor)))
		       (last e 3)))
	      structured-text)))

(defun doc-scroll-cursor-svg-coords (index)
  (let* ((text-element (doc-scroll-cursor-element index))
	 normalized-coords)
    (if (not text-element)
	nil
      (setq normalized-coords (doc-scroll-coords-normalize (seq-take text-element 4)
							   (nth (1- (car index)) doc-scroll-internal-page-sizes)))
      (doc-scroll-coords-denormalize normalized-coords
				     (overlay-get (doc-scroll-current-overlay) 'size)
				     'svg))))

  ;; (defmacro image-roll-page-overlay-get (page prop)
;;   "Get overlay-property PROP of overlay holding page number PAGE.
;; Implemented as macro to make it setf'able."
;;   `(overlay-get (nth (1- ,page) (image-roll-overlays))
;;                 ,prop))
(defun doc-scroll-increase-index (index type)
  (let ((n (pcase type ('page 0) ('block 1) ('line 2) ('word 3))))
    (cl-incf (nth n index))
    (let ((e (doc-scroll-cursor-element index)))
      (if (and e (cl-every (apply-partially #'<= 0) (seq-take e 4)))
	  index
	(if (eq type 'page)
	    (message "No next unit")
	  (let ((i 3))
	    (while (>= i n)
	      (setf (nth i index) 0)
	      (setq i (1- i))))
	  (doc-scroll-increase-index index (pcase type
					     ('word 'line)
					     ('line 'block)
					     ('block 'page))))))))
  
(defun doc-scroll-cursor-next-unit (type)
  (let* ((index (image-mode-window-get 'cursor))
	 (page (car index))
	 (structured-text (nth (1- page) doc-scroll-structured-contents)))
    (doc-scroll-increase-index index type)
    (image-mode-window-put 'cursor index)
    (doc-scroll-redisplay-svg page (doc-scroll-cursor-svg-coords index))))

(defun doc-scroll-cursor-next-line ()
  (interactive)
  (doc-scroll-cursor-next-unit 'word))

(define-minor-mode doc-scroll-cursor-mode
  "Display and edit by cursor"
  :lighter "DC"
  :keymap '(("a" . doc-scroll-cursor-next-line))
  (let* ((page (doc-scroll-current-page))
	 (index (list page 0 0 0))
	 (svg-coords (or (doc-scroll-cursor-svg-coords index)
			 (doc-scroll-cursor-svg-coords (doc-scroll-increase-index index 'word)))))
    (image-mode-window-put 'cursor index)
    (doc-scroll-redisplay-svg page svg-coords)))

(defun doc-scroll-add-annot (page edges style &optional display)
  (let* ((o (doc-scroll-page-overlay page))
	 (base64-data (doc-pymupdf-epc-add-annot page edges style display (car (overlay-get o 'size)))))
    (base64-decode-string base64-data)))
    ;; (doc-scroll-display-image o data)))

(defun doc-scroll-display-page (overlay &optional async svg)
  ;; (ldbg "EPC")
  (when (numberp overlay)
    (setq overlay (doc-scroll-page-overlay overlay)))
  (let* ((size (overlay-get overlay 'size))
	 (data (funcall (if async #'epc:call-deferred #'epc:call-sync)
			doc-pymupdf-epc-server
			'renderpage_base64
			(list (1+ (overlay-get overlay 'i))
			      (car size))))
	 (display (lambda (x)
		    (doc-scroll-display-image overlay
					      ;; (base64-decode-string x)
					      x
					      t))))
    (if async
	(deferred:$ data
		    (deferred:nextc it display))
      (funcall display data))))

(defun doc-scroll-undisplay-page (overlay)
  (let ((size (overlay-get overlay 'size)))
    (overlay-put overlay 'display `(space . (:width (,(car size)) :height (,(cdr size)))))))

;; The buffer-locally defined functions get called for each window
(defun doc-scroll-redisplay (&optional force)
  (ldbg "WINDOW CONFIGURATION CHANGE (redisplay)")
  (when (or force
            (/= (or (image-mode-window-get 'win-width) -1)
                (window-pixel-width)))
    (image-mode-window-put 'win-width (window-pixel-width))
    (let* ((w (doc-scroll-overlay-base-width (image-mode-window-get 'columns) 0))
           (h (doc-scroll-overlay-base-height w))
           (overlays (image-mode-window-get 'overlays)))
      (dolist (o overlays)
        (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (overlay-put o 'size (cons w h)))
      ;; NOTE this seems not required when a non-nil UPDATE argument is passes
      ;; to the `window-end' function (however, the outcommenting might lead
      ;; errors)
      ;; (redisplay)
      (dolist (o (doc-scroll-visible-overlays))
        (doc-scroll-display-page o t)))))

;; The `image-mode-new-window-functions' are called from `image-mode-winprops'
;; (with the single argument `winprops'). To obtain the winprops for the correct
;; window (e.g. when using `(car winprops)' below), `image-mode-winprops' should
;; somehow (i.e. directly or indirectly) get called from the
;; `window-configuration-change-hook'.
(defun doc-scroll-new-window-function (winprops)
  ;; (ldbg "NEW WINDOW")
  ;; (ldbg (car winprops))
  ;; (ldbg  image-mode-winprops-alist)
  (if (not (overlays-at 1))
      (let ((inhibit-read-only t)
            overlays) ; required because we derive mode (inherit) from
                                        ; `special-mode'

        (erase-buffer)
        (setq overlays (doc-scroll-create-overlays doc-scroll-number-of-pages
                                                   nil nil nil
                                                   (make-string 120 (string-to-char " "))
                                                   'window (car winprops)))
        (image-mode-window-put 'overlays overlays)
        (image-mode-window-put 'columns 1))

    ;; To independently update overlays in different windows, we create "window
    ;; local" overlays using the overlay `window' property (see `Overlay
    ;; Properties' documentation), and store them as winprops to enable easy
    ;; access.
    ;; When splitting some window, the new window inherits the winprops.
    ;; Therefore, we can just use (image-mode-window-get 'overlays) to retrieve
    ;; the "original" overlays (from the new window).
    (let ((overlays (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (image-mode-window-get 'overlays))))
      (image-mode-window-put 'overlays overlays winprops)
      (when (image-mode-window-get 'win-width)
        (doc-scroll-redisplay 'force))

      (goto-char (point-min)))))

(defun doc-scroll-next-unit (n &optional previous)
  (interactive "p")
  (let* ((current (overlay-get (doc-scroll-current-overlay) 'i))
         (next (nth (+ current (if previous (- n) n)) (image-mode-window-get 'overlays)))
         (before (doc-scroll-visible-overlays))
         after)
    (goto-char (overlay-start next))))

(defmacro doc-scroll-overlays (&optional winprops)
  "List of overlays that make up a scroll.
Setf-able function."
  `(image-mode-window-get 'overlays ,winprops))

(defun doc-scroll-overlay-size (page)
  "List of overlays that make up a scroll."
  (overlay-get (nth (1- page) (doc-scroll-overlays)) 'size))

(defun doc-scroll-vscroll-to-pscroll (&optional vscroll)
  "Scroll in units of page size."
  (/ (float (or vscroll (window-vscroll nil t)))
     (cdr (doc-scroll-overlay-size (doc-scroll-current-page)))))

(defun doc-scroll-pscroll-to-vscroll (pscroll &optional page)
  (* pscroll
     (cdr (doc-scroll-overlay-size (if page
                                       (cdr (doc-scroll-overlay-size page))
                                     (doc-scroll-current-page))))))

(defun doc-scroll--forward (&optional n row)
  "Scroll forward N units.
Default unit is pixels. If ROW is non-nil then unit is row, which
is equivalent to page if the value of `doc-scroll-columns` is 1.
If N is nil, the value of `doc-scroll-step-size` is used."
  (let ((old-vscroll (window-vscroll nil t))
	(old-overlays (doc-scroll-visible-overlays)))

    (if row
	(let ((pscroll (doc-scroll-vscroll-to-pscroll)))
	  (if (= (doc-scroll-current-page) (length (image-mode-window-get 'overlays)))
	      (message "End of buffer")
	    (forward-line n)
	    (image-set-window-vscroll old-vscroll)))

      (let ((new-vscroll (+ old-vscroll (or n doc-scroll-step-size)))
            (current-overlay-height (doc-scroll-current-overlay-height)))
	(cond ((> (ldbg  new-vscroll) (ldbg  current-overlay-height))
	       (forward-line)
               (set-window-vscroll nil (floor (- new-vscroll current-overlay-height)) t))
              (t (if (and (= (doc-scroll-current-page) (length (image-mode-window-get 'overlays)))
			  (> (+ new-vscroll (window-text-height nil t)) (doc-scroll-current-overlay-height)))
                     (message "End of buffer")
		   (image-set-window-vscroll new-vscroll))))))

    (redisplay)
    (let ((new-overlays (doc-scroll-visible-overlays)))
      (dolist (o (seq-difference old-overlays new-overlays))
	(doc-scroll-undisplay-page o))
      (dolist (o (seq-difference new-overlays old-overlays))
	(doc-scroll-display-page o t)))))
      
(defun doc-scroll--backward (&optional n row)
  (let ((old-vscroll (window-vscroll nil t))
	(old-overlays (doc-scroll-visible-overlays)))

    (if row
	(let ((pscroll (doc-scroll-vscroll-to-pscroll)))
	  (if (= (doc-scroll-current-page) 1)
              (progn (image-set-window-vscroll 0)
		     (message "Beginning of buffer"))
	    (forward-line (- (or n 1)))
	    (image-set-window-vscroll old-vscroll)))

      (let ((new-vscroll (- old-vscroll (or n doc-scroll-step-size)))
            (current-overlay-height (doc-scroll-current-overlay-height)))
	(cond ((< (ldbg  new-vscroll) 0)
	       (if (= (doc-scroll-current-page) 1)
                     (message "Beginning of buffer")
		 (forward-line -1)
		 (set-window-vscroll nil (floor (- current-overlay-height old-vscroll)) t)))
              (t (image-set-window-vscroll new-vscroll)))))

    (redisplay)
    (let ((new-overlays (doc-scroll-visible-overlays)))
      (dolist (o (seq-difference old-overlays new-overlays))
	(doc-scroll-undisplay-page o))
      (dolist (o (seq-difference new-overlays old-overlays))
	(doc-scroll-display-page o t)))))

    ;; (cond ((< new-vscroll 0)
    ;;        (cond ((<= (doc-scroll-page-at-point) (doc-scroll-columns))
    ;;               (doc-scroll-set-window-fscroll 0) ;or set to vertical margin
    ;;               (message "Beginning of buffer"))
    ;;              (t
    ;;               (forward-line -1)
    ;;               (doc-scroll-set-window-fscroll (cdr (doc-scroll-current-size)))
    ;;               (doc-scroll-update)
    ;;               (run-hooks 'doc-scroll-after-change-page-hook))))
    ;;       ((< new-vscroll (- (window-text-height nil t) (cdr (doc-scroll-current-size))))
    ;;        (doc-scroll-set-window-fscroll new-vscroll)
    ;;        (doc-scroll-update))
    ;;       (t (doc-scroll-set-window-fscroll new-vscroll)))))

      
(defun doc-scroll-forward (n)
  (interactive "p")
  (dotimes (_ n)
    (doc-scroll--forward)))

(defun doc-scroll-backward (n)
  (interactive "p")
  (dotimes (_ n)
    (doc-scroll--backward)))

(defun doc-scroll-screen-forward (n)
  (interactive "p")
  (dotimes (_ n)
    (doc-scroll--forward (window-text-height nil t))))

(defun doc-scroll-next-page (n)
  (interactive "p")
  (doc-scroll--forward n t))

(defun doc-scroll-previous-page (n)
  (interactive "p")
  (doc-scroll--backward n t))
