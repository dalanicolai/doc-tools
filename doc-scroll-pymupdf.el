;;; -*- lexical-binding: t; -*-

(require 'image-mode)

;; (load-file "/home/dalanicolai/git/doc-tools/doc-scroll.el")
(load-file "/home/dalanicolai/git/doc-tools-pymupdf/doc-pymupdf-epc.el")

(defvar doc-scroll-incompatible-modes '(visual-line-mode
                                        global-hl-line-mode))

(defvar-local doc-scroll-step-size 80)

(cl-pushnew (cons "\\.pdf\\'" 'doc-scroll-mode) auto-mode-alist)

(let ((counter 0))
  (defun ladebug (&rest args)
    (setq counter (1+ counter))
    (apply #'lwarn 'doc-scroll :debug
           (concat
            (number-to-string counter)
            " "
            (number-to-string (minibuffer-depth))
            (propertize (apply #'concat (make-list (1- (length args)) " %s"))
                        'face '(foreground-color . "red"))
            " %s")
           args)
    (car (last args))))
    ;; (print (format "%s %s" message args) #'external-debugging-output)
    ;; message)

(defun doc-pymupdf-kill-server ()
  (ladebug "STOP EPC")
  (epc:stop-epc doc-pymupdf-epc-server))

(setq doc-scroll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'doc-scroll-scroll-forward)
        (define-key map (kbd "<down>") 'doc-scroll-scroll-forward)
        (define-key map (kbd "C-p") 'doc-scroll-scroll-backward)
        (define-key map (kbd "<up>") 'doc-scroll-scroll-backward)
        (define-key map (kbd "<wheel-down>") 'doc-scroll--scroll-forward)
        (define-key map (kbd "<wheel-up>") 'doc-scroll-scroll-backward)
        ;; (define-key map (kbd "<mouse-5>") 'doc-scroll--scroll-forward)
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
  (image-mode-winprops))

(defun doc-scroll-create-overlays (number
                                   &optional columns hspace vspace text
                                   &rest overlay-props)
  (setq columns (or columns 1))
  (dolist (m doc-scroll-incompatible-modes)
    (funcall m -1))
  (toggle-truncate-lines 1) ; also disables visual-mode
  (let (overlays)
    (ladebug "OVERLAYS")
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
  (let* ((visible (overlays-in (window-start) (ladebug (window-end nil t))))
         (start (apply #'min (mapcar (lambda (o) (overlay-get o 'i)) visible)))
         (end (apply #'max (mapcar (lambda (o) (overlay-get o 'i)) visible)))
         ;; include previous/next rows for 'smoother' displaying
         (new-start (ldbg "start" (max (- start (image-mode-window-get 'columns)) 0)))
         (new-end (ldbg "end" (min (+ end (image-mode-window-get 'columns)) (1- doc-scroll-number-of-pages)))))
         ;; start and end should be limited to index start/end page
    ;; (seq-subseq overlays (max new-start 0) (1+ (min new-end (length overlays))))))
    (ldbg (seq-subseq (image-mode-window-get 'overlays) new-start (1+ new-end)))))

(defun doc-scroll-display-image (overlay data)
  (overlay-put overlay 'display (create-image data 'png t)))

(defun doc-scroll-add-annot (page edges style &optional display)
  (let* ((o (doc-scroll-page-overlay page))
	 (base64-data (doc-pymupdf-epc-add-annot page edges style display (car (overlay-get o 'size)))))
    (base64-decode-string base64-data)))
    ;; (doc-scroll-display-image o data)))

(defun doc-scroll-display-page (overlay &optional async)
  (ladebug "EPC")
  (let ((data (funcall (if async #'epc:call-deferred #'epc:call-sync)
		       doc-pymupdf-epc-server
		       'renderpage_data
		       (list (1+ (overlay-get overlay 'i))
			     (car (overlay-get overlay 'size)))))
	(display (lambda (x)
		   (doc-scroll-display-image overlay
					     (base64-decode-string x)))))
    (if async
	(deferred:$ data
		    (deferred:nextc it display))
      (funcall display data))))

(defun doc-scroll-undisplay-page (overlay)
  (let ((size (overlay-get overlay 'size)))
    (overlay-put overlay 'display `(space . (:width (,(car size)) :height (,(cdr size)))))))

;; The buffer-locally defined functions get called for each window
(defun doc-scroll-redisplay (&optional force)
  (ladebug "WINDOW CONFIGURATION CHANGE")
  (when (or force
            (/= (or (image-mode-window-get 'win-width) -1)
                (window-pixel-width)))
    (image-mode-window-put 'win-width (window-pixel-width))
    (let* ((w (doc-scroll-overlay-base-width (image-mode-window-get 'columns) 0))
           (h (doc-scroll-overlay-base-height w))
           (overlays (ladebug (image-mode-window-get 'overlays))))
      (dolist (o overlays)
        (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (overlay-put o 'size (ladebug (cons w h))))
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
  (ladebug "NEW WINDOW")
  ;; (ladebug (car winprops))
  ;; (ladebug  image-mode-winprops-alist)
  (if (not (overlays-at 1))
      (let ((inhibit-read-only t)
            overlays) ; required because we derive mode (inherit) from
                                        ; `special-mode'

        (erase-buffer)
        (setq overlays (doc-scroll-create-overlays doc-scroll-number-of-pages
                                                   nil nil nil
                                                   (make-string 120 (string-to-char " "))
                                                   'window (ladebug (car winprops))))
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

(defun doc-scroll--scroll-forward (&optional screen)
  (let ((new-vscroll (+ (window-vscroll nil t) (if screen
                                                   (window-text-height nil t)
                                                 doc-scroll-step-size)))
        (current-overlay-height (doc-scroll-current-overlay-height))
        (before (doc-scroll-visible-overlays))
        after)
    (cond ((> new-vscroll current-overlay-height)
           (doc-scroll-next-unit (image-mode-window-get 'columns))
           (recenter 0)
           (redisplay)
           (set-window-vscroll nil (floor (- new-vscroll current-overlay-height)) t))
           ;; (run-hooks 'doc-scroll-after-change-page-hook))
          ;; ((> (+ new-vscroll (window-text-height nil t)) (cdr (doc-scroll-current-size)))
          ;;  (cond ((and (= (doc-scroll-columns) 1) (= (doc-scroll-page-at-point) doc-scroll-last-page))
          ;;         (message "End of buffer"))
          ;;        (t
          ;;         (doc-scroll-set-window-fscroll new-vscroll)
          ;;         (doc-scroll-update))))
          (t (if (and (= (doc-scroll-current-page) (length (image-mode-window-get 'overlays)))
                  (> (+ new-vscroll (window-text-height nil t)) (doc-scroll-current-overlay-height)))
                 (message "End of buffer")
               (set-window-vscroll nil new-vscroll t))))
    (redisplay)
    (setq after (doc-scroll-visible-overlays))
    (dolist (o (seq-difference before after))
      (doc-scroll-undisplay-page o))
    (dolist (o (seq-difference after before))
      (doc-scroll-display-page o t))))

(defun doc-scroll-scroll-forward (n &optional previous)
  (interactive "p")
  (dotimes (i n)
    (if previous
        (doc-scroll--scroll-backward)
      (doc-scroll--scroll-forward))))
