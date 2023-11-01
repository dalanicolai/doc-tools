;; -*- lexical-binding: t; -*-

(setq doc-tools-dir (file-name-directory (or load-file-name
																						 buffer-file-name)))

(dolist (f '("doc-mupdf.el" "doc-tools-pymupdf.el" "doc-scroll.el"))
	(load-file (concat doc-tools-dir f)))


(defcustom doc-pymupdf-virutalenv-root nil
	"Pymupdf virtualenv"
	:group 'doc-tools)

(defvar-local doc-pymupdf-epc-server nil)

(defun doc-backend-pymupdf-image-data (page _)
  (nth (1- page) doc-scroll-page-images))

(defun doc-backend-pdf-epc-svg-embed-base64 (svg data image-type &rest args)
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

(defun doc-backend-pymupdf-kill-server ()
  (epc:stop-epc doc-pymupdf-epc-server))

;;;###autoload
(define-derived-mode doc-backend-pymupdf-mode special-mode "DS-PyMuPDF/"
  (let ((default-directory doc-tools-dir)
				(python-interpreter (if-let (venv doc-pymupdf-virutalenv-root)
																										(expand-file-name "bin/python" venv)
															"python")))
		(unless (= (call-process python-interpreter nil nil nil "-c" "'import fitz'") 0)
			(doc-pymupdf-epc-install))
		(setq doc-pymupdf-epc-server (epc:start-epc python-interpreter '("doc-pymupdf-epc-server.py"))))
  (doc-pymupdf-epc-init)
  (add-hook 'kill-buffer-hook #'doc-backend-pymupdf-kill-server nil t)

  (doc-mupdf-create-pages image-roll-overlay-max-width)

  (image-roll-mode)

  (setq-local doc-scroll-page-sizes (doc-pymupdf-epc-page-sizes)
							doc-scroll-image-file-sizes (mapcar
																					 (lambda (s)
																						 (let ((scale (/ (float image-roll-overlay-max-width) (car s))))
																							 (cons image-roll-overlay-max-width (round (* scale (cdr s))))))
																					 doc-scroll-page-sizes)
																					 
																											
              doc-scroll-length (length doc-scroll-page-sizes)

							image-roll-images
							(cddr (directory-files
										 "/tmp/doc-tools/Peter Seibel - Practical Common Lisp-Apress (2005)/pages"
										 t))

              ;; doc-scroll-image-type 'png
              ;; doc-scroll-image-data-function #'mupdf-get-image-data
              ;; doc-scroll-image-data-function #'doc-pymupdf-epc-page-base64-image-data
              ;; doc-scroll-image-data-function #'doc-backend-pymupdf-image-data

              ;; imenu-create-index-function #'doc-backend-mupdf--imenu-create-index
              imenu-create-index-function #'doc-backend-pymupdf--imenu-create-index
              imenu-default-goto-function (lambda (_name position &rest _rest)
                                            ;; NOTE VERY WEIRD, the first
                                            ;; result is a number, while the
                                            ;; other results are markers
                                            (doc-scroll-goto-page (if (markerp position)
                                                                 (marker-position position)
                                                               position)))
              ;; doc-scroll-info-function #'doc-pymupdf-epc-info-commands
							)

		(doc-pymupdf-epc-init-data))

(setq magic-mode-alist (remove '("%PDF" . pdf-view-mode) magic-mode-alist))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-backend-pymupdf-mode))

(defun doc-backend-pymupdf-structured-text (page)
	(overlay-get (image-roll-overlay page) 'text))

(defun doc-backend-pymupdf-word-index-at-point (point text)
	(seq-position text point (lambda (x y) (and (< (cdr y) (nth 3 x))
																							(< (car y) (nth 2 x))))))

(defun doc-backend-pymupdf-region-to-selection (coords text)
	(pcase-let* ((`(,x0 ,y0 ,x1 ,y1) coords)
							 (beg (doc-backend-pymupdf-word-index-at-point (cons x0 y0) text))
							 (end (doc-backend-pymupdf-word-index-at-point (cons x1 y1) text)))
		(seq-subseq text beg (1+ end))))

;;; Imenu
(defun doc-backend-pymupdf--imenu-create-index ()
  (pymupdf--imenu-parse-outline (doc-pymupdf-epc-toc)))

(defun tree-map (tree)
  (mapcar (lambda (e)
            (if-let (p (numberp (nth 1 e)))
                (cons (concat (nth 0 e) " " (number-to-string (nth 1 e)))
                      (nth 1 e))
              (cons (car e) (tree-map (cdr e)))))
          tree))

(defun pymupdf--imenu-parse-outline (outline)
  (let ((levels (list (cons 0 (list (cdar outline)))))
        (current-level 1))

    (dolist (e (cdr outline))
      (let ((ne (cdr e)))
        (cond ((= (car e) current-level)
               (when-let (ce (alist-get current-level levels))
                 (push ce (alist-get (1- current-level) levels)))
               (setf (alist-get current-level levels) ne))
              ((> (car e) current-level)
               (setf (alist-get current-level levels)
                     (reverse (butlast (alist-get current-level levels))))
               (cl-incf current-level)
               (setf (alist-get current-level levels) ne))
              (t (while (<= (car e) current-level)
                   (push (let ((l (alist-get current-level levels)))
                           (if (numberp (nth 1 l)) l (reverse l)))
                         (alist-get (1- current-level) levels))
                   (setf (alist-get current-level levels) nil)
                   (cl-decf current-level))
                 (cl-incf current-level)
                 (setf (alist-get current-level levels) ne)))))

    (while (>= current-level 1)
      (push (alist-get current-level levels)
            (alist-get (1- current-level) levels))
      (setf (alist-get current-level levels) nil)
      (cl-decf current-level))

    (tree-map (reverse (alist-get 0 levels)))))
