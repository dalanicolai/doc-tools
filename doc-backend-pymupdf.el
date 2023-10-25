;; -*- lexical-binding: t; -*-

(load-file "/home/dalanicolai/git/doc-tools/doc-scroll.el")
(load-file "/home/dalanicolai/git/doc-tools-mupdf/doc-mupdf.el")
(load-file "/home/dalanicolai/git/doc-tools-pymupdf-0.1/doc-tools-pymupdf.el")
(load-file "/home/dalanicolai/git/doc-tools-poppler/doc-poppler.el")

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

(define-derived-mode doc-backend-pymupdf-mode special-mode "DS-PyMuPDF"
  (let ((default-directory "/home/dalanicolai/git/doc-tools-pymupdf-0.1"))
    (setq doc-pymupdf-epc-server (epc:start-epc "python" '("doc-pymupdf-epc-server.py"))))
  (doc-pymupdf-epc-init)
  (add-hook 'kill-buffer-hook #'doc-backend-pymupdf-kill-server nil t)

  (doc-mupdf-create-pages doc-scroll-overlay-width)

  (doc-scroll-minor-mode)

  (setq-local doc-scroll-internal-page-sizes (doc-pymupdf-epc-page-sizes)
              doc-scroll-last-page (length doc-scroll-internal-page-sizes)
              ;; doc-scroll-structured-contents (doc-poppler-structured-contents nil nil t)

              ;; doc-scroll-display-page-function #'doc-backend-djvu-display-page
              doc-scroll-image-type 'png
              ;; doc-scroll-image-data-function #'mupdf-get-image-data
              doc-scroll-image-data-function #'doc-pymupdf-epc-page-base64-image-data
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
              doc-scroll-info-function #'doc-pymupdf-epc-info-commands))

(setq magic-mode-alist (remove '("%PDF" . pdf-view-mode) magic-mode-alist))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-backend-pymupdf-mode))

(defun doc-backend-pymupdf-structured-text (page)
  (nth (1- page) doc-scroll-structured-text))

(defun doc-backend-pymupdf-parse-contents (e)
  ;; (print e))
  (let ((coords (nth 1 e)))
    (pcase (car e)
      ('page `(0 0
                 ,(string-to-number (alist-get 'width coords))
                 ,(string-to-number (alist-get 'height coords))
                 ,@(mapcar #'pymupdf-parse-contents (nthcdr 2 e))))
      (_
       `(,(car e)
         ,@(mapcar (lambda (c)
                     (string-to-number (alist-get c coords)))
                   '(xmin ymin xmax ymax))
         ,(car (last e)))))))

;;; imenu

;; (defun pymupdf--imenu-recur ()
;;   (let ((level (caar doc-scroll-imenu-index))
;;         sublist)
;;     (while (and (cdr doc-scroll-imenu-index)
;;                 (>= (car (nth 1 doc-scroll-imenu-index)) level))
;;       (let* ((e (car doc-scroll-imenu-index))
;;              (title (nth 1 e))
;;              (page (nth 2 e)))
;;         (cond ((= (car (nth 1 doc-scroll-imenu-index)) level)
;;                (push (cons title page) sublist)
;;                (pop doc-scroll-imenu-index))
;;               ((> (car (nth 1 doc-scroll-imenu-index)) level)
;;                (pop doc-scroll-imenu-index)
;;                (push (append (list title) (pymupdf--imenu-recur))
;;                      sublist)))))
;;     (when (= (car (car doc-scroll-imenu-index)) level)
;;       (let ((e (car doc-scroll-imenu-index)))
;;         (push (cons (nth 1 e) (nth 2 e)) sublist)))
;;     (when (and (cdr doc-scroll-imenu-index)
;;                (<= (- level (car (nth 1 doc-scroll-imenu-index))) 1))
;;       (pop doc-scroll-imenu-index))
;;     (nreverse sublist)))

(defun doc-backend-pymupdf--imenu-create-index ()
  (pymupdf--imenu-parse-outline (doc-pymupdf-toc)))
  ;; (setq doc-scroll-imenu-index (doc-pymupdf-toc))
  ;; (pymupdf--imenu-recur))

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

;;; annots

(defun doc-backend-pymupdf-word-at-point (page-contents start-point end-point)
  (let ((text (doc-pymupdf-page-structured-text 1 'blocks))
        (i 0))
    (while (> (cdr start-point) (nth 3 (nth i text)))
      (setq i (1+ i)))
    (while (> (car start-point) (nth 2 (nth i text)))
      (setq i (1+ i)))
    (print (nth i text)))) ;NOTE posn-area only does not work for

;; NOTE for use with doc-toc
(defun doc-backend-pymupdf-extract-blocks (pred &optional page)
  (mapcan (lambda (p)
            (let* ((contents (doc-pymupdf-page-structured-text p 'blocks))
                   (lines (seq-filter pred contents)))
              (mapcar (lambda (l) (cons (nth 4 l) p)) (cdr lines))))
         (or page (number-sequence 1 doc-scroll-last-page))))

;; (pp (pymupdf--imenu-parse-outline doc-scroll-imenu-index))

