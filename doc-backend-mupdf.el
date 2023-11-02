;; -*- lexical-binding: t; -*-

(setq doc-tools-dir (file-name-directory (or load-file-name
																						 buffer-file-name)))

(dolist (f '("doc-mupdf.el" "doc-poppler.el" "doc-scroll.el"))
	(load-file (concat doc-tools-dir f)))

(define-derived-mode doc-scroll-mupdf-mode special-mode "Doc-MuPDF "
  (setq-local doc-scroll-length (doc-poppler-number-of-pages)
							doc-scroll-page-sizes (doc-poppler-page-sizes nil image-roll-length))

  (doc-mupdf-create-pages image-roll-overlay-max-width)
  (image-roll-mode)

  (setq-local	doc-scroll-image-file-sizes (mapcar
																					 (lambda (s)
																						 (let ((scale (/ (float image-roll-overlay-max-width) (car s))))
																							 (cons image-roll-overlay-max-width (round (* scale (cdr s))))))
																					 doc-scroll-page-sizes)

							image-dir "/tmp/doc-tools/Peter Seibel - Practical Common Lisp-Apress (2005)/pages")


							;; doc-scroll-internal-page-sizes (doc-mupdf-page-sizes)
							;; doc-scroll-last-page (length doc-scroll-internal-page-sizes)
							;; doc-scroll-structured-contents (doc-poppler-structured-contents nil nil t)

							;; doc-scroll-display-page-function #'doc-scroll-djvu-display-page
							;; doc-scroll-image-type 'png
							;; doc-scroll-image-data-function #'doc-mupdf-get-image-data

							;; imenu-create-index-function #'doc-scroll-mupdf--imenu-create-index
							;; imenu-default-goto-function (lambda (_name position &rest _rest)
							;;                               ;; NOTE WEIRD, the first result is
							;;                               ;; a number, while the other
              ;;                               ;; results are markers
              ;;                               (doc-scroll-goto-page (if (markerp position)
              ;;                                                    (marker-position position)
              ;;                                                  position)))
  ;; doc-scroll-info-function #'doc-mupdf-info
	)

(setq magic-mode-alist (remove '("%PDF" . pdf-view-mode) magic-mode-alist))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-scroll-mupdf-mode))
