;; -*- lexical-binding: t; -*-

(load-file "/home/dalanicolai/git/doc-tools-mupdf/doc-mupdf.el")
(load-file "/home/dalanicolai/git/doc-tools-poppler/doc-poppler.el")

;; TODO move to doc-scroll.el and only import doc-scroll
;; (load-file "/home/dalanicolai/programming/elisp/doc-tools/doc-image-roll.el")
(load-file "/home/dalanicolai/programming/elisp/doc-tools/doc-scroll.el")


(define-derived-mode doc-scroll-mupdf-mode special-mode "Doc-MuPDF "
  (doc-mupdf-create-pages 850)
  (image-roll-mode)

  (setq-local doc-scroll-length (doc-poppler-number-of-pages)
							doc-scroll-page-sizes (doc-poppler-page-sizes nil image-roll-length)

							image-roll-images
							(cddr (directory-files
										 "/tmp/doc-tools/Peter Seibel - Practical Common Lisp-Apress (2005)/pages"
										 t))


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
              doc-scroll-info-function #'doc-mupdf-info))

(setq magic-mode-alist (remove '("%PDF" . pdf-view-mode) magic-mode-alist))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-scroll-mupdf-mode))
