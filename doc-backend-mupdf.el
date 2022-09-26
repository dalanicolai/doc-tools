;; -*- lexical-binding: t; -*-

(load-file "/home/dalanicolai/git/doc-tools/doc-scroll.el")
(load-file "/home/dalanicolai/git/doc-tools-mupdf/doc-mupdf.el")
(load-file "/home/dalanicolai/git/emacs-poppler/poppler.el")

(define-derived-mode doc-scroll-mupdf-mode special-mode "Doc-MuPDF"
  (doc-mupdf-create-pages doc-scroll-overlay-width)
  (doc-scroll-minor-mode)

  (setq-local doc-scroll-internal-page-sizes (doc-mupdf-page-sizes)
              doc-scroll-last-page (length doc-scroll-internal-page-sizes)
              ;; doc-scroll-structured-contents (poppler-structured-contents nil nil t)

               ;; doc-scroll-display-page-function #'doc-scroll-djvu-display-page
               doc-scroll-image-type 'png
               doc-scroll-image-data-function #'doc-mupdf-get-image-data

               imenu-create-index-function #'doc-scroll-mupdf--imenu-create-index
               imenu-default-goto-function (lambda (_name position &rest _rest)
                                             ;; NOTE WEIRD, the first result is
                                             ;; a number, while the other
                                             ;; results are markers
                                             (doc-scroll-goto-page (if (markerp position)
                                                                  (marker-position position)
                                                                position)))
               doc-scroll-info-function #'doc-mupdf-info))

;; (setq doc-scroll-mupdf-mode-map doc-scroll-mode-map)
(defun doc-scroll-mupdf--imenu-create-index ()
  (let ((outline (doc-mupdf-outline)))
    (with-current-buffer (get-buffer-create "*outline*")
      (erase-buffer)
      (let ((level 0))
        (insert "((" (format "%S . %d" (cadar outline) (print (cddar outline))))
        (dolist (e (cdr outline))
          (cond ((= (car e) level)
                 (insert ") (" (format "%S . %d" (nth 1 e) (print (cddr e)))))
                ((> (car e) level)
                 (while (not (looking-back "\\."))
                   (delete-char -1))
                 (delete-char -1)
                 (insert " (" (format "%S . %d" (nth 1 e) (cddr e))))
                ((< (car e) level)
                 (dotimes (_ (1+ (- level (car e))))
                   (insert ")"))
                 (insert " (" (format "%S . %d" (nth 1 e) (cddr e)))))
          (setq level (car e)))
        (dotimes (_ (+ level 2))
          (insert ")")))
      (goto-char (point-min))
      (setq test (read (current-buffer))))))

  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-scroll-mupdf-mode))
