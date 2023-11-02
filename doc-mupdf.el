;; -*- lexical-binding: t; -*-
(defvar doc-mupdf-info-commands '(doc-mupdf-page-sizes
                              doc-mupdf-structured-contents))

(defun doc-mupdf-info (function &optional arg)
  (interactive (if (member (file-name-extension (buffer-file-name))
                           '("pdf" "epub"))
                   (list (intern-soft (completing-read "Select info type: "
                                                       doc-mupdf-info-commands))
                         current-prefix-arg)
                 (user-error "Buffer file not of `pdf' or `epub' type")))
  (pp (pcase function
        ('doc-mupdf-structured-contents (call-interactively #'doc-mupdf-structured-contents))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*doc-mupdf-info*")))
  (when arg (pop-to-buffer "*doc-mupdf-info*")))

(defun doc-mupdf-page-sizes (&optional file)
  (setq file (or file buffer-file-name))
  (pcase (file-name-extension file)

    ("epub" (let ((outdir (concat "/tmp/doc-tools/"
                                  (file-name-as-directory (file-name-base file))
                                  "thumbs/")))
              (unless (file-exists-p outdir)
                (doc-mupdf-create-thumbs))
              (let* ((size-string (cadr (split-string
                                         (car (process-lines "file"
                                                             (concat outdir "thumb1.png")))
                                         "\\(data, \\|, [0-9]*-bit\\)")))
                     (size-list (mapcar #'string-to-number (split-string size-string " x "))))
                (make-list (length (directory-files outdir))
                           (cons (car size-list) (cadr size-list))))))

    ("pdf" (let* ((lines (process-lines "mutool"
                                        "pages"
                                        buffer-file-name))
                  sizes)
             (dolist (l lines)
               (when (string-match-p "^<MediaBox" l)
                 (let ((size (mapcar (lambda (c)
                                       (string-to-number
                                        (string-replace "\"" ""
                                                        (nth 1 (split-string c "=")))))
                                     (seq-subseq (split-string l) 3 5))))
                   (push (cons (nth 0 size) (nth 1 size))
                         sizes))))
             sizes))))


;;      (let* ((lines (process-lines "mutool" "info" "-M"
;;                                         file))
;;                   (split-lines (mapcar (lambda (l)
;;                                              (split-string l " " t))
;;                                            lines))
;;                   (pages (or (string-to-number (cadr (nth 5 split-lines)))
;;                              (user-error "Output does not have 11 lines;
;; the function `doc-mupdf-page-sizes' should get modified/generalized")))
;;                   (box-line (nth 9 split-lines)))
;;              (make-list pages (cons (string-to-number (nth 5 box-line))
;;                                     (string-to-number (nth 6 box-line))))))))

(defun doc-mupdf-get-image-data (page width &optional file)
  (setq file (or file (buffer-file-name)))
  (call-process "mutool" nil nil nil
                "draw"
                "-o" "/tmp/pdf-temp-img"
                "-w" (number-to-string width)
                file
                (number-to-string page))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq coding-system-for-read 'binary)
    (insert-file-contents-literally "/tmp/pdf-temp-img")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun doc-mupdf-create-pages (width &optional file force)
  (setq file (or file buffer-file-name))
  (let* ((outdir (concat "/tmp/doc-tools/" (file-name-as-directory (file-name-base file)) "pages/"))
				 (files (when (file-exists-p outdir) (directory-files outdir nil "\\.png$"))))
    (when (or (/= (length files) (length doc-scroll-page-sizes))  force)
      (unless (file-exists-p outdir)
        (make-directory outdir t))
      (let ((proc (start-process "mutool" "mutool create page files" "mutool"
                                 "draw"
                                 "-o" (concat outdir "page-%d.png")
                                 "-w" (number-to-string width)
                                 file)))
        (set-process-sentinel proc (lambda (process event)
                                     (message "Create pages process %s" event)))))))

(defun doc-mupdf-create-thumbs (&optional file force)
  (setq file (or file buffer-file-name))
  (let ((outdir (concat "/tmp/doc-tools/"
                        (file-name-as-directory (file-name-base file))
                        "thumbs/")))
    (when (or (not (file-exists-p outdir)) force)
      (unless (file-exists-p outdir)
        (make-directory outdir t))
      (let ((proc (start-process "mutool" "mutool create thumbs" "mutool"
                                 "draw"
                                 "-o" (concat outdir "thumb%d.png")
                                 "-w" "200"
                                 file)))
        (set-process-sentinel proc (lambda (process event)
                                     (message "Create pages process %s" event)))))))


(defun doc-mupdf-parse-coords (coords-string)
  (mapcar #'string-to-number (split-string coords-string)))

(defun doc-mupdf-parse-line (line-contents)
  (append (doc-mupdf-parse-coords (alist-get 'bbox (nth 1 line-contents)))
          (list (mapcan (lambda (e)
                          (mapconcat (lambda (c)
                                       (alist-get 'c (nth 1 c)))
                                     (nthcdr 2 e)))
                        (nthcdr 2 line-contents)))))

(defun doc-mupdf-structured-contents (&optional page detail file)
  (interactive "nEnter page number: ")
  (setq file (or file buffer-file-name))
  (let (text)
    (with-temp-buffer
      (apply #'call-process "mutool" nil t nil
               "draw" "-F" "stext" file
               (when page (list (number-to-string page))))
               ;; (when pages (list (mapconcat #'number-to-string pages ","))))
      (goto-char (point-min))
      (flush-lines "^warning")
      (setq text (libxml-parse-xml-region)))
    text))
    ;; (when detail ;i.e. page or more detail
    ;;   (setq text (nthcdr 2 text)))
    ;; (when (memq detail '(block line char))
    ;;   (setq text (mapcan (apply-partially #'nthcdr 2) text)))
    ;; (when (memq detail '(line char))
    ;;   (setq text (mapcan (apply-partially #'nthcdr 2) text)))
    ;; (when (eq detail 'char)
    ;;   (setq text (mapcan (apply-partially #'nthcdr 2)
    ;;                      (mapcan (apply-partially #'nthcdr 2) text))))
    ;; (mapcar #'doc-mupdf-parse-line text)))

(defun doc-mupdf-outline (&optional file)
  (setq file (or file (buffer-file-name)))
  (mapcar (lambda (l)
            (let* ((parts (split-string l "\\(#page=\\|&zoom\\)"))
                   (p1 (split-string (car parts) "\""))
                   (level (- (length (car p1)) 2))
                   (title (string-join (nbutlast (cdr p1)))))
              (cons level (cons title (string-to-number (nth 1 parts))))))
          (process-lines "mutool" "show" file "outline")))

  ;; (with-current-buffer (get-buffer-create "*pdf-outline*")
  ;;   (call-process "mutool" nil t nil "show" file "outline")
  ;;   (while (not (bobp))
  ;;     (forward-line -1)
  ;;     (delete-char 2))

  ;;   (save-excursion
  ;;     (search-forward-regexp "^[[:space:]]*\""))
  ;;   (let ((current-level (length (match-string 0))))
  ;;     (insert "((")
  ;;     (forward-line)
  ;;     (while (not (eobp))
  ;;       (save-excursion
  ;;         (search-forward-regexp "^[[:space:]]*\""))
  ;;       (let ((new-level (length (match-string 0))))
  ;;         (cond ((= new-level current-level)
  ;;                (insert ")("))
  ;;               ((> new-level current-level)
  ;;                (insert "("))
  ;;               (t
  ;;                (dotimes (_ (1+ (- current-level new-level)))
  ;;                        (insert ")"))
  ;;                (insert "(")))
  ;;         (setq current-level new-level)
  ;;         (forward-line)))
  ;;     (dotimes (_ (1+ current-level))
  ;;       (insert ")")))))

