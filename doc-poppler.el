(defvar doc-poppler-info-commands '(doc-poppler-page-sizes
                              doc-poppler-structured-contents))

(defun doc-poppler-info (function &optional arg)
  (interactive (if (member (file-name-extension (buffer-file-name))
                           '("pdf" "epub"))
                   (list (intern-soft (completing-read "Select info type: "
                                                       doc-poppler-info-commands))
                         current-prefix-arg)
                 (user-error "Buffer file not of `pdf' or `epub' type")))
  (pp (pcase function
        ('doc-poppler-structured-contents (call-interactively #'doc-poppler-structured-contents))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*doc-poppler-info*")))
  (when arg (pop-to-buffer "*doc-poppler-info*")))

(defun doc-poppler-structured-contents (&optional page stop word)
    (interactive "nEnter page number: ")
    (let ((file-path (buffer-file-name)))
      (with-temp-buffer
	(shell-command (concat "pdftotext "
                               ;; (concat "pdftotext "
                               (when page
				 (format "-f %s -l %s " page (or stop page)))
                               (if word
                                   "-bbox "
				 "-bbox-layout ")
                               "-q " ;don't print errors
                               (shell-quote-argument file-path)
                               " -")
                       t)
	(let-alist (libxml-parse-html-region (point-min) (point-max))
          (cdr .body.doc)))))

(defun doc-poppler-structural-filter (fn hidden-text-list &optional format-fn)
  (letrec ((elements nil)
           (recur (lambda (text)
                    (if (funcall fn text)
                        (push (if format-fn (funcall format-fn text n) text)
                              elements)
                      (unless (stringp (nth 2 text))
                        (mapcar (lambda (e)
                                  (funcall recur e))
                                (nthcdr 2 text))))))
           (n 0))
    (if (symbolp (car hidden-text-list))
        (funcall recur hidden-text-list)
      (dolist (p hidden-text-list)
        (setq n (1+ n))
        (funcall recur p)))
    (nreverse elements)))

(defun doc-poppler-search-word (word)
  (doc-poppler-structural-filter
   (lambda (e)
     (when (stringp (nth 2 e))
       (string-match word (nth 2 e))))
   doc-scroll-structured-contents
   (lambda (e page) (cons page
                          (append (mapcar (lambda (c)
                                            (string-to-number (cdr c)))
                                          (nth 1 e))
                                  (last e))))))

(defun doc--poppler-element-coords (element)
  (mapcar (lambda (c)
            (string-to-number (cdr c)))
          (nth 1 element)))

(defun test (text)
  (doc-poppler-structural-filter
   (lambda (e)
     (when (eq (car e) 'page)
       (nth 1 e)))
   text))

(defun doc-poppler-number-of-pages (&optional file)
  (setq file (or file buffer-file-name))
  (let ((lines (mapcar #'split-string (process-lines "pdfinfo" file))))
    (string-to-number (car (alist-get "Pages:" lines nil nil 'string=)))))

(defun doc-poppler-page-sizes (&optional file last-page)
  (setq file (or file buffer-file-name))
  (let* ((lines (mapcar #'split-string
                        (process-lines "pdfinfo"
                                       "-l" (number-to-string
                                             (or last-page doc-scroll-last-page))
                                       file)))
         page-sizes)
    (dolist (l lines)
      (when (string= "size:" (nth 2 l))
        (push (cons (string-to-number (nth 3 l))
                    (string-to-number (nth 5 l)))
              page-sizes)))
    page-sizes))

(defun doc-poppler-parse-contents (contents)
  (mapcar (lambda (e)
            (pcase-let ((`(,type ,size . ,child) e))
              (pcase type
                ('page (let ((page-size
                              (append (list 0 0)
                                      (mapcar (lambda (d)
                                                (round (string-to-number (cdr d))))
                                              size))))
                         (append (list type)
                                 page-size
                                 (apply #'append
                                        (apply #'append (doc-poppler-parse-contents child))))))
                ('flow (doc-poppler-parse-contents child))
                ('block (doc-poppler-parse-contents child))
                ('line (append (list type)
                               (mapcar (lambda (d)
                                         (round (string-to-number (cdr d))))
                                       size)
                               (doc-poppler-parse-contents child)))
                ('word (append (list type)
                               (mapcar (lambda (d)
                                         (round (string-to-number (cdr d))))
                                       size)
                               child)))))
          contents))
