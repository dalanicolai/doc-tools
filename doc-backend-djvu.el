;; -*- lexical-binding: t; -*-

(load (concat (file-name-directory (or load-file-name buffer-file-name))
              "doc-scroll.el")) ;; load first so that setf is known
(load "/home/dalanicolai/git/doc-tools-djvu/doc-djvu.el")

(defvar-local doc-scroll-current-rectangles nil)

(define-derived-mode doc-backend-djvu-mode special-mode "Doc-DJVU"
  (doc-djvu-decode-pages doc-scroll-overlay-width)
  (doc-scroll-minor-mode)

  (setq-local doc-scroll-internal-page-sizes (doc-djvu-page-sizes)
              doc-scroll-last-page (length doc-scroll-internal-page-sizes)
              doc-scroll-contents (doc-djvu-parse-raw-contents)
              ;; doc-scroll-contents-function #'doc-djvu-parse-raw-contents
              ;; doc-scroll-structured-contents (doc-djvu-structured-text 'char)

              doc-scroll-image-type 'tiff
              doc-scroll-image-data-function #'doc-djvu-decode-page

              imenu-create-index-function #'doc-backend-djvu--imenu-create-index

              ;; TODO because of an Emacs bug, so that Emacs always passes a
              ;; marker, jumping for nested functions don't work with normal
              ;; `imenu'. However, it works with `imenu-list'.

              imenu-default-goto-function (lambda (_name position &rest _rest)
                                            (doc-scroll-goto-page (if (markerp position)
                                                                      (marker-position position)
                                                                    position)))
              doc-scroll-info-function #'doc-djvu-info))

(add-to-list 'auto-mode-alist '("\\.djvu\\'" . doc-backend-djvu-mode))

(defun doc-backend-djvu-structured-text (&optional page)
  (if page
      (car (alist-get 'text (alist-get page doc-scroll-contents)))
    (let (text)
      (dotimes (p (caar (last doc-scroll-contents)))
        (push (car (alist-get 'text (alist-get (1+ p) doc-scroll-contents))) text))
      (nreverse text))))


(defun doc-backend-djvu-save ()
  (interactive)
  ;; (remove-overlays)
  ;; (setq image-mode-winprops-alist nil)
  (let ((file buffer-file-name)
        (dsed-file "/tmp/djvu.dsed")
        (contents doc-scroll-contents))
    (with-temp-file dsed-file
      (insert (doc-djvu-format-contents contents)))
    (call-process "djvused" nil t nil
                     file "-f" dsed-file "-s")))
    ;; (revert-buffer t t)))
    ;; (doc-scroll-new-window-function (image-mode-winprops))
    ;; (doc-scroll-update)))

;;; imenu

(defun create-imenu (data)
  (mapcar (lambda (e)
            (let* ((str (car e))
                   (page (substring (cadr e) 1))
                   (formatted-str (concat
                                   (if (<= (length str) 55)
                                       (format (if (fboundp 'imenu-list) "%-55s " "%s ") str)
                                     (format "%s... "(substring str 0 52)))
                                   page))
                   (se (nthcdr 2 e)))
              (if se
                  (cons formatted-str (create-imenu se))
                (cons formatted-str
                      (string-to-number page)))))
          (or (alist-get 'bookmarks data) (doc-scroll-debug "%s" data))))

(defun doc-backend-djvu--imenu-create-index ()
  (create-imenu (doc-djvu-bookmarks)))

;;; doc-scroll-swiper

;; TODO move to djvu.el

;; (named-let recur ((text hidden-text-list))
;;   (if (funcall fn text)
;;       (push (if format-fn (funcall format-fn text) text)
;;             elements)
;;     (unless (stringp (nth 5 text))
;;       (mapcar #'recur
;;               (nthcdr 5 text)))))

;; experimental function with functionality similar to pdf.tocgen
(defun doc-djvu-get-toc (min-line-height max-line-height)
  (interactive (list (read-number "Enter minimum line height: ")
                     (read-number "Enter maximum line height: ")))
  (let ((contents (doc-djvu-structured-text 'line)))
    (with-current-buffer (get-buffer-create "TOC")
      (let ((p 0))
        (pp (mapcar (lambda (page)
                      (setq p (1+ p))
                      (when page
                        (doc-scroll-structural-filter
                         (lambda (l)
                           (< min-line-height (- (nth 4 l) (nth 2 l)) max-line-height))
                         page
                         (lambda (e) (doc-scroll-structural-filter
                                      (lambda (line)
                                        (stringp (nth 5 line)))
                                      e
                                      (lambda (w) (insert (format "%s %d
" (nth 5 w) p))))))))
                    contents))))
    (pop-to-buffer "TOC")))

;;; annots

;; (defun doc-scroll-doc-djvu-word-at-point (point text i)
;;   (while (or  (< (cdr point) (nth 2 (nth i text)))
;;               (> (car point) (nth 3 (nth i text))))
;;     (setq i (1+ i)))
;;   i)

(defun doc-backend-djvu-get-regions (page-contents start-point end-point)
  ;; first we filter out the element in the correct column, we do not support
  ;; annotations that stretch over multiple columns.
  (doc-scroll-debug "%s %s" start-point end-point)
  (let* ((column
          (when (eq (car (nth 5 page-contents)) 'column)
            (let (columns)
              (dolist (col (nthcdr 5 page-contents))
                (let ((beg (nth 1 col))
                      (end (nth 3 col)))
                  (when (and (< beg (car start-point) end)
                             (< beg (car end-point) end))
                    (push col columns))))
              (nreverse columns))))
  ;; first prepare the structured text to only contain 'effectively lines' (if
  ;; the 5th element of some 'higher level element' is a string, then we count
  ;; it as line))
         (lines (doc-scroll-debug "%s" (doc-djvu-structural-filter
                                   (lambda (e)
                                     (or (stringp (nth 5 e))
                                         (eq (caar (nthcdr 5 e)) 'word)))
                                   (or column page-contents))))
  ;; now we filter the lines/elemnts between the correct heights
         (v-region (doc-scroll-debug "%s" (let (region)
                                       (dolist (l lines)
                                         (when (and (< (nth 2 l) (cdr start-point))
                                                    (> (nth 4 l) (cdr end-point) ))
                                           (push l region)))
                                       (doc-scroll-debug "%s" (nreverse region)))))
    ;; finally we find the start and end words en set the correct regions
         (start-word (let ((first-line (car v-region)))
                       (if (stringp (nth 5 first-line))
                           first-line
                         (let* ((words (nthcdr 5 first-line))
                                (word (car words)))
                           (while (> (car start-point) (nth 3 word))
                             (pop words)
                             (setq word (car words)))
                           word))))
         (end-word (let ((last-line (car (last v-region))))
                     (if (stringp (nth 5 last-line))
                         last-line
                       (let* ((words (nthcdr 5 last-line))
                              (word (car words)))
                         (while (> (car end-point) (nth 3 word))
                           (pop words)
                           (setq word (car words)))
                         word)))))
    (if (= (length  v-region) 1)
        (let ((coords (append (seq-subseq start-word 1 3) (seq-subseq end-word 3 5)))
              (text (substring (mapconcat (lambda (w)
                                            (when (and (> (nth 3 w) (car start-point))
                                                       (< (nth 1 w) (car end-point)))
                                              (concat " " (nth 5 w))))
                                          (nthcdr 5 (car v-region))) 1)))
          (list (append coords (list text))))
      (let ((first-line (car v-region))
            (last-line (car (last v-region))))
        (append
                            ;; parse first line
                            (list (append (append (seq-subseq start-word 1 3) (seq-subseq first-line 3 5))
                                          (list (substring (mapconcat (lambda (w)
                                                                        (when (> (nth 3 w) (car start-point))
                                                                          (concat " " (nth 5 w))))
                                                                      (nthcdr 5 first-line))
                                                           1))))
                            ;; parse mid lines
                            (mapcar (lambda (l)
                                      (append (seq-subseq l 1 5)
                                              (list (mapconcat (lambda (w) (nth 5 w)) (nthcdr 5 l) " "))))
                                    (butlast (cdr v-region)))
                            ;; parse last line
                            (list (append (append (seq-subseq last-line 1 3) (seq-subseq end-word 3 5))
                                          (list (substring (mapconcat (lambda (w)
                                                                        (when (< (nth 1 w) (car end-point))
                                                                          (concat " " (nth 5 w))))
                                                                      (nthcdr 5 last-line))
                                                           1))))
                            )))))

(defun doc-djvu-structural-filter (fn hidden-text-list &optional format-fn)
  (letrec ((elements nil)
           (n 0)
           (w 0)
           (recur (lambda (text)
                    (when (stringp (nth 5 text)) ;also 'non-word' elements can
                                        ;contain strings
                      (setq w (1+ w)))
                    (if (funcall fn text)
                        (push (if format-fn (funcall format-fn text n w) text)
                              elements)
                      (unless (stringp (nth 5 text))
                        (mapcar (lambda (e)
                                  (funcall recur e))
                                (nthcdr 5 text)))))))
    (if (symbolp (car hidden-text-list))
        (funcall recur hidden-text-list)
      (dolist (p hidden-text-list)
        (setq n (1+ n))
        (funcall recur p)))
    (nreverse elements)))

;; TODO this function should replace `doc-scroll--doc-djvu-get-matches'

(defun doc-backend-djvu-get-matches (pattern contents)
  (doc-djvu-structural-filter
   (lambda (e)
     (and (eq (car e) 'word)
          (string-match pattern (nth 5 e))))
   contents))

(defun doc-scroll-swiper-format-candidate (text format-spec page coords)
  (let ((str (concat " " text)))
    (put-text-property 0 1 'swiper-line-coords coords str)
    (put-text-property 0 1 'swiper-page-number page str)
    (put-text-property 0 1 'display (format swiper--format-spec page) str)
    str))

(defun doc-scroll-swiper-candidates ()
  (let* ((swiper--width (1+ (floor (log doc-scroll-last-page 10))))
         (swiper--format-spec
          (format "%%-%dd " swiper--width))
         (p 1)
         candidates)
    (dolist (contents (doc-djvu-structured-text 'line))
      (if (stringp (nth 5 contents))
          (push (doc-scroll-swiper-format-candidate (nth 5 contents)
                                                 swiper--format-spec
                                                 p
                                                 (seq-subseq contents 0 5))
                candidates)
        (named-let recur ((text (nthcdr 5 contents)))
          (dolist (sub-contents text)
            (if (stringp (nth 5 sub-contents))
                (push (doc-scroll-swiper-format-candidate
                       (string-replace "" ""
                                       (replace-regexp-in-string "- \n" "" (nth 5 sub-contents)))
                       swiper--format-spec
                       p
                       (seq-subseq sub-contents 0 5))
                      candidates)
              (recur (nthcdr 5 sub-contents))))))
      (setq p (1+ p)))
    (nreverse candidates)))

;; (defun doc-scroll-structural-filter (fn hidden-text-list &optional format-fn)
;;   (let (elements)
;;     (named-let recur ((text hidden-text-list))
;;       (if (funcall fn text)
;;           (push (if format-fn (funcall format-fn text) text)
;;                 elements)
;;         (unless (stringp (nth 5 text))
;;           (mapcar (lambda (e) (recur e))
;;                   (nthcdr 5 text)))))
;;     (nreverse elements)))

;; ;; (defun doc-scroll-swiper ()
;; ;;   (interactive)
;; ;;   (ivy-read "Select line: "
;; ;;             (doc-scroll--search-candidates)
;; ;;             :action (lambda (c)
;; ;;                       (let* ((split-line (print (split-string (car c))))
;; ;;                              (page (string-to-number (car split-line)))
;; ;;                              (contents (nth (1- page)
;; ;;                                                  doc-scroll-line-structured-contents))
;; ;;                              (page-width (nth 3 contents))
;; ;;                              (page-height (nth 4 contents))
;; ;;                              (scale (/ (car (nth (1- page) doc-scroll-page-sizes)) (float page-width) ))
;; ;;                              (line-data  (flatten-list (doc-scroll-doc-djvu-search-page-matches (mapconcat #'identity (cdr split-line) " ") contents))))
;; ;;                         (print (nth 4 contents))
;; ;;                         (push (append (print (mapcar (lambda (d)
;; ;;                                                        (round (* scale d)))
;; ;;                                                      (doc-scroll-coords-to-svg page-height (seq-subseq line-data 1 5))))
;; ;;                                       (list :fill "green" :opacity 0.5))
;; ;;                               doc-scroll-current-rectangles)
;; ;;                         (doc-scroll-goto-page page)
;; ;;                         (print ivy-text)))))

;; (defun doc-scroll--swiper-update-fn ()
;;   (let* ((c (ivy-state-current ivy-last))
;;          (input ivy-text)
;;          (page (get-text-property 0 'swiper-page-number c))
;;          (coords (cdr (get-text-property 0 'swiper-line-coords c)))
;;          ;; (filtered-cands (ivy--filter ivy-text ivy--all-candidates))
;;          (filtered-cands ivy--old-cands)
;;          (doc-window (ivy-state-window ivy-last))
;;          matching-lines
;;          page-rectangles)
;;     (dolist (s filtered-cands)
;;       (when (eq (get-text-property 0 'swiper-page-number s) page)
;;         (push s matching-lines)))
;;     (with-current-buffer (ivy-state-buffer ivy-last)
;;       (let* ((page-size (doc-scroll-page-size page))
;;              (contents (nth (1- page)
;;                                  doc-scroll-structured-contents))
;;              (internal-size (nth (1- page) doc-scroll-internal-page-sizes))
;;              (img-coords (doc-scroll-coords-convert coords
;;                                                internal-size 'djvu
;;                                                page-size 'svg))
;;              matches)
;;         ;; get all matching elements on page
;;         (when (> (length input) 1)
;;           (let ((match-colors '("red" "blue" "green" "orange" "purple"))
;;                 (matching-lines-data
;;                  (mapcan (lambda (line-coords)
;;                            ;; (doc-scroll-doc-djvu-elements
;;                            ;;  (nth (1- page) doc-scroll-structured-contents)
;;                            ;;  '(para line)
;;                            (doc-scroll-structural-filter
;;                             (lambda (e)
;;                               (when (equal (seq-subseq e 1 5)
;;                                            (cdr (get-text-property 0 'swiper-line-coords line-coords)))
;;                                 (print (list e))))
;;                              (nth (1- page) doc-scroll-structured-contents)))
;;                          matching-lines))
;;                 matches)
;;             (dolist (in (split-string input))
;;               (dolist (ld matching-lines-data)
;;                 (let* ((pattern-coords (mapcar (lambda (m)
;;                                          (seq-subseq m 1 5))
;;                                        (doc-scroll--doc-djvu-get-matches in ld))))
;;                   (dolist (c pattern-coords)
;;                     (cl-pushnew (append c
;;                                         (list :fill (car match-colors) :opacity 0.4))
;;                                 matches :test #'equal))))
;;               (print (setq match-colors (cdr match-colors))))
;;             (with-selected-window doc-window
;;               (setf (doc-scroll-swiper-matches page) (print (append (list coords) matches))))
;;             (doc-scroll-goto-page page doc-window)
;;             (set-window-vscroll doc-window
;;                                 (max
;;                                  (- (nth 1 (doc-scroll-coords-to-svg page coords)) 200)
;;                                  0)
;;                                 t)))))))

;; (defun ivy-display-function-window (text)
;;   (let ((buffer (get-buffer-create "*ivy-candidate-window*"))
;;         (str (with-current-buffer (get-buffer-create " *Minibuf-1*")
;;                (let ((point (point))
;;                      (string (concat (buffer-string) "  " text)))
;;                  (add-face-text-property
;;                   (- point 1) point 'ivy-cursor t string)
;;                  string))))
;;     (with-current-buffer buffer
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert str)))
;;     (with-ivy-window
;;       (display-buffer
;;        buffer
;;        `((display-buffer-reuse-window
;;           display-buffer-pop-up-window)
;;          (window-height . ,(1+ (ivy--height (ivy-state-caller ivy-last)))))))))

(defun doc-scroll-swiper (&optional initial-input)
  "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (let ((ivy-height (/ (window-height) 3))
        (candidates (doc-scroll-swiper-candidates)))
    (swiper--init)
    (setq swiper-invocation-face
          (plist-get (text-properties-at (point)) 'face))
    (let ((preselect
           (if (or swiper-use-visual-line (null search-invisible))
               (count-screen-lines
                (point-min)
                (save-excursion (beginning-of-visual-line) (point)))
             (1- (line-number-at-pos))))
          (minibuffer-allow-text-properties t)
          res)
      (unwind-protect
           (and
            (setq res
                  (ivy-read
                   "Swiper: "
                   candidates
                   :initial-input initial-input
                   :keymap swiper-map
                   :preselect
                   (if initial-input
                       (cl-position-if
                        (lambda (x)
                          (<= (1+ preselect) (swiper--line-number x)))
                        (progn
                          (setq ivy--old-re nil)
                          (ivy--filter initial-input candidates)))
                     preselect)
                   :require-match t
                   ;; :action #'swiper--action
                   ;; :action (lambda (c) (print c))
                   :action (lambda (c)
                             ;; (print ivy--old-cands))
                             ;; (print "hoi"))
                             (let* ((page (get-text-property 0 'swiper-page-number c))
                                    (coords (get-text-property 0 'swiper-line-coords c))
                                    (contents (nth (1- page)
                                                   (doc-djvu-structured-text 'line)))
                                    (page-width (nth 3 contents))
                                    (page-height (nth 4 contents))
                                    (scale (/ (car (nth (1- page) (doc-scroll-desired-overlay-sizes))) (float page-width))))
                                    ;; substring because candidates has an extra space for page-number display property
                               ;;      (line-data  (doc-scroll-doc-djvu-search-page-matches (substring c 1) contents)))
                               (push (append (mapcar (lambda (d)
                                                             (round (* scale d)))
                                                     (doc-scroll-coords-to-svg page (cdr coords)))
                                             (list :fill "green" :opacity 0.5))
                                     doc-scroll-current-rectangles)
                               (doc-scroll-goto-page page)))
                               ;; (print ivy-text)))
                   :re-builder #'swiper--re-builder
                   :history 'swiper-history
                   :extra-props (list :fname (buffer-file-name))
                   :caller 'doc-scroll-swiper))
            (point))
        (unless (or res swiper-stay-on-quit)
          (goto-char swiper--opoint))
        (isearch-clean-overlays)
        (unless (or res (string= ivy-text ""))
          (cl-pushnew ivy-text swiper-history))
        (setq swiper--current-window-start nil)
        (when swiper--reveal-mode
          (reveal-mode 1))))))

(ivy-configure 'doc-scroll-swiper
  :occur #'swiper-occur
  :update-fn #'doc-scroll--swiper-update-fn
  :unwind-fn #'swiper--cleanup
  :index-fn #'ivy-recompute-index-swiper)
