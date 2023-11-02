;; (load-file "/home/dalanicolai/programming/elisp/doc-tools/doc-image-roll.el")

(load-file (concat doc-tools-dir "doc-image-roll.el"))

;; doc-scroll uses the `doc-image-roll' display engine. The code is
;; much more coherent by `adding' (i.e. aliasing) the required
;; image-roll functions into the `doc-scroll' namespace.
(defvaralias 'doc-scroll-length 'image-roll-length)
(defvaralias 'doc-scroll-page-sizes 'image-roll-image-sizes)
(defvaralias 'doc-scroll-image-file-sizes 'image-roll-image-file-sizes)
(defvaralias 'doc-scroll-x-margin 'image-roll-x-margin)
(defvaralias 'doc-scroll-y-margin 'image-roll-y-margin)

(defalias 'doc-scroll-scroll-forward 'image-roll-scroll-forward)
(defalias 'doc-scroll-scroll-backward 'image-roll-scroll-backward)
(defalias 'doc-scroll-next-page 'image-roll-next-image)
(defalias 'doc-scroll-previous-page 'image-roll-previous-image)
(defalias 'doc-scroll-scroll-screen-forward 'image-roll-scroll-screen-forward)
(defalias 'doc-scroll-scroll-screen-backward 'image-roll-scroll-screen-backward)
(defalias 'doc-scroll-goto-page 'image-roll-goto-image)
(defalias 'doc-scroll-fit-toggle 'image-roll-fit-toggle)
(defalias 'doc-scroll-set-columns 'image-roll-set-columns)

(defalias 'doc-scroll-overlay 'image-roll-overlay)
(defalias 'doc-scroll-page-size 'image-roll-image-file-size)
(defalias 'doc-scroll-image-size 'image-roll-image-size)
(defalias 'doc-scroll-image-display-size 'image-roll-image-display-size)
(defalias 'doc-scroll-desired-overlay-size 'image-roll-desired-overlay-size)
(defalias 'doc-scroll-display-page 'image-roll-display-image)
(defalias 'doc-scroll-goto-pos 'image-roll-goto-pos)
(defalias 'doc-scroll-current-page 'image-roll-current-image)
(defalias 'doc-scroll-page-at-point 'image-roll-image-at-point)
(defalias 'doc-sroll-overlay-image 'image-roll-overlay-image)
(defalias 'doc-scroll-image-file-size 'image-roll-image-file-size)

(defvar-local doc-scroll-annot-type 'line)

(defvar-local doc-scroll-search-state nil)

(defvar-local doc-scroll-active-region nil)


(setq doc-scroll-mode-map
      (let ((map (make-sparse-keymap)))
				(define-key map "T" 'doc-scroll-page-text)
				(define-key map "i" 'doc-scroll-info)
				(define-key map "y" 'doc-scroll-kill-new)
				(define-key map (kbd "C-s") 'doc-scroll-search)
				(define-key map [down-mouse-1] 'doc-scroll-select-region)
				(define-key map [text-region down-mouse-1] 'doc-scroll-select-region)
				(define-key map [S-down-mouse-1] 'doc-scroll-select-region-free)

				(define-key map (kbd "C-c C-a") 'doc-scroll-annot-mode)
        map))

(when (featurep 'evil)
  (evil-define-key 'motion doc-scroll-mode-map
    "T" 'doc-scroll-page-text
    "/" 'doc-scroll-search
    "n" 'doc-scroll-search-next
    "p" 'doc-scroll-search-previous
    "i" 'doc-scroll-info
    "y" 'doc-scroll-kill-new
    "o" 'imenu-list-smart-toggle
    [down-mouse-1] 'doc-scroll-select-region))

(define-minor-mode doc-scroll-mode 
	"DS"
  :lighter "DS/"
  :keymap doc-scroll-mode-map)

;;; Coords

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
               (dx (abs (- x1 x0)))
               (dy (abs (- y1 y0)))
               (scale-factors (list w h w h)))
    (cl-mapcar #'*
               (pcase type
                 ('djvu (list x0 (- 1 y1) x1 (- 1 y0)))
                 ('svg (list (min x0 x1) (min y0 y1) dx dy))
                 ;; ('svg-line (list x0 y1 x1 y0))
                 (_ coords))
               scale-factors)))

(defun doc-scroll-coords-convert (coords from-size from-type to-size &optional to-type)
  (mapcar #'round
          (doc-scroll-coords-denormalize
           (doc-scroll-coords-normalize coords from-size from-type)
           to-size to-type)))

(defun doc-scroll-coords-to-svg (coords from-size to-size)
  (doc-scroll-coords-convert coords from-size
                             (pcase major-mode
                               ('doc-backend-djvu-mode 'djvu)
                               ('doc-scroll-mupdf-mode 'pdf)
															 ('doc-backend-pymupdf-mode 'pdf))
                             to-size 'svg))

(defun doc-scroll-coords-scale (coords from to)
  (pcase-let* ((`(,fw . ,fh) from)
               (`(,tw . ,th) to)
							 (scale-x (/ (float tw) fw))
							 (scale-y (/ (float th) fh)))
		(list (* scale-x (nth 0 coords))
					(* scale-y (nth 1 coords))
					(* scale-x (nth 2 coords))
					(* scale-y (nth 3 coords)))))

(defun doc-scroll-coords-point-scale (page coords)
  (pcase-let* ((`(,pw . ,ph) (doc-scroll-page-size page))
               (`(,ow . ,oh) (doc-scroll-desired-overlay-size))
							 (w (- ow (* 2 doc-scroll-x-margin)))
							 (h (- oh (* 2 doc-scroll-y-margin))))
    (cons (* (/ (float pw) w) (car coords))
          (* (/ (float ph) h) (cdr coords)))))


(defun doc-scroll-coords-to-points (coords)
	(pcase-let ((`(,x0 ,y0 ,x1 ,y1) (mapcar #'round coords)))
		(cons (cons x0 y0) (cons x1 y1))))

(defun doc-scroll-hot-spots (display-coords)
	(mapcar (lambda (c)
						(list (cons 'rect (doc-scroll-coords-to-points c))
									'text-region
									'(pointer text)))
					display-coords))

;;; Search

(setq doc-scroll-search-mode-map
      (let ((map (make-sparse-keymap)))
				(kbd "<escape>") 'doc-scroll-search-abort
        map))

(when (featurep 'evil)
  (evil-define-key 'motion doc-scroll-search-mode-map
   (kbd "<escape>") 'doc-scroll-search-abort))

(define-minor-mode doc-scroll-search-mode 
	"DS"
  :lighter "DS/"
  :keymap doc-scroll-search-mode-map)

(defun doc-scroll-search (word)
  (interactive "sEnter search pattern: ")
  ;; (unless doc-scroll-contents
  ;;   (setq doc-scroll-contents (funcall doc-scroll-contents-function)))
  (let* ((results (funcall (pcase major-mode
                             ('doc-backend-djvu-mode
															#'doc-djvu-search-word)
                             ('doc-backend-pymupdf-mode
															;; #'doc-poppler-search-word)
															#'doc-pymupdf-epc-search)
                             ;; ((or 'doc-backend-pymupdf-mode 'doc-scroll-mupdf-mode)
                             ('doc-scroll-mupdf-mode
															#'doc-poppler-search-word))
                           word))
				 next)
		(cond (results
					 (doc-scroll-search-mode)
					 (setq next (seq-position results (doc-scroll-page-at-point)
																		(lambda (x y) (>= (car x) y))))
					 (setq doc-scroll-search-state (list :index next :results results))
					 (let* ((next-hit (nth next results))
									(page (car next-hit))
									(coords (cdr next-hit)))
						 (doc-scroll-goto-pos page (max (- (nth 1 coords) (/ (window-pixel-height) 2)) 0))))
					(t (message "No hits")))))

(defun doc-scroll-search-abort ()
	(interactive)
	(doc-scroll-search-mode 0))

(defun doc-scroll-search-next ()
  (interactive)
  (unless (nth (cl-incf (plist-get doc-scroll-search-state :index))
							 (plist-get doc-scroll-search-state :results))
    (setf (plist-get doc-scroll-search-state :index) 0))
  (let* ((n (plist-get doc-scroll-search-state :index))
         (m (ldbg  (nth n (plist-get doc-scroll-search-state :results))))
         (page (car m))
         ;; (coords (doc-scroll-coords-djvu-to-svg page (cl-subseq m 1 5))))
         (coords (cl-subseq m 1 5)))
    ;; first adjusting the scroll and then go to page displays smoothly
    (doc-scroll-goto-pos page (max (- (nth 1 coords) (/ (window-pixel-height) 2)) 0))))

(defun doc-scroll-search-previous ()
  (interactive)
  (when (< (cl-decf (plist-get doc-scroll-search-state :index)) 0)
    (setf (plist-get doc-scroll-search-state :index)
					(- (length (plist-get doc-scroll-search-state :results)) 1)))
  (let* ((n (plist-get doc-scroll-search-state :index))
         (m (nth n (plist-get doc-scroll-search-state :results)))
         (page (car m))
         ;; (coords (doc-scroll-coords-djvu-to-svg page (cl-subseq m 1 5))))
         (coords (cl-subseq m 1 5)))
    ;; first adjusting the scroll and then go to page displays smoothly
    (doc-scroll-goto-pos page (max (- (nth 1 coords) (/ (window-pixel-height) 2)) 0))))

(defun doc-scroll-svg-search-results (svg page)
	(let* ((index (plist-get doc-scroll-search-state :index))
				 (pos (nth index (plist-get doc-scroll-search-state :results)))
				 (coords (cdr pos))
				 (svg-coords (doc-scroll-coords-denormalize coords
																										(doc-scroll-image-file-size page)
																										'svg)))
		(apply #'svg-rectangle svg (append svg-coords
																			 (list :fill "green"
																						 :opacity 0.5))))
	svg)

;;; Select

;;;; Mouse

(setq doc-scroll-select-mode-map
      (let ((map (make-sparse-keymap)))
				(define-key map (kbd "<escape>") 'doc-scroll-select-abort)
        map))

(when (featurep 'evil)
  (evil-define-key 'motion doc-scroll-select-mode-map
		(kbd "<escape>") 'doc-scroll-select-abort
		"a" 'test))

(define-minor-mode doc-scroll-select-mode 
	"Mode active during text selection."
  :lighter "Select "
  :keymap doc-scroll-select-mode-map)

(define-minor-mode doc-scroll-free-select-mode 
	"Mode active during free selection."
  :lighter "Free "
  :keymap doc-scroll-select-mode-map)

(defun doc-scroll-select-region-free (event)
  (interactive "@e")
  (doc-scroll-select-region event t))

(defun doc-scroll-select-region (event &optional free)
  "Draw objects interactively via a mouse drag EVENT. "
  (interactive "@e")
	(doc-scroll-free-select-mode (if free 1 0))
	(doc-scroll-select-mode (if free 0 1))
  (let* ((start (event-start event))
         ;; (page (print (posn-area start)))
         (page (image-property (posn-image start) :image))
         ;; (start-point (doc-scroll-coords-point-scale page (posn-object-x-y start)))
         (start-point (posn-object-x-y start))
         (text (funcall (pcase major-mode
                          ('doc-backend-djvu-mode #'doc-backend-djvu-structured-text)
                          ('doc-backend-pymupdf-mode #'doc-backend-pymupdf-structured-text))
                        page))
         first-element
         result)
    ;; NOTE we either must pass page to the 'doc-backend-djvu-get-regions' and pdf
    ;; version functions, or we should correct the coords for djvu here, we
    ;; choose the second option. DON'T SET THIS IN THE TRACK MOUSE WHILE LOOP
    (when (eq major-mode 'doc-backend-djvu-mode)
      (setq start-point (cons (car start-point)
                              (- (cdr (doc-scroll-page-size page)) (cdr start-point)))))
    (track-mouse
      (while (not (memq (car event) '(drag-mouse-1 S-drag-mouse-1)))
        (setq event (read-event))
        (let* ((end (event-end event))
               (end-point (posn-object-x-y end)))
               ;; (end-point (doc-scroll-coords-point-scale page (posn-object-x-y end))))
          (when (eq major-mode 'doc-backend-djvu-mode)
            ;; NOTE see note above 'track-mouse' about correcting coords
            (setq end-point (cons (car end-point)
                                  (- (cdr (doc-scroll-page-size page)) (cdr end-point)))))
          ;; (doc-scroll-debug "%s %s" start-point end-point)
          ;; (doc-scroll-debug "%s"
          ;; (print "hoi")
          (setq doc-scroll-active-region
								(pcase-let* ((`(,x0 . ,y0) start-point)
                             (`(,x1 . ,y1) end-point)
														 (coords (list x0 y0 x1 y1)))
									(cons page (doc-scroll-coords-normalize coords
																													(doc-scroll-image-file-size page)))))
					;; 				(funcall (pcase major-mode
					;; 									 ('doc-backend-pymupdf-mode #'doc-backend-pymupdf-region-to-selection)
					;; 									 ('doc-backend-djvu-mode #'doc-backend-djvu-get-regions))
          ;;                  coords text)))
          (doc-scroll-display-page page))))
		(when doc-scroll-annot-mode
			(doc-scroll-add-annot))))

(defun doc-scroll-select-abort ()
	(interactive)
	(doc-scroll-select-mode 0)
	(doc-scroll-free-select-mode 0)
	(let ((page (car doc-scroll-active-region)))
		(setq doc-scroll-active-region nil)
		(doc-scroll-display-page page)))


(defun doc-scroll-active-region-text ()
  (mapconcat (lambda (e)
               (nth 4 e))
             (cdr doc-scroll-active-region)
             "\n"))

(defun doc-scroll-active-regions (regions)
  (mapcar (lambda (r) (if (nthcdr 4 r) (butlast r) r)) regions))

(defun doc-scroll-svg-active-region (svg page)
	(let* ((free doc-scroll-free-select-mode)
				 (line (eq doc-scroll-annot-type 'line))
				 (coords (cdr doc-scroll-active-region))
				 (svg-coords (doc-scroll-coords-denormalize coords
																										(doc-scroll-image-file-size page)
																										(unless (and free line) 'svg))))
		(apply (if (and free line) #'svg-line #'svg-rectangle)
					 svg (append svg-coords (if (and free line)
																						 (list :stroke "blue" :stroke-width 3)
																					 (list :fill "gray" :opacity 0.5)))))
	svg)
;; (defun doc-scroll-svg-active-region (svg page)
;; 	(let* ((coords (cdr doc-scroll-active-region)))
;; 		(dolist (c (print coords))
;; 			(let ((svg-coords (doc-scroll-coords-denormalize c
;; 																											 (doc-scroll-image-size page)
;; 																											 (unless (eq doc-scroll-annot-type 'line)
;; 																												 'svg))))
;; 				(apply (if (eq doc-scroll-annot-type 'line) #'svg-line #'svg-rectangle)
;; 							 svg (append svg-coords
;; 													 (if (eq doc-scroll-annot-type 'line)
;; 															 (list :stroke "blue"
;; 																		 :strok-width 3)
;; 														 (list :fill "gray"
;; 																	 :opacity 0.5)))))))
;; 		svg)
;;; Copy/Kill

(defun doc-scroll-kill-new ()
  (interactive)
  (kill-new (doc-scroll-active-region-text))
  (setq doc-scroll-active-region nil)
  (doc-scroll-update t))

;;; Annots

;; (setq doc-scroll-annot-mode-map
;;       (let ((map (make-sparse-keymap)))
;; 				(define-key map (kbd "<escape>") 'doc-scroll-select-abort)
;;         map))

(define-minor-mode doc-scroll-annot-mode 
	"DS"
  :lighter "DS/")
  ;; :keymap doc-scroll-annot-mode-map)

(defun doc-scroll-add-annot ()
	(interactive)
	(pcase-let ((`(,page . ,edges) doc-scroll-active-region))
		(doc-pymupdf-epc-add-annot page
															 edges
															 doc-scroll-annot-type)
		;; (set-buffer-modified-p t)
		(doc-pymupdf-epc-page-base64-image-data
		 page
		 850
		 (format (concat "/tmp/doc-tools/"
										 (file-name-as-directory
											(file-name-base buffer-file-name))
										 "pages/page-%d.png")
						 page))
		(doc-scroll-select-mode 0)
		(doc-scroll-display-page page)))

;;;; Utils

(defun doc-scroll-page-text (&optional arg)
  ;; (interactive "P")
	(overlay-get (doc-scroll-overlay 1) 'text))
  ;; (pp (doc-djvu-structured-text 'plain (if arg
  ;;                                          (read-number "Page: ")
  ;;                                        (doc-scroll-page-at-point)))
  ;;     (pop-to-buffer (get-buffer-create "*djvu-text*"))))

;; (0.46 0.5052083333333334 0.6670588235294118 0.3342013888888889)

;; ((0.19899628122610083 0.10992369525435912 0.5039742133284951 0.1404165004586575 "forums.apress.com" 0 0 0) (0.19899628122610083 0.14859331557496877 0.23871698160384638 0.1655338035523199 "FOR" 1 0 0) (0.24418618523703084 0.14859331557496877 0.4048591448069745 0.1655338035523199 "PROFESSIONALS" 1 0 1) (0.4103283784582362 0.14859331557496877 0.4365530713459288 0.1655338035523199 "BY" 1 0 2) (0.44202230499719053 0.14859331557496877 0.6221718936736456 0.1655338035523199 "PROFESSIONALS™" 1 0 3) (0.14587800263382933 0.20157287221684228 0.17845713225482657 0.2148346646006082 "JOIN" 2 0 0) (0.18283740011520797 0.20157287221684228 0.21036445730543754 0.2148346646006082 "THE" 2 0 1) (0.2147447251658189 0.20157287221684228 0.2719392485313908 0.2148346646006082 "APRESS" 2 0 2) (0.2763195163917722 0.20157287221684228 0.33921141079622585 0.2148346646006082 "FORUMS" 2 0 3) (0.3435916786566072 0.20157287221684228 0.3740933472006285 0.2148346646006082 "AND" 2 0 4) (0.37847361506100985 0.20157287221684228 0.39720270398806057 0.2148346646006082 "BE" 2 0 5) (0.401582971848442 0.20157287221684228 0.4374357829275543 0.2148346646006082 "PART" 2 0 6) ...)

;; (doc-backend-pymupdf-region-to-selection '(0.46 0.5052083333333334 0.6670588235294118 0.3342013888888889) test)

;; (length test)
