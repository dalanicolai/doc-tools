;;; Explanation

;; This buffer (partially) shows how image-mode its winprops initialization
;; mechanism works.


;;; Usage

;; Start Emacs from terminal. Debugging output is printed in terminal.

;; Evaluate buffer and "trigger" a test by creating a new buffer (find-file),
;; with extension '.t'

(cl-pushnew (cons "\\.t\\'" 'doc-test-mode) auto-mode-alist)

(defun doc-test-window-cc-hook ()
  (print "CONFIGURATION CHANGE" #'external-debugging-output))

(defun doc-test-new-window-function ()
  (print "NEW WINDOW" #'external-debugging-output))

(define-derived-mode doc-test-mode special-mode "DT"
  (add-hook 'window-configuration-change-hook 'doc-test-window-cc-hook nil t)
  (add-hook 'image-mode-new-window-functions 'doc-test-new-window-function nil t)
  ;; (setq image-mode-winprops-alist nil)
  (image-mode-winprops))

(defun image-mode-winprops (&optional window cleanup)
  "Return winprops of WINDOW.
A winprops object has the shape (WINDOW . ALIST).
WINDOW defaults to `selected-window' if it displays the current buffer, and
otherwise it defaults to t, used for times when the buffer is not displayed."
  (cond ((null window)
         (setq window
               (if (eq (print (current-buffer) #'external-debugging-output)
                       (print (window-buffer) #'external-debugging-output))
                   (selected-window) t)))
        ((eq window t))
	((not (windowp window))
	 (error "Not a window: %s" window)))
  (when cleanup
    (setq image-mode-winprops-alist
  	  (delq nil (mapcar (lambda (winprop)
			      (let ((w (car-safe winprop)))
				(if (or (not (windowp w)) (window-live-p w))
				    winprop)))
  			    image-mode-winprops-alist))))
  (let ((winprops (assq window image-mode-winprops-alist)))
    ;; For new windows, set defaults from the latest.
    (if winprops
        ;; Move window to front.
        (setq image-mode-winprops-alist
              (cons winprops (delq winprops image-mode-winprops-alist)))
      (setq winprops (cons window
                           (copy-alist (cdar image-mode-winprops-alist))))
      ;; Add winprops before running the hook, to avoid inf-loops if the hook
      ;; triggers window-configuration-change-hook.
      (setq image-mode-winprops-alist
            (cons winprops image-mode-winprops-alist))
      (run-hook-with-args 'image-mode-new-window-functions winprops))
    winprops))
