;;; doc-tools-pymupdf.el --- Manipulate pdf files with doc-pymupdf-epc-server  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@fedora>
;; Keywords: multimedia, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'epc)
(require 'svg)

(defconst doc-tools-pymupdf-base (file-name-directory load-file-name))


(defcustom doc-pymupdf-epc-executable
  (expand-file-name "doc-pymupdf-epc-server.py" doc-tools-pymupdf-base)
  "Name of doc-pymupdf-epc-server python file.")

(defvar doc-pymupdf-epc-server nil)

(defvar doc-pymupdf-text-page-methods
  "See https://pymupdf.readthedocs.io/en/latest/textpage.html#textpage"
  '(text blocks words xml))

(defvar doc-pymupdf-epc-info-commands '(doc-pymupdf-epc-number-of-pages
                                    doc-pymupdf-epc-toc
                                    doc-pymupdf-epc-metadata
                                    doc-pymupdf-epc-page-structured-text))

(defsubst list-to-cons (pair-list)
  (cons (nth 0 pair-list)
        (nth 1 pair-list)))

(defun doc-pymupdf-epc-info (function &optional arg)
  (interactive (list (completing-read "Select info type: "
                                      doc-pymupdf-epc-info-commands)
                     current-prefix-arg))
  (pp (pcase (intern function)
        ('doc-pymupdf-epc-page-structured-text
         (call-interactively #'doc-pymupdf-epc-page-structured-text))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*doc-pymupdf-epc-info*")))
  (when arg (pop-to-buffer "*doc-pymupdf-epc-info*")))

;;;###autoload
(defun doc-pymupdf-epc-start-server (&optional local)
  (interactive)
  (let ((server (epc:start-epc "python" (list doc-pymupdf-epc-executable))))
    (unless local
      (setq doc-pymupdf-epc-server server))
    server))

(defun doc-pymupdf-epc-kill-server (&optional local)
  (interactive)
  (epc:stop-epc doc-pymupdf-epc-server))

(defun doc-pymupdf-epc-restart-server (&optional local)
  (interactive)
  (epc:manager-restart-process doc-pymupdf-epc-server))

(defun doc-pymupdf-epc-test ()
  (interactive)
  (pp (epc:call-sync doc-pymupdf-epc-server 'test (list 'hello))))

(defun doc-pymupdf-epc-init (&optional file)
  (interactive "fSelect pdf file: ")
  (epc:call-sync doc-pymupdf-epc-server 'open (list (or file buffer-file-name))))

(defun doc-pymupdf-epc-init-data (&optional file)
	(deferred:$
	 (epc:call-deferred doc-pymupdf-epc-server
											'init_data
											(list (or file buffer-file-name)))
	 (deferred:nextc it
									 (lambda (_) (message "Loading page data on server completed")))))

(defun doc-pymupdf-epc-number-of-pages ()
  (interactive)
  (epc:call-sync doc-pymupdf-epc-server 'number_of_pages nil))

(defun doc-pymupdf-epc-structured-text (page &optional detail async push)
  "If PUSH non-nil then push to (buffer-local) `doc-scroll-structured-text'."
  (let ((buffer (current-buffer)))
    (if async
				(deferred:$
				 (epc:call-deferred doc-pymupdf-epc-server
														'structured_text
														(list (1- page) (when detail (symbol-name detail))))
				 (deferred:nextc it
												 (lambda (x)
													 (with-current-buffer buffer
														 (overlay-put (image-roll-overlay page) 'text x)))))
      (epc:call-sync doc-pymupdf-epc-server
										 'structured_text
										 (list (1- page) (when detail (symbol-name detail)))))))

;; (defun doc-pymupdf-epc-structured-text (&optional start-page end-page detail async push)
;;   "If PUSH non-nil then push to (buffer-local) `doc-scroll-structured-text'."
;;   (let ((buffer (current-buffer)))
;;     (if async
;; 				(deferred:$
;; 				 (epc:call-deferred doc-pymupdf-epc-server
;; 														'structured_text
;; 														(list (1- (or start-page 1)) end-page (if detail (symbol-name detail))))
;; 				 (deferred:nextc it
;; 												 (lambda (x)
;; 													 (if push
;; 															 (with-current-buffer buffer
;; 																 (push (car x) doc-scroll-structured-text)
;; 																 (when (= (print (length doc-scroll-structured-text)) doc-scroll-number-of-pages)
;; 																	 (setq doc-scroll-structured-text (nreverse doc-scroll-structured-text))))
;; 														 x))
;; 												 ;; (setq-local doc-scroll-structured-contents x)
;; 												 ;; (message (concat (propertize "doc-scroll-structured-contents" 'face 'bold) " set"))))))
;; 												 ))
;;       (epc:call-sync doc-pymupdf-epc-server
;; 										 'structured_text
;; 										 (list (1- (or start-page 1)) end-page (if detail (symbol-name detail)))))))

;; (defun doc-pymupdf-epc-page-structured-text (&optional page detail)
;;   (interactive (let ((last-page (doc-pymupdf-epc-number-of-pages)))
;;                  (list (read-number (format "Select page(s) (max %s): " last-page)
;;                                     (or (doc-scroll-current-page) 1))
;;                        (intern (completing-read "Select detail: "
;;                                                 '(plain djvu blocks words xml))))))
;;   (when (eq detail 'plain) (setq detail nil))
;;   (let ((text (epc:call-sync doc-pymupdf-epc-server
;;                              'page_structured_text
;;                              (list page (symbol-name detail)))))
;;     (if (eq detail 'xml)
;;         (with-temp-buffer
;;           (insert text)
;;           (xml-parse-region))
;;       text)))

(defun doc-pymupdf-epc-restructure-text (text)
  "Convert structured text to djvu text structure."
  (mapcar (lambda (e)
            (let ((type (car e)))
              (pcase type
                ('page (let-alist (nth 1 e)
                         (append (list type 0 0
                                       (string-to-number .width) (string-to-number .height))
                                 (doc-pymupdf-epc-restructure-text
                                  (delete "\n" (nthcdr 3 e))))))
                ((or 'line 'block) (append (cons type (mapcar #'string-to-number
                                                              (split-string (cdar (nth 1 e)))))
                                           (doc-pymupdf-epc-restructure-text
                                            (pcase type
                                              ('block (delete "\n" (nthcdr 3 e)))
                                              ('line (delete "\n" (nthcdr 3 (nth 3 e))))))))
                ('char (let-alist (nth 1 e)
                         (let ((coord-string (split-string .quad)))
                           `(,type
                             ,@(mapcar (lambda (n)
                                         (string-to-number (nth n coord-string)))
                                       '(0 1 6 7))
                             ,.c)))))))
          text))

(defun doc-pymupdf-epc-page-sizes ()
  (interactive)
  (let ((sizes (epc:call-sync doc-pymupdf-epc-server 'pagesizes nil)))
        (mapcar #'list-to-cons sizes)))

(defun doc-pymupdf-epc-page-svg-data (page text)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'renderpage_svg (list page text)))

(defun doc-pymupdf-epc-page-base64-image-data (page width &optional file)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'renderpage_base64 (list page width file)))

(defun doc-pymupdf-epc-page-image-file (page width path)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'renderpage_file (list page width path)))

(defun doc-pymupdf-epc-toc ()
  (interactive)
  (epc:call-sync doc-pymupdf-epc-server 'toc nil))

(defun doc-pymupdf-epc-metadata ()
  (interactive)
  (epc:call-sync doc-pymupdf-epc-server 'metadata nil))

(defun doc-pymupdf-epc-get-annots (page)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'get_annots (list page)))

(defun doc-pymupdf-epc-add-annot (page
				  edges
				  style
				  &optional display width)
  (epc:call-sync doc-pymupdf-epc-server
			     'addannot
			     (list page edges style display width)))
    

(defun doc-pymupdf-epc-search (pattern)
  (interactive "sEnter pattern: ")
  (epc:call-sync doc-pymupdf-epc-server 'search (list pattern)))

(defun doc-pymupdf-epc-list-colors ()
  (interactive "nEnter pattern: ")
  (epc:call-sync doc-pymupdf-epc-server 'list_colors nil))

(defun doc-pymupdf-epc-text-blocks ()
  (interactive "nEnter pattern: ")
  (epc:call-sync doc-pymupdf-epc-server 'text_blocks nil))

(defun doc-pymupdf-epc-save ()
  (interactive)
  (let ((status (print (epc:call-sync doc-pymupdf-epc-server 'save (list buffer-file-name)))))
    (pcase status
      ('t (set-buffer-modified-p nil)
	  (message "Document saved succesfully"))
      (_ status))))

(provide 'doc-tools-pymupdf)
;;; doc-pymupdf-epc.el ends here
