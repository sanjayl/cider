;;; cider-browse-deps.el --- CIDER dependency browser

;; Copyright © 2014-2016 Bozhidar Batsov and CIDER contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; M-x cider-browse-deps
;;
;; Display a list of all namespaces on the classpath as well as the dependencies
;; of each of those namespaces.

;;; Code:

(require 'cider-interaction)
(require 'cider-client)
(require 'cider-compat)
(require 'cider-util)

(defconst cider-browse-deps-buffer "*cider-deps-browser*")

(push cider-browse-deps-buffer cider-ancillary-buffers)

;;; TODO SANJAYL (defvar-local cider-browse-ns-current-ns nil)

;; Mode Definition

(defvar cider-browse-deps-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    ;;; TODO SANJAYL (define-key map "d" #'cider-browse-ns-doc-at-point)
    ;;; TODO SANJAYL (define-key map "s" #'cider-browse-ns-find-at-point)
    ;;; TODO SANJAYL (define-key map [return] #'cider-browse-ns-operate-at-point)
    ;;; TODO SANJAYL (define-key map "^" #'cider-browse-ns-all)
    (define-key map "n" #'next-line)
    (define-key map "j" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "k" #'previous-line)
    map))

;;; TODO SANJAYL 
;; (defvar cider-browse-ns-mouse-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mouse-1] #'cider-browse-ns-handle-mouse)
;;     map))

(define-derived-mode cider-browse-deps-mode special-mode "browse-deps"
  "Major mode for browsing namespaces and their dependencies.

\\{cider-browse-deps-mode-map}"
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t)
  ;;; TODO SANJAYL (setq-local cider-browse-ns-current-ns nil)
  )

;;; TODO SANJAYL 
;; (defun cider-browse-ns--text-face (text)
;;   "Match TEXT with a face."
;;   (cond
;;    ((string-match-p "\\." text) 'font-lock-type-face)
;;    ((string-match-p "\\`*" text) 'font-lock-variable-name-face)
;;    (t 'font-lock-function-name-face)))

;;; TODO SANJAYL 
;; (defun cider-browse-ns--properties (text)
;;   "Decorate TEXT with a clickable keymap and a face."
;;   (let ((face (cider-browse-ns--text-face text)))
;;     (propertize text
;;                 'font-lock-face face
;;                 'mouse-face 'highlight
;;                 'keymap cider-browse-ns-mouse-map)))

;;; TODO SANJAYL 
;; (defun cider-browse-ns--list (buffer title items &optional ns noerase)
;;   "Reset contents of BUFFER.
;; Display TITLE at the top and ITEMS are indented underneath.
;; If NS is non-nil, it is added to each item as the
;; `cider-browse-ns-current-ns' text property.  If NOERASE is non-nil, the
;; contents of the buffer are not reset before inserting TITLE and ITEMS."
;;   (with-current-buffer buffer
;;     (cider-browse-ns-mode)
;;     (let ((inhibit-read-only t))
;;       (unless noerase (erase-buffer))
;;       (goto-char (point-max))
;;       (insert (cider-propertize title 'ns) "\n")
;;       (dolist (item items)
;;         (insert (propertize (concat "  " item "\n")
;;                             'cider-browse-ns-current-ns ns)))
;;       (goto-char (point-min)))))

(defun cider-browse-deps--list (buffer title items)
  "Reset contents of BUFFER.
Display TITLE at the top and ITEMS are indented underneath."
  (with-current-buffer buffer
    (cider-browse-deps-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)  ;; TODO SANJAYL: figure out if you need the (unless noerase (erase-buffer)) and the optional noerase arg
      (goto-char (point-max))
      (insert (cider-propertize title 'emph) "\n")
      (dolist (item items)
        (insert (concat "  " (car item) "\n"))
        (dolist (subitem (cadr item))
          (insert (concat "    " subitem "\n")))) ;; TODO SANJAYL: do we need any props here
      (goto-char (point-min)))))

;; Interactive Functions

;;;###autoload
(defun cider-browse-deps ()
  "List all NAMESPACE's on classpath and their dependencies."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-browse-deps-buffer t)
    (let ((deps (cider-sync-request:dependencies)))
      (cider-browse-deps--list (current-buffer)
                             "All namespaces on classpath"
                             deps))))

(defun cider-browse-ns--thing-at-point ()
  "Get the thing at point.
Return a list of the type ('ns or 'var) and the value."
  (let ((line (cider-string-trim (thing-at-point 'line))))
    (if (string-match "\\." line)
        (list 'ns line)
      (list 'var (format "%s/%s"
                         (or (get-text-property (point) 'cider-browse-ns-current-ns)
                             cider-browse-ns-current-ns)
                         line)))))

(defun cider-browse-ns-doc-at-point ()
  "Show the documentation for the thing at current point."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (value (cadr thing)))
    ;; value is either some ns or a var
    (cider-doc-lookup value)))

(defun cider-browse-ns-operate-at-point ()
  "Expand browser according to thing at current point.
If the thing at point is a ns it will be browsed,
and if the thing at point is some var - its documentation will
be displayed."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (type (car thing))
         (value (cadr thing)))
    (if (eq type 'ns)
        (cider-browse-ns value)
      (cider-doc-lookup value))))

(defun cider-browse-ns-find-at-point ()
  "Find the definition of the thing at point."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (type (car thing))
         (value (cadr thing)))
    (if (eq type 'ns)
        (cider-find-ns nil value)
      (cider-find-var current-prefix-arg value))))

(defun cider-browse-ns-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-browse-ns-operate-at-point))

(provide 'cider-browse-ns)

;;; cider-browse-ns.el ends here
