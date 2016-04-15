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

(require 'cider-browse-ns)
(require 'cider-interaction)
(require 'cider-client)
(require 'cider-compat)
(require 'cider-util)

(defgroup cider-browse-deps nil
  "Dependency browsing and navigation."
  :prefix "cider-browse-deps-"
  :package-version `(cider . "0.12.0")
  :group 'cider)

(defconst cider-browse-deps-buffer "*cider-deps-browser*")

(push cider-browse-deps-buffer cider-ancillary-buffers)

;;; FACES
(defface cider-browse-deps-ns-face
  '((t (:inherit font-lock-type-face)))
  "Face for a namespace on the classpath."
  :group 'cider-browse-deps)

(defface cider-browse-deps-dep-face
  '((t (:inherit shadow)))
  "Face for a namespace's dependency/dependencies."
  :group 'cider-browse-deps)

;;; Mode Definition
(defvar cider-browse-deps-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map [return] #'cider-browse-ns-find-at-point)
    (define-key map "n" #'next-line)
    (define-key map "j" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "k" #'previous-line)
    (define-key map "N" #'cider-browse-deps--next-ns)
    (define-key map "J" #'cider-browse-deps--next-ns)
    (define-key map "P" #'cider-browse-deps--previous-ns)
    (define-key map "K" #'cider-browse-deps--previous-ns)
    map))

(define-derived-mode cider-browse-deps-mode special-mode "browse-deps"
  "Major mode for browsing namespaces and their dependencies.

\\{cider-browse-deps-mode-map}"
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t))

(defun cider-browse-deps--list (buffer title items)
  "Reset contents of BUFFER.
Display TITLE at the top and ITEMS are indented underneath."
  (with-current-buffer buffer
    (cider-browse-deps-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-max))
      (insert (cider-propertize title 'emph) "\n\n")
      (dolist (item items)
        (insert (propertize (concat "  " (car item) "\n")
                            'face 'cider-browse-deps-ns-face
                            'id (car item)
                            'type 'ns))
        (dolist (subitem (cadr item))
          (insert (propertize (concat "    " subitem "\n")
                              'face 'cider-browse-deps-dep-face
                              'id (car item)
                              'type 'dep))))
      (goto-char (point-min)))))

;; Interactive Functions

;;;###autoload
(defun cider-browse-deps (namespace)
  "Gets NAMESPACE's dependencies."
  (interactive (list (completing-read "Dependencies for NS: "
                                      (seq-map #'car (cider-sync-request:dependencies)))))
  (with-current-buffer (cider-popup-buffer cider-browse-deps-buffer t)
    (let ((deps (cider-sync-request:dependencies namespace)))
      (cider-browse-deps--list (current-buffer)
                             "Namespaces in classpath and their dependencies."
                             deps))))

;;;###autoload
(defun cider-browse-deps-all ()
  "List all NAMESPACE's on classpath and their dependencies."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-browse-deps-buffer t)
    (let ((deps (cider-sync-request:dependencies)))
      (cider-browse-deps--list (current-buffer)
                             "Namespaces in classpath and their dependencies."
                             deps))))

;;; Navigation/Actions

(defun cider-browse-deps--next-ns ()
  (interactive)
  "Advance point to the next NS in the list."
  (with-current-buffer (get-buffer cider-browse-deps-buffer)
    (when-let ((pos (next-single-property-change (point) 'id)))
      (goto-char pos))))

(defun cider-browse-deps--previous-ns ()
  (interactive)
  "Retract point to the previous NS in the list."
  (with-current-buffer (get-buffer cider-browse-deps-buffer)
    (when-let ((pos (previous-single-property-change (point) 'id)))
      (goto-char pos))))

(provide 'cider-browse-deps)

;;; cider-browse-deps.el ends here
