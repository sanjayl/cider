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
(require 'dired)

(defgroup cider-browse-deps nil
  "Dependency browsing and navigation."
  :prefix "cider-browse-deps-"
  :package-version `(cider . "0.13.0")
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

(defface cider-browse-deps-circular-ns-face
  '((t (:inherit error)))
  "Face for a namespace with circular dependencies."
  :group 'cider-browse-deps)

(defun cider-browse-deps--choose-heading-face (heading)
  (if (s-contains? "Circular Dependency Error" heading)
      cider-browse-deps-circular-ns-face
    cider-browse-deps-ns-face))

;;; Mode Definition
(defhydra def-browse-help (:color pink)
  "
Movement:
_n_/_j_: Next Line          _N_/_J_: Next Heading
_p_/_k_: Previous Line      _P_/_K_: Previous Heading

Actions:
_d_:^^ Documentation of namespace              _f_: Filter by Glob
_s_/_<return>_: Goto source of namespace       _F_: Filter by Emacs RegEx
^ ^ ^        ^                                 _r_: Reset filtering

Exiting:
_q_: Quit browser
_?_/_g_: Close this popup"
  ("n" next-line nil)
  ("j" next-line nil)
  ("p" previous-line nil)
  ("k" previous-line nil)
  ("N" cider-browse-deps--next-ns nil)
  ("J" cider-browse-deps--next-ns nil)
  ("P" cider-browse-deps--previous-ns nil)
  ("K" cider-browse-deps--previous-ns nil)
  ("d" cider-browse-ns-doc-at-point nil)
  ("s" cider-browse-ns-find-at-point nil :color blue)
  ("f" cider-browse-deps--filter-by-glob nil)
  ("F" cider-browse-deps--filter-by-regex nil)
  ("r" cider-browse-deps--reset-filter nil)
  ("<return>" cider-browse-ns-find-at-point nil :color blue)
  ("q" cider-popup-buffer-quit-function nil :color blue)
  ("g" nil nil)
  ("?" nil nil))

(defvar cider-browse-deps-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map "?" #'def-browse-help/body)
    (define-key map "n" #'next-line)
    (define-key map "j" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "k" #'previous-line)
    (define-key map "N" #'cider-browse-deps--next-ns)
    (define-key map "J" #'cider-browse-deps--next-ns)
    (define-key map "P" #'cider-browse-deps--previous-ns)
    (define-key map "K" #'cider-browse-deps--previous-ns)
    (define-key map "d" #'cider-browse-ns-doc-at-point)
    (define-key map [return] #'cider-browse-ns-find-at-point)
    (define-key map "f" #'cider-browse-deps--filter-by-glob)
    (define-key map "F" #'cider-browse-deps--filter-by-regex)
    (define-key map "r" #'cider-browse-deps--reset-filter)
    ;; q for quit inherited from cider-popup-buffer-mode-map
    map))

(define-derived-mode cider-browse-deps-mode special-mode "browse-deps"
  "Major mode for browsing namespaces and their dependencies.

\\{cider-browse-deps-mode-map}"
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t))

;;; ACTIONS

(defun cider-browse-deps--filter-by-glob (glob)
  (interactive "sGlob pattern to filter by: ")
  (cider-browse-deps--filter-by-regex (dired-glob-regexp glob)))

(defun cider-browse-deps--filter-by-regex (re)
  (interactive "sEmacs-style regex to filter by: ")
  (with-current-buffer cider-browse-deps-buffer
    (let ((inhibit-read-only t)
          (pos (point)))
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((null (get-text-property (point) 'heading)) (forward-line))
              ((or (string-match re (get-text-property (point) 'heading))
                   (string-match re (get-text-property (point) 'id)))
               (let ((start (point)))
                 (forward-line)
                 (add-text-properties start (point) '(invisible t))))
              (t (forward-line))))
      (goto-char pos))))

(defun cider-browse-deps--reset-filter ()
  (interactive)
  (with-current-buffer cider-browse-deps-buffer
    (let ((inhibit-read-only t)
          (pos (point)))
      (remove-text-properties (point-min) (point-max) '(invisible nil))
      (goto-char pos))))

(defun cider-browse-deps--list (buffer title items)
  "Reset contents of BUFFER.
Display TITLE at the top and ITEMS are indented underneath."
  (with-current-buffer (cider-popup-buffer buffer t)
    (cider-browse-deps-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-max))
      (insert (cider-propertize title 'emph) "\n\n")
      (dolist (item items)
        (let ((heading (car item)))
          (insert (propertize (concat "  " heading "\n")
                                      'face (cider-browse-deps--choose-heading-face heading)
                                      'heading heading
                                      'type 'ns
                                      'id heading))
          (dolist (subitem (cadr item))
            (insert (propertize (concat "    " subitem "\n")
                                'face 'cider-browse-deps-dep-face
                                'heading heading
                                'type 'dep
                                'id subitem)))))
      (goto-char (point-min)))))

;; Interactive Functions
;;;###autoload
(defun cider-browse-circular-deps-all ()
  "Shows all circular dependencies on classpath."
  (interactive)
  (with-current-buffer (get-buffer-create cider-browse-deps-buffer)
    (when-let ((deps (cider-sync-request:circular)))
      (cider-browse-deps--list (current-buffer)
                               "Circular Dependencies on classpath."
                               deps))))

;;;###autoload
(defun cider-browse-deps (namespace)
  "Gets NAMESPACE's dependencies."
  (interactive (list (completing-read "Dependencies for NS: "
                                      (seq-map #'car (cider-sync-request:dependencies)))))
  (with-current-buffer (get-buffer-create cider-browse-deps-buffer)
    (when-let ((deps (cider-sync-request:dependencies namespace)))
      (cider-browse-deps--list (current-buffer)
                               "Namespaces in classpath and their dependencies."
                               deps))))

;;;###autoload
(defun cider-browse-deps-all ()
  "List all NAMESPACE's on classpath and their dependencies."
  (interactive)
  (with-current-buffer (get-buffer-create cider-browse-deps-buffer)
    (when-let ((deps (cider-sync-request:dependencies)))
      (cider-browse-deps--list (current-buffer)
                               "Namespaces in classpath and their dependencies."
                               deps))))

;;; Navigation/Actions

(defun cider-browse-deps--next-ns ()
  (interactive)
  "Advance point to the next NS in the list."
  (with-current-buffer (get-buffer cider-browse-deps-buffer)
    (when-let ((pos (next-single-property-change (point) 'heading)))
      (goto-char pos))))

(defun cider-browse-deps--previous-ns ()
  (interactive)
  "Retract point to the previous NS in the list."
  (with-current-buffer (get-buffer cider-browse-deps-buffer)
    (when-let ((pos (previous-single-property-change (point) 'heading)))
      (goto-char pos))))

(provide 'cider-browse-deps)

;;; cider-browse-deps.el ends here
