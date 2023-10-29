;;; focus.el --- A minor mode for focused writing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: wp
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

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

;; This is kinda important for this package, need to document some
;; way. Definitely need to set this in userland.
(customize-set-variable 'sentence-end-double-space nil)

(defun focus-sentence-region ()
  (save-excursion
    ;; Adapted from `forward-sentence'.
    (let ((pos (point))
          (sentence-end (sentence-end))
          par-text-beg par-text-end
          focus-beg focus-end)
      (save-excursion
        (start-of-paragraph-text)
        (setq par-text-beg (point))
        (end-of-paragraph-text)
        (setq par-text-end (point)))
      (if (re-search-backward sentence-end par-text-beg t)
          (setq focus-beg (match-end 0))
        (setq focus-beg par-text-beg))
      ;; Reset position before searching again
      (goto-char pos)
      (if (re-search-forward sentence-end par-text-end t)
          (setq focus-end (match-beginning 0))
        (setq focus-end par-text-end))
      (cons focus-beg focus-end))))

(defvar-local focus-prev-region nil)

;; TODO: Error in post-command-hook (focus-sentence-hook): (error "Invalid search bound (wrong side of point)")
(defun focus-sentence ()
  (interactive)
  (let ((region (focus-sentence-region)))
    (unless (equal focus-prev-region region)
      (when focus-prev-region
        (set-text-properties (car focus-prev-region) (cdr focus-prev-region) nil))
      (add-face-text-property (car region) (cdr region) '(:foreground "red"))
      (setq focus-prev-region region))))

(defvar-local focus-last-command-pos 0)

(defun focus-sentence-hook ()
  (unless (or (window-minibuffer-p) (equal (point) focus-last-command-pos))
    (focus-sentence))
  (setq focus-last-command-pos (point)))

(define-minor-mode focus-mode
  "Toggle focus mode."
  :lighter "focus-mode")

(add-hook 'focus-mode-hook
          (lambda ()
            (if focus-mode
                (progn
                  (font-lock-mode -1)
                  ;; TODO: custom face
                  (buffer-face-set 'consult-separator)
                  (add-hook 'post-command-hook #'focus-sentence-hook nil t))
              (progn 
                (buffer-face-mode -1)
                (font-lock-mode 1)
                (remove-hook 'post-command-hook #'focus-sentence-hook)))))

(provide 'focus)
;;; focus.el ends here
