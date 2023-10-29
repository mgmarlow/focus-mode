;;; focus-mode.el --- A minor mode for focused writing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: wp
;; Homepage: https://git.sr.ht/~mgmarlow/focus-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

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

;; A minor mode for focused writing.

;;; Code:

;; TODO:
;; This is kinda important for this package, need to document some
;; way. Definitely need to set this in userland.
(customize-set-variable 'sentence-end-double-space nil)

(defgroup focus nil
  "Minor mode for focused writing."
  :group 'wp)

(defcustom focus-face-main '(:foreground "#cfbcba")
  "Primary, focused font face.

Value is a face name or plist of face attributes."
  :type 'face
  :group 'focus)

(defcustom focus-face-dim '(:foreground "#cfbcba")
  "Dimmed, background font face.

Value is a face name or plist of face attributes."
  :type 'face
  :group 'focus)

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
      ;; Avoid "wrong side of point" errors during re-search.
      (princ (cons par-text-beg par-text-end))
      (when (and (>= pos par-text-beg)
                 (<= pos par-text-end))
        (if (re-search-backward sentence-end par-text-beg t)
            (setq focus-beg (match-end 0))
          (setq focus-beg par-text-beg))
        ;; Reset position before searching again
        (goto-char pos)
        (if (re-search-forward sentence-end par-text-end t)
            (setq focus-end (match-end 0))
          (setq focus-end par-text-end))
        (cons focus-beg focus-end)))))

(defvar-local focus-prev-region nil)

;; TODO: Something here is interferring with selecting text.
(defun focus-sentence ()
  (condition-case nil
      (when-let ((region (focus-sentence-region)))
        (unless (or (equal focus-prev-region region)
                    (< (car region) 0)
                    (> (cdr region) (point-max)))
          (when focus-prev-region
            (set-text-properties (car focus-prev-region) (cdr focus-prev-region) nil))
          (add-face-text-property (car region) (cdr region) focus-face-main)
          (setq focus-prev-region region)))
    ;; Ignore navigation errors in the hope we recover.
    (args-out-of-range nil)))

(defvar-local focus-last-command-pos 0)

;; TODO: sentence vs. paragraph mode
(defun focus-sentence-hook ()
  (unless (or (window-minibuffer-p)
              (equal (point) focus-last-command-pos))
    (message (format "trying to focus at %d" (point)))
    (focus-sentence))
  (setq focus-last-command-pos (point)))

(define-minor-mode focus-mode
  "Toggle focused writing mode."
  :lighter "focus-mode")

(add-hook 'focus-mode-hook
          (lambda ()
            (if focus-mode
                (progn
                  (font-lock-mode -1)
                  (buffer-face-set focus-face-dim)
                  (add-hook 'post-command-hook #'focus-sentence-hook nil t))
              (progn
                (buffer-face-mode -1)
                (font-lock-mode 1)
                (remove-hook 'post-command-hook #'focus-sentence-hook)))))

(provide 'focus-mode)
;;; focus-mode.el ends here
