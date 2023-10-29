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

(defgroup focus nil
  "Minor mode for focused writing."
  :group 'wp)

(defcustom focus-type 'sentence
  "Set focusing behavior to active sentence or active paragraph.

Defaults to sentence."
  :type '(choice (const sentence)
                 (const paragraph))
  :group 'focus)

(defcustom focus-face-main '(:foreground "#cfbcba")
  "Primary, focused font face.

Value is a face name or plist of face attributes."
  :type 'face
  :group 'focus)

(defcustom focus-face-dim '(:foreground "#887c8a")
  "Dimmed, background font face.

Value is a face name or plist of face attributes."
  :type 'face
  :group 'focus)

(defun focus--paragraph-region ()
  "Return a dotted pair containing the current paragraph's min and max."
  (let (par-text-beg par-text-end)
    (save-excursion
      (start-of-paragraph-text)
      (setq par-text-beg (point))
      (end-of-paragraph-text)
      (setq par-text-end (point)))
    (cons par-text-beg par-text-end)))

;; TODO: Occasional off-by-one errors where the previous sentence's punctuation is highlighted
(defun focus--sentence-region ()
  "Return a dotted pair containing the current sentence's min and max."
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

(defvar-local focus--prev-region nil
  "Previous focused region.

Avoid focusing a region if it is has already been focused.")

(defun focus--clear-prev-region ()
  (when focus--prev-region
    (set-text-properties (car focus--prev-region) (cdr focus--prev-region) nil)))

;; TODO: Something here is interferring with selecting text.
(defun focus--region (region-fn)
  (condition-case nil
      (when-let ((region (funcall region-fn)))
        (unless (or (equal focus--prev-region region)
                    (< (car region) 0)
                    (> (cdr region) (point-max)))
          (focus--clear-prev-region)
          (add-face-text-property (car region) (cdr region) focus-face-main)
          (setq focus--prev-region region)))
    ;; Ignore navigation errors in the hope we recover.
    (args-out-range nil)))

(defvar-local focus--last-command-pos 0
  "Previous point position.

Ensures that `focus-post-command-hook' is only run after commands
that deal with moving the cursor.")

(defun focus-post-command-hook ()
  (unless (or (window-minibuffer-p)
              (equal (point) focus--last-command-pos))
    (if (eq focus-type 'sentence)
        (focus--region #'focus--sentence-region)
      (focus--region #'focus--paragraph-region)))
  (setq focus--last-command-pos (point)))

;;;###autoload
(define-minor-mode focus-mode
  "Toggle focused writing mode."
  :lighter "focus-mode"
  (cond
   ;; Initialization
   (focus-mode
    (setq focus--prev-region nil)
    (font-lock-mode -1)
    (buffer-face-set focus-face-dim)
    (add-hook 'post-command-hook #'focus-post-command-hook nil t))
   ;; Cleanup
   (t
    (focus--clear-prev-region)
    (buffer-face-mode -1)
    (font-lock-mode 1)
    (remove-hook 'post-command-hook #'focus-post-command-hook t))))

(provide 'focus-mode)
;;; focus-mode.el ends here
