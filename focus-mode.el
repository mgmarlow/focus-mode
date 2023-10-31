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
        (focus--region (lambda () (bounds-of-thing-at-point 'sentence)))
      (focus--region (lambda () (bounds-of-thing-at-point 'paragraph)))))
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
