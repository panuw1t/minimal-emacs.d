;;; toggle-vterm.el --- keymap toggle for vterm      -*- lexical-binding: t; -*-

;; Copyright (C) 2026  panuvit sreudomdetsakul

;; Author: panuvit sreudomdetsakul <doggo@panuvits-MacBook-Pro-2.local>
;; Keywords: terminals, convenience

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

(require 'vterm)

(defun my-vterm-switch (n)
  "Switch to vterm buffer N, create if it doesn't exist."
  (let ((name (format "*vterm-%d*" n)))
    (if (get-buffer name)
        (switch-to-buffer name)
      (vterm name))))

(dotimes (i 5)
  (let ((n (1+ i)))
    (eval
     `(defun ,(intern (format "my-vterm-%d" n)) ()
        (interactive)
        (my-vterm-switch ,n)))))

(defvar my-last-non-vterm-buffer nil
  "Stores the last non-vterm buffer.")

(defun my-toggle-vterm ()
  "Toggle between last vterm and last non-vterm buffer."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (when (buffer-live-p my-last-non-vterm-buffer)
        (switch-to-buffer my-last-non-vterm-buffer))
    (setq my-last-non-vterm-buffer (current-buffer))
    (let ((vterm-buffer
           (seq-find
            #'my--vterm-buffer-p
            (buffer-list))))
      (if vterm-buffer
          (switch-to-buffer vterm-buffer)
        (my-vterm-1)))))

(defun my--vterm-buffer-p (buf)
  "Return non-nil if BUF is a vterm buffer."
  (with-current-buffer buf
    (derived-mode-p 'vterm-mode)))

(defun my--vterm-number (buf)
  "Extract numeric suffix from vterm buffer name."
  (with-current-buffer buf
    (if (string-match "\\*vterm-\\([0-9]+\\)\\*" (buffer-name))
        (string-to-number (match-string 1 (buffer-name)))
      0)))

(defun my--vterm-buffers ()
  "Return vterm buffers sorted numerically."
  (seq-sort
   (lambda (a b)
     (< (my--vterm-number a)
        (my--vterm-number b)))
   (seq-filter #'my--vterm-buffer-p (buffer-list))))

(defun my-vterm-next ()
  "Switch to next vterm buffer."
  (interactive)
  (let* ((buffers (my--vterm-buffers))
         (current (current-buffer))
         (pos (cl-position current buffers)))
    (when buffers
      (switch-to-buffer
       (nth (mod (1+ (or pos -1)) (length buffers))
            buffers)))))

(defun my-vterm-prev ()
  "Switch to previous vterm buffer."
  (interactive)
  (let* ((buffers (my--vterm-buffers))
         (current (current-buffer))
         (pos (cl-position current buffers)))
    (when buffers
      (switch-to-buffer
       (nth (mod (1- (or pos 1)) (length buffers))
            buffers)))))

(provide 'toggle-vterm)
;;; toggle-vterm.el ends here
