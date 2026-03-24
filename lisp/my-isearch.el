;;; my-isearch.el --- wrap iseach                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  panuvit sreudomdetsakul

;; Author: panuvit sreudomdetsakul <doggo@panuvits-MacBook-Pro-2.local>
;; Keywords: convenience

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

(defun my-isearch-abort-dwim ()
  "Quit isearch fully if in error, else cancel normally."
  (interactive)
  (if (and (boundp 'isearch-error) isearch-error)
      (isearch-abort)
    (isearch-cancel)))

(defun my-isearch-switch-to-backward ()
  "Switch to backward search if currently forward, else repeat backward."
  (interactive)
  (if isearch-forward
      (progn
        (isearch-repeat-backward)
        (isearch-repeat-backward))
    (isearch-repeat-backward)))

(defun my-isearch-switch-to-forward ()
  "Switch to forward search if currently backward, else repeat forward."
  (interactive)
  (if (not isearch-forward)
      (progn
        (isearch-repeat-forward)
        (isearch-repeat-forward))
    (isearch-repeat-forward)))

(defun my-isearch-forward-region-or-word ()
  "Start `isearch-forward`. If a region is active, use its text as the initial search string.
The region is deactivated after starting the search."
  (interactive)
  (let ((search-string (when (use-region-p)
                         (prog1
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (deactivate-mark))))) ; deactivate region
    (isearch-forward nil 1)
    (when search-string
      (isearch-yank-string search-string))))

(provide 'my-isearch)
;;; my-isearch.el ends here
