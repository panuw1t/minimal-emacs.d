;;; my-embark.el --- custom for embark action        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  panuvit sreudomdetsakul

;; Author: panuvit sreudomdetsakul <doggo@panuvits-MacBook-Pro-2.local>
;; Keywords:

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

(require 'embark)

(defun my/embark-act-ace-window ()
  "Use `ace-window' to select a window if multiple exist, then run the Embark action."
  (interactive)
  (let* ((target (car (embark--targets)))
         (type (plist-get target :type))
         (action (embark--default-action type)))

    (unless action
      (user-error "No default action found for target type '%s'" type))

    (if (<= (length (window-list)) 1)
        ;; SCENARIO A: Only 1 window. Do nothing special, just run the action.
        (command-execute action)

      ;; SCENARIO B: 2+ windows. Run ace-window with dispatch enabled.
      (let ((aw-dispatch-always t))
        (call-interactively #'ace-window))

      ;; Execute the action, forcing it into the currently selected window
      ;; so `display-buffer` doesn't misroute it if a new split was created.
      (let ((display-buffer-overriding-action '(display-buffer-same-window)))
        (command-execute action)))))

;; Bind this to "A" (Shift + a) or any key you prefer in the general Embark map
(with-eval-after-load 'embark
  (define-key embark-general-map (kbd "A") #'my/embark-act-ace-window))

(provide 'my-embark)
;;; my-embark.el ends here
