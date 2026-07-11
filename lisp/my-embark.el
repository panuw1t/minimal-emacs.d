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
  "Select a window and run the Embark action.
- 1 window: Split and execute.
- 2 windows: Switch to the other window and execute.
- 3+ windows: Use ace-window to select, then execute."
  (interactive)
  (let* ((target (car (embark--targets)))
         (type (plist-get target :type))
         (action (embark--default-action type))
         (win-count (length (window-list)))) ;; Store the count to check it easily

    (unless action
      (user-error "No default action found for target type '%s'" type))

    (cond
     ;; SCENARIO A: Only 1 window. Split it and switch.
     ((= win-count 1)
      (or (split-window-sensibly)
          (split-window-below))
      (other-window 1))

     ;; SCENARIO B: Exactly 2 windows. Just jump to the other one.
     ((= win-count 2)
      (other-window 1))

     ;; SCENARIO C: 3+ windows. Run ace-window with dispatch enabled.
     (t
      (let ((aw-dispatch-always t))
        (call-interactively #'ace-window))))

    ;; Execute the action, forcing it into whichever window was just selected above.
    (let ((display-buffer-overriding-action '(display-buffer-same-window)))
      (command-execute action))))

;; Bind this to "A" (Shift + a) or any key you prefer in the general Embark map
(with-eval-after-load 'embark
  (define-key embark-general-map (kbd "A") #'my/embark-act-ace-window))

(provide 'my-embark)
;;; my-embark.el ends here
