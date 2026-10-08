;;; post-init.el --- load before init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" minimal-emacs-user-directory))

;; macOS: ⌘ is Meta; leave ⌥ free for AeroSpace window-manager bindings
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
        ns-option-modifier 'none
        ns-right-option-modifier 'none))

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))

(use-package compile-angel
  :demand t
  :ensure t
  :custom
  (compile-angel-verbose t)
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (push "/lisp/toggle-vterm.el" compile-angel-excluded-files)
  (push "/lisp/my-isearch.el" compile-angel-excluded-files)
  (push "/lisp/meow-setup.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

(use-package autorevert
  :ensure nil
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package which-key
  :ensure t ; builtin
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  :config
  (which-key-setup-side-window-bottom))

(use-package compile
  :ensure nil
  :custom
  (compilation-environment (list (concat "PATH=" (getenv "HOME") "/.bun/bin:" (getenv "PATH"))))
  :config
  (setf (alist-get 'gradle-kotlin compilation-error-regexp-alist-alist)
        '("^e: file://\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "|")
  (uniquify-after-kill-buffer-p t))     ; TODO same file name for different project cause switch to show both need fix.

(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode))    ; TODO hover mouse on link for information, need investigate eldoc package

(use-package window
  :ensure nil)                          ; TODO need to check other package for control instead of pure configure
                                        ; windmove  directional move
                                        ; popper    handle *..* buffer
                                        ; shackle   control where special buffers appear


(use-package project
  :ensure nil
  :custom
  (project-compilation-buffer-name-function
   (lambda (mode) (format "*compilation-%s*" (project-name (project-current)))))
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" ?m))
  (add-to-list 'project-switch-commands
               '(project-compile "compile" ?c)))

;; (use-package server
;;   :ensure nil
;;   :commands server-start
;;   :hook
;;   (after-init . server-start))

(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion))
  :custom
  (dabbrev-case-replace nil)
  (dabbrev-case-fold-search 1))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              (";" . dired-do-shell-command))
  :custom
  (insert-directory-program "gls")
  (dired-listing-switches "-alh --group-directories-first"))

 (use-package emacs
  :custom
  (auto-save-default t)
  (auto-save-interval 300)
  (auto-save-timeout 30)
  (truncate-lines nil)
  (package-install-upgrade-built-in t)
  (line-number-mode t)
  (column-number-mode t)
  (mode-line-position-column-line-format '("%l:%C"))
  (treesit-font-lock-level 4)
  (confirm-kill-emacs 'y-or-n-p)
  (read-buffer-completion-ignore-case t)
  :hook
  (after-init . repeat-mode)
  (after-init . delete-selection-mode)
  (after-init . display-time-mode)
  (after-init . show-paren-mode)
  (after-init . winner-mode)
  (after-init . window-divider-mode)
  (after-init . minibuffer-depth-indicate-mode)
  :config
  (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-15"))
  ;; (mapc #'disable-theme custom-enabled-themes)
  ;; (load-theme 'wombat t)

  (setq-default display-line-numbers-type 'relative)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-line-numbers-mode))

  (unless (and (eq window-system 'mac)
               (bound-and-true-p mac-carbon-version-string))
    ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
    ;; versions, except for emacs-mac.
    ;;
    ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
    ;; this version of Emacs natively supports smooth scrolling.
    ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
    (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
    (pixel-scroll-precision-mode 1)))

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-trigger ".")
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
        ("C-M-g" . corfu-info-location)
        ("C-M-h" . corfu-info-documentation)
        ("C-M-SPC" . corfu-insert-separator))
  :hook
  (after-init . global-corfu-mode)
  :config
  (corfu-popupinfo-mode))

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (defalias 'cape-dabbrev-min-3 (cape-capf-prefix-length #'cape-dabbrev 3))
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev-min-3)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package vertico
  :ensure t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-multiform-commands           ;still stuck with default input for project-find-file may need other package
   '((project-find-file (vertico-sort-function . vertico-sort-length-alpha))))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)   ;; Emacs 31: partial-completion behaves like substring
  )

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode)
  :config
  (setf (alist-get 'imenu marginalia-annotators)
        '(none marginalia-annotate-imenu builtin)))

(use-package meow
  :ensure t)

(use-package meow-setup
  :ensure nil
  :after meow
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package key-chord
  :ensure
  :after meow
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.15)
  (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit))

;; TODO embark + consult

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package stripspace
  :ensure t
  :commands stripspace-local-mode
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))

(use-package undo-fu
  :ensure t
  :bind (("C-/" . 'undo-fu-only-undo)
         ("C-M-/" . 'undo-fu-only-redo))
  :init
  (global-unset-key (kbd "C-z")))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer
             my-eglot-kotlin)
  :config
  (setf (alist-get '(kotlin-mode kotlin-ts-mode) eglot-server-programs nil nil #'equal)
        '("127.0.0.1" 9999))
  (defun my-kotlin-lsp-running-p ()
    (if (executable-find "nc")
        (eq 0 (call-process "nc" nil nil nil "-z" "127.0.0.1" "9999"))
      (error "nc (netcat) is not installed")))


  (defun my-kotlin-lsp-start ()
    (start-process-shell-command
     "kotlin-lsp"
     "*kotlin-lsp*"
     (format "%s --multi-client"
             (executable-find "kotlin-lsp"))))

  (defun my-kotlin-wait-and-start-eglot (&optional retries)
    (let ((retries (or retries 10)))
      (when (> retries 0)
        (run-at-time
         2.0 nil
         (lambda ()
           (if (my-kotlin-lsp-running-p)
               (progn
                 (message "Kotlin LSP ready")
                 (eglot-ensure))
             (my-kotlin-wait-and-start-eglot (1- retries))))))))

  (defun my-eglot-kotlin ()
    (interactive)
    (if (my-kotlin-lsp-running-p)
        (eglot-ensure)
      (progn
        (message "Starting Kotlin LSP...")
        (my-kotlin-lsp-start)
        (my-kotlin-wait-and-start-eglot)))))

(use-package org
  :ensure nil
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  ;; (org-fontify-done-headline t)
  ;; (org-fontify-todo-headline t)
  ;; (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks t)
  (org-startup-truncated t)
  :config
  (define-key org-mode-map (kbd "C-,") nil))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package kotlin-ts-mode
  :ensure t
  :mode (("\\.kt\\'"  . kotlin-ts-mode)
         ("\\.kts\\'" . kotlin-ts-mode))
  :hook
  (kotlin-ts-mode . (lambda ()
                      (setq-local eglot-ignored-server-capabilities
                                  '(:completionProvider))))
  )

(use-package magit
  :commands (magit-status magit-blame)
  :bind (("C-x g" . magit-status)
         :map magit-mode-map
         ("n" . magit-section-forward-sibling)
         ("p" . magit-section-backward-sibling))

  :config
  (add-to-list 'display-buffer-alist
               '((major-mode . magit-status-mode)
                 (display-buffer-full-frame))))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-hide-results t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "10:00"))

(use-package avy
  :ensure t
  :commands (avy-goto-char
             avy-goto-char-2
             avy-next)
  :init
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "C-=") 'avy-goto-char))

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(use-package expand-region
  :bind ("C-;" . er/expand-region))

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-time-icon nil)
  :init
  (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  ;; (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("S-C-<return>" . crux-smart-open-line-above)
         ("C-x C-r" . crux-recentf-find-file)
         ("C-c F" . crux-recentf-find-directory)
         ("C-c U" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-^" . crux-top-join-line)
         ("C-c b" . crux-switch-to-previous-buffer)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; TODO tempel
;; TODO apheleia
