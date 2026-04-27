;;; post-init.el --- load before init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(defvar my-leader-map (make-sparse-keymap))

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
  (which-key-max-description-length 40)
  :config
  (which-key-setup-side-window-right-bottom))

(use-package compile
  :ensure nil
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
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :custom
  (tooltip-delay 20)
  (tooltip-short-delay 0.08)
  (tooltip-hide-delay 4))

(use-package window
  :ensure nil
  :custom
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  (switch-to-prev-buffer-skip-regexp "^\\*\\|^magit")
  (switch-to-prev-buffer-skip 'this)
  :config
  (add-to-list 'display-buffer-alist
               '((major-mode . compilation-mode)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.3)))
  (add-to-list 'display-buffer-alist
               '("\\*helpful.*\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (window-width . 80))))

(use-package project
  :ensure nil
  :custom
  (project-compilation-buffer-name-function
   (lambda (mode) (format "*compilation-%s*" (project-name (project-current)))))
  :bind (:map my-leader-map
              ("f" . project-find-file)
              ("c" . project-compile)
              ("r" . project-recompile)
              ("p" . project-switch-project))
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" ?m))
  (add-to-list 'project-switch-commands
               '(project-compile "compile" ?c))
  (defun my-toggle-project-compilation-buffer ()
    "Show or hide the current project's compilation buffer without moving focus."
    (interactive)
    (when-let* ((pr (project-current))
                (buff-name (format "*compilation-%s*" (project-name pr)))
                (target (get-buffer buff-name)))
      (if-let ((window (get-buffer-window target)))
          (delete-window window)
        (display-buffer target))))
  (keymap-set my-leader-map "s" #'my-toggle-project-compilation-buffer))

;; (use-package server
;;   :ensure nil
;;   :commands server-start
;;   :hook
;;   (after-init . server-start))

(use-package emacs
  :custom
  (auto-save-default t)
  (auto-save-interval 300)
  (auto-save-timeout 30)
  (dabbrev-case-replace nil)
  (dabbrev-case-fold-search nil)
  (package-install-upgrade-built-in t)
  (line-number-mode t)
  (column-number-mode t)
  (mode-line-position-column-line-format '("%l:%C"))
  (treesit-font-lock-level 4)
  (dired-movement-style 'bounded-files)
  (confirm-kill-emacs 'y-or-n-p)
  (compilation-environment (list (concat "PATH=" (getenv "HOME") "/.bun/bin:" (getenv "PATH"))))
  :config
  (define-key global-map (kbd "C-,") my-leader-map)
  ;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)
  (global-set-key (kbd "M-n") 'scroll-up-line)
  (global-set-key (kbd "M-p") 'scroll-down-line)
  (global-set-key (kbd "C-M-v") 'scroll-down-line)
  (global-set-key (kbd "M-/") 'dabbrev-completion)
  (global-set-key (kbd "C-c u") 'winner-undo)
  (global-set-key (kbd "C-c r") 'winner-redo)
  (with-eval-after-load 'winner
    (define-key winner-repeat-map (kbd "u") #'winner-undo)
    (define-key winner-repeat-map (kbd "r") #'winner-redo))

  (defun my-scroll-other-window-up ()
    (interactive)
    (scroll-other-window 1))

  (defun my-scroll-other-window-down ()
    (interactive)
    (scroll-other-window -1))

  (global-set-key (kbd "C-M-v") 'my-scroll-other-window-up)
  (global-set-key (kbd "C-M-S-v") 'my-scroll-other-window-down)
  (global-set-key (kbd "M-[") 'switch-to-prev-buffer)
  (global-set-key (kbd "M-]") 'switch-to-next-buffer)

  (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-16"))
  ;; (mapc #'disable-theme custom-enabled-themes)
  ;; (load-theme 'wombat t)
  (setq-default display-line-numbers-type 'relative)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-line-numbers-mode))
  (delete-selection-mode 1)

  (unless (and (eq window-system 'mac)
               (bound-and-true-p mac-carbon-version-string))
    ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
    ;; versions, except for emacs-mac.
    ;;
    ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
    ;; this version of Emacs natively supports smooth scrolling.
    ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
    (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
    (pixel-scroll-precision-mode 1))

  (add-hook 'after-init-hook #'repeat-mode)
  (add-hook 'after-init-hook #'display-time-mode)
  (add-hook 'after-init-hook #'show-paren-mode)
  (add-hook 'after-init-hook #'winner-mode)
  (add-hook 'after-init-hook #'window-divider-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq dired-omit-files (concat "\\`[.]\\'"
                                 "\\|\\(?:\\.js\\)?\\.meta\\'"
                                 "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                                 "\\|^\\.DS_Store\\'"
                                 "\\|^\\.\\(?:svn\\|git\\)\\'"
                                 "\\|^\\.ccls-cache\\'"
                                 "\\|^__pycache__\\'"
                                 "\\|^\\.project\\(?:ile\\)?\\'"
                                 "\\|^flycheck_.*"
                                 "\\|^flymake_.*"))
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  ;; dired: Group directories first
  (with-eval-after-load 'dired
    (let ((args "--group-directories-first -ahlv"))
      (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
        (if-let* ((gls (executable-find "gls")))
            (setq insert-directory-program gls)
          (setq args nil)))
      (when args
        (setq dired-listing-switches args))))
  (add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

  (defun my-backward-kill-word ()
    (interactive)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if (and (> (point) (point-min))
               (member (char-before) '(?\ ?\t ?\n)))
          (let ((origin (point)))
            (skip-chars-backward " \t\n")
            (kill-region (point) origin))
        (backward-kill-word 1))))
  (global-set-key (kbd "C-w") 'my-backward-kill-word)
  (add-to-list 'load-path (expand-file-name "lisp/" minimal-emacs-user-directory))
  (global-set-key (kbd "C-c w") ctl-x-4-map)
  )

(use-package my-isearch
  :ensure nil
  :after isearch
  :bind (("C-r" . my-isearch-backward-region-or-word)
         ("C-s" . my-isearch-forward-region-or-word)
         :map isearch-mode-map
              ("C-g" . my-isearch-abort-dwim)
              ("C-s" . my-isearch-switch-to-forward)
              ("C-r" . my-isearch-switch-to-backward)))

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :bind
  (:map vertico-map
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group))
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (consult-buffer reverse)))
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (consult-location buffer)))
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (side . right)
          (window-width . 0.5))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))
                                   (project-file (styles partial-completion))))
  ;; (completion-pcm-leading-wildcard t)   ;; Emacs 31: partial-completion behaves like substring
  )

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

(use-package embark
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :custom
  (consult-fd-args (list (if (executable-find "fdfind" 'remote) "fdfind" "fd")
                         "--type" "f"
                         "--hidden"
                         "--no-ignore"
                         "--color=never"))
  (consult-buffer-sources
   '(consult-source-buffer
     consult-source-hidden-buffer
     consult-source-modified-buffer
     consult-source-other-buffer
     consult-source-buffer-register
     consult-source-file-register consult-source-bookmark
     consult-source-project-buffer-hidden
     consult-source-project-recent-file-hidden
     consult-source-project-root-hidden))
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)
         :map my-leader-map
         ("a" . consult-fd)
         ("SPC" . consult-buffer))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  ;; (setq consult-async-input-debounce 0.02
  ;;       consult-async-input-throttle 0.05
  ;;       consult-async-refresh-delay 0.02)

  :config
  (add-to-list 'consult-buffer-filter "\\*vterm")
  (consult-customize
   consult-line  :initial (when (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end)))
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

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
             eglot-format-buffer)
  :config
  (setf (alist-get '(kotlin-mode kotlin-ts-mode) eglot-server-programs nil nil #'equal)
        '("127.0.0.1" 9999)))

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
  (org-startup-truncated t))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(unless (package-installed-p 'kotlin-ts-mode)
  (package-vc-install
   '(kotlin-ts-mode
     :url "https://gitlab.com/bricka/emacs-kotlin-ts-mode.git")))

(use-package kotlin-ts-mode
  :ensure nil
  :mode (("\\.kt\\'"  . kotlin-ts-mode)
         ("\\.kts\\'" . kotlin-ts-mode))
  :hook
  (kotlin-ts-mode . (lambda ()
                      (setq-local eglot-ignored-server-capabilities
                                  '(:completionProvider))))
  )

(use-package magit
  :commands (magit-status magit-blame)
  :bind (("C-x g" . magit-status))
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
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "10:00"))

(use-package ispell
  :ensure nil
  :commands (ispell ispell-minor-mode)
  :custom
  ;; Set the ispell program name to aspell
  (ispell-program-name "aspell")

  ;; Define the "en_US" spell-check dictionary locally, telling Emacs to use
  ;; UTF-8 encoding, match words using alphabetic characters, allow apostrophes
  ;; inside words, treat non-alphabetic characters as word boundaries, and pass
  ;; -d en_US to the underlying spell-check program.
  (ispell-local-dictionary-alist
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; Configures Aspell's suggestion mode to "ultra", which provides more
  ;; aggressive and detailed suggestions for misspelled words. The language
  ;; is set to "en_US" for US English, which can be replaced with your desired
  ;; language code (e.g., "en_GB" for British English, "de_DE" for German).
  (ispell-extra-args '(; "--sug-mode=ultra"
                       "--lang=en_US")))

(use-package flyspell
  :ensure nil
  :commands flyspell-mode
  :hook
  (; (prog-mode . flyspell-prog-mode)
   (text-mode . (lambda()
                  (if (or (derived-mode-p 'yaml-mode)
                          (derived-mode-p 'yaml-ts-mode)
                          (derived-mode-p 'ansible-mode))
                      (flyspell-prog-mode)
                    (flyspell-mode 1)))))
  :config
  ;; Remove strings from Flyspell
  (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                       flyspell-prog-text-faces))

  ;; Remove doc from Flyspell
  (setq flyspell-prog-text-faces (delq 'font-lock-doc-face
                                       flyspell-prog-text-faces)))

(use-package avy
  :ensure t
  :commands (avy-goto-char
             avy-goto-char-2
             avy-next)
  :init
  (global-set-key (kbd "C-=") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2))

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

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yasnippet                  ; alternative https://github.com/minad/tempel
  :ensure t
  :commands (yas-minor-mode
             yas-global-mode)

  :hook
  (after-init . yas-global-mode)

  :custom
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
  (setq yas-verbosity 0))

(use-package persist-text-scale
  :commands (persist-text-scale-mode
             persist-text-scale-restore)

  :hook (after-init . persist-text-scale-mode)

  :custom
  (text-scale-mode-step 1.07))

(use-package vterm ;; has prerequisite cmake, libtool check document
  :ensure t
  :commands (vterm)
  :bind (("C-<return>" . my-toggle-vterm)
         (:map vterm-mode-map
               ("C-M-1" . my-vterm-1)
               ("C-M-2" . my-vterm-2)
               ("C-M-3" . my-vterm-3)
               ("C-M-4" . my-vterm-4)
               ("C-M-5" . my-vterm-5)
               ("C-M-]" . my-vterm-next)
               ("C-M-[" . my-vterm-prev))))

(use-package toggle-vterm
  :ensure nil
  :after vterm)

;; (use-package combobulate
;;   :custom
;;   (combobulate-key-prefix "C-c o")
;;   :commands (combobulate-mode)
;;   :hook (prog-mode . combobulate-mode)
;;   :load-path ("~/combobulate"))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . 'ace-window)))

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

(use-package easysession
  :ensure t
  :demand t
  :commands (easysession-switch-to
             easysession-save
             easysession-save-mode
             easysession-load-including-geometry)
  :custom
  (easysession-mode-line-misc-info t)
  (easysession-switch-to-save-session nil)
  :config
  (global-set-key (kbd "C-c l") 'easysession-switch-to)
  (global-set-key (kbd "C-c s") 'easysession-save)
  (setq easysession-setup-load-session nil)
  (easysession-define-handler
    "eww"
    (lambda (session-data)
      (dolist (item session-data)
        (let* ((buffer-name (car item))
               (data (cdr item))
               (url (alist-get 'url data))
               (point-pos (alist-get 'point data)))
          (when (and url (string-match "\\*eww\\*" buffer-name))
            (eww url)
            (when point-pos
              ;; wait until page is rendered
              (add-hook
               'eww-after-render-hook
               (lambda ()
                 (goto-char (min point-pos (point-max))))
               ;; append, local
               nil t))))))
    (lambda (buffers)
      (easysession-save-handler-dolist-buffers
        buffers
        (when (derived-mode-p 'eww-mode)
          (let ((url (or (and (boundp 'eww-current-url) eww-current-url)
                         (plist-get eww-data :url))))
            (when url
              (cons (buffer-name)
                    (list
                     (cons 'url url)
                     (cons 'point (point)))))))))))

(use-package better-jumper
  :ensure t
  :custom
  (better-jumper-add-jump-behavior 'replace)
  :commands (better-jumper-jump-backward
             better-jumper-jump-forward
             better-jumper-set-jump)
  :bind (("C-o" . better-jumper-jump-backward)
         ("C-M-o" . better-jumper-jump-forward))
  :init
  (defun my/better-jumper-set-jump (&rest _)
    (better-jumper-set-jump))
  (advice-add 'xref-find-definitions :before #'my/better-jumper-set-jump)
  (advice-add 'xref-find-references :before #'my/better-jumper-set-jump)
  (advice-add 'consult-line :before #'my/better-jumper-set-jump)
  (advice-add 'consult-imenu :before #'my/better-jumper-set-jump)
  (advice-add 'consult-buffer :before #'my/better-jumper-set-jump)
  (advice-add 'project-find-file :before #'my/better-jumper-set-jump)
  (advice-add 'consult-grep :before #'my/better-jumper-set-jump)
  (advice-add 'consult-ripgrep :before #'my/better-jumper-set-jump)
  :config
  (better-jumper-mode 1))

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
