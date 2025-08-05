;; init config
(setenv "LSP_USE_PLISTS" "true")

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq custom-file null-device)

(electric-pair-mode 1)
(transient-mark-mode 1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq fill-column 80)

;; Eglot booster
(use-package eglot-booster
	:after eglot
	:config	(eglot-booster-mode))

;; Melpa
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Comestic
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
    (global-set-key [f8] 'neotree-toggle)))

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Programming utilities

(use-package vterm
  :ensure t)

(use-package company
  :ensure t
  :init
  (global-company-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-rust
    :ensure t
    :after (flycheck rust-ts-mode)
    :hook
    (flycheck-mode . flycheck-rust-setup))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-inline-mode))

;; Programming Languages

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(c-mode . c-ts-mode)
	(c++-mode . c++-ts-mode)
	(shell-script-mode . bash-ts-mode)
	(sh-mode . bash-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

(add-hook 'python-ts-mode-hook #'eglot-ensure)

(use-package nasm-mode
  :ensure t
  :mode "\\.asm"
  :hook (nasm-mode . (lambda () (electric-indent-mode -1))))

;; Debug

(use-package dape
  :ensure t
  :config
  (dape-breakpoint-global-mode)
  (setq dape-inlay-hints t))

(use-package repeat
  :ensure t
  :config
  (repeat-mode))

;; Text
(use-package markdown-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;; Moves
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c f") 'eglot-format-buffer)

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c q" . 'mc/mark-next-like-this)
	 ("C-c p" . 'mc/mark-previous-like-this)
	 ("C-c a" . 'mc/mark-all-like-this)))

;;MOVE TEXT UP OR DOWN USING ALT (META) UP~DOWN
(use-package move-text
  :ensure t
  :init
  (move-text-default-bindings))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)))

;; Perf
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
