(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)

(scroll-bar-mode -1)

(global-display-line-numbers-mode t)

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents) 
  (package-install 'use-package) )

(load-theme 'kanagawa t)

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)))

(use-package flycheck
  :ensure t
  :init  (global-flycheck-mode t)
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(global-set-key (kbd "C-<tab>") 'other-window)

(electric-pair-mode 1)

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (global-set-key [f8] 'neotree-toggle)))

(use-package all-the-icons
  :ensure t)

(use-package company
  :ensure t)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(global-tree-sitter-mode)


(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'prog-mode-hook #'eglot-ensure)

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(setq rustic-lsp-client 'eglot)


(use-package alchemist
  :ensure t)

(use-package dap-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package vterm
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package zig-mode
  :ensure t)

(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)
(tooltip-mode 1)
(dap-ui-controls-mode 1)

(require 'dap-cpptools)
(require 'dap-codelldb)

(dap-register-debug-template "Codelldb"
			     (list :type "lldb"
				   :request "launch"
				   :program "path"
				   :cwd "path"))
