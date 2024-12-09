(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(setq inhibit-splash-screen t)

(transient-mark-mode 1)

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

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(setq TeX-PDF-mode t)

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

(use-package flycheck-haskell
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

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

(use-package dap-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package vterm
  :ensure t)

(use-package multiple-cursors
  :ensure t)
;;VSCODE CTRL + D OR ADD A CURSOR IN LINE BELOW
(global-set-key (kbd "C-c q") 'mc/mark-next-like-this)
;;SAME BUT ADD LINE ABOVE
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
;;MARK ALL THE SAME
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)

;;MOVE TEXT UP OR DOWN USING ALT (META) UP~DOWN
(use-package move-text
  :ensure t
  :init
  (move-text-default-bindings))

(use-package go-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package latex-preview-pane
  :ensure t
  :custom
  (latex-preview-pane-enable))

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

(with-eval-after-load 'eglot
  (setf (alist-get 'elixir-mode eglot-server-programs)
        (if (and (fboundp 'w32-shell-dos-semantics)
                 (w32-shell-dos-semantics))
            '("language_server.bat")
          (eglot-alternatives
           '("language_server.sh" "~/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))))


