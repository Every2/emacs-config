;; Emacs display/initial configurations
(setenv "LSP_USE_PLISTS" "true")

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(setq inhibit-splash-screen t)

(electric-pair-mode 1)

(transient-mark-mode 1)

(scroll-bar-mode -1)

(global-display-line-numbers-mode t)

;; MELPA
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package) )

;; Comestic
(use-package kanagawa-themes
  :ensure t
  :config
  (load-theme 'kanagawa-wave t))

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (global-set-key [f8] 'neotree-toggle)))

(use-package all-the-icons
  :ensure t)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(add-to-list 'load-path "~/emacs-libvterm")
(require 'vterm)

(global-tree-sitter-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons
  :ensure t)

;; NON MELPA PACKAGES
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(setq TeX-PDF-mode t)

;; Programming/Modes/LSP


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package markdown-mode
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (go-mode . lsp)
  (elixir-mode . lsp)
  (cmake-mode . lsp)
  (toml-mode . lsp)
  (racket-mode . lsp)
  :commands lsp)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . 'lsp-mode))


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-flycheck-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  )

(setq lsp-ui-doc-enable t)


(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :ensure t)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :ensure t)

(add-hook 'after-init-hook 'global-company-mode)

(use-package flycheck
  :ensure t
  :init  (global-flycheck-mode t)
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package toml-mode
  :ensure t)

(use-package elixir-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package rustic
  :ensure t
  :config
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

(defun rk/rustic-mode-hook ()
  (when buffer-file-name
   (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package flycheck-rust
    :ensure t
    :after flycheck
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package cmake-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package latex-preview-pane
  :ensure t
  :custom
  (latex-preview-pane-enable))

(use-package dap-mode
  :ensure t
  :init
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(require 'dap-cpptools)
(require 'dap-codelldb)
(dap-register-debug-template "Codelldb"
			     (list :type "lldb"
				   :request "launch"
				   :program "path"
				   :cwd "path"))

(use-package magit
  :ensure t)

;; Movimentation

(global-set-key (kbd "C-<tab>") 'other-window)

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)))

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


