;; Remover boas vindas
(setq inhibit-startup-message t)

;; Remover Menus
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil) ; stop creating ~ files

;; Remover barra de rolagem
(scroll-bar-mode -1)

;; Linhas numeradas
(global-display-line-numbers-mode t)

;; Tamanho da fonte
(set-face-attribute 'default nil :height 100)

;; Pacotes
(require 'package)


;; Melpa - repositorio
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents) 
  (package-install 'use-package) )

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (global-set-key [f8] 'neotree-toggle)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))) ;; me movimentar entre varias janelas

(use-package autothemer
  :ensure t)

;; LSP

(use-package company
  :ensure t
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))
  :hook (elixir-mode . company-mode))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
   (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (python-mode . lsp)
  :hook (c-mode . lsp)
  :hook (c++-mode . lsp)
  :init
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-pylsp-plugins-pylint-args '("--disable=C0114,C0115,C0116"))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'c-mode 'lsp)
  (add-hook 'cpp-mode 'lsp)
  (setq lsp-enable-file-watchers nil))
 

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-flycheck-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package toml-mode :ensure t) 

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol
  :ensure t) 
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol
  :ensure t) 
(use-package lsp-treemacs :commands lsp-treemacs-errors-list
  :ensure t)

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package kuronami-theme
  :ensure t
  :config (load-theme 'kuronami t))

(use-package flycheck
  :ensure t
  :init  (global-flycheck-mode t)
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-clang-args "-std=c++20")) 

(electric-pair-mode 1) ;; ativar auto close

(use-package dap-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :hook (elixir-mode . smartparens-mode)
  :config
  (sp-with-modes '(elixir-mode)
		 (sp-local-pair "do" "end" :actions '(insert))))

;;Atalhos personalizados
(global-set-key (kbd "C-<tab>") 'other-window) ;; navegar entre as janelas
(global-set-key (kbd "M-<down>") 'enlarge-window) ;; diminuir a janela alt + seta pra baixo
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c C-SPC") 'set-mark-command) ;; selecionar uma area
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("77f1e155387d355fbbb3b382a28da41cc709b2a1cc71e7ede03ee5c1859468d2" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(flycheck-credo smartparens flycheck typescript-mode elixir-mode lsp-haskell dap-mode lsp-treemacs lsp-ivy helm-lsp toml-mode yasnippet lsp-ui lsp-mode rustic company kanagawa-theme autothemer ace-window neotree all-the-icons auto-complete which-key try)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
