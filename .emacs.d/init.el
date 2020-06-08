(setq user-full-name "Takuro Niitsuma"
      user-mail-address "bakednt@gmail.com")

(setq initial-major-mode 'org-mode
      initial-scratch-message (format-time-string "<%Y/%m/%d> \n" (current-time))
      inhibit-startup-screen t
      inhibit-startup-message t)

(add-hook 'emacs-startup-hook 'load-init-settings)
(add-hook 'emacs-startup-hook 'enable-to-copy-and-paste-on-system)
(add-hook 'emacs-startup-hook 'enable-to-move-window-with-key)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))
(use-package evil-collection
  :after evil
  :init (evil-collection-init))
(use-package evil-magit :after evil)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-ff-lynx-style-map t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match    t)
  (helm-mode 1)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini))
(use-package helm-tramp
  :after helm
  :config (setq tramp-default-method "ssh")
  :bind ("C-x C-s" . 'helm-tramp))
(use-package helm-gtags
  :after helm
  :config (setq helm-gtags-path-style 'root)
  :hook ((python-mode . helm-gtags-mode)
	 (enh-ruby-mode . helm-gtags-mode))
  :bind (("M-t" . 'helm-gtags-find-tag)
	 ("M-r" . 'helm-gtags-find-rtag)
	 ("M-s" . 'helm-gtags-find-symbol)
	 ("C-t" . 'helm-gtags-pop-stack)
	 ("C-x t" . 'helm-gtags-select)))
(use-package helm-dash :if (memq window-system '(mac)))

(use-package helm-projectile
  :defer t
  :config (helm-projectile-on)
  :bind (("C-c p" . helm-projectile)
	 ("s-p" . helm-projectile)))
(use-package projectile-rails
  :defer t
  :hook (projectile-mode . projectile-rails-mode)
  :bind ("C-c r" . projectile-rails-command-map))
(use-package slim-mode
  :defer t
  :mode ("\\.slim\\'" . slim-mode))

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(use-package gtags :defer t)
(use-package magit :defer t)
(use-package forge
  :defer t
  :after magit)
(use-package quickrun :defer t)
(use-package anzu :init (global-anzu-mode +1))
; (use-package whitespace :init (global-whitespace-mode 1))
(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (migemo-init)
  (setq migemo-command "cmigemo"
	migemo-options '("-q" "--emacs")
	migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"
	migemo-user-dictionary nil
	migemo-regex-dictionary nil
	migemo-coding-system 'utf-8-unix))

(use-package smartparens
  :init (smartparens-global-mode t)
  :config (require 'smartparens-config))
(use-package recentf
  :init (recentf-mode 1)
  :bind ("C-x C-r" . 'helm-recentf))
(use-package company :init (global-company-mode))
(use-package yasnippet :init (yas-global-mode 1))
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'symbols
	flycheck-indication-mode 'left-fringe
	flycheck-disabled-checkers nil
	flycheck-check-syntax-automatically '(save mode-enabled)))
(use-package expand-region :config (define-key evil-visual-state-map (kbd "C-v") #'er/expand-region))
(use-package popwin :init (popwin-mode 1))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq electric-indent-local-mode 1
	indent-tabs-mode nil
	python-indent-guess-indent-offset-verbose nil
	python-indent-offset 4
	python-indent 4
	tab-width 4))

;; (use-package pipenv
;;   :defer t
;;   :if (equal major-mode 'python-mode)
;;   :hook (python-mode . pipenv-mode)
;;   :custom (pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))
(use-package poetry
  :defer t
  :if (equal major-mode 'python-mode))

(use-package ein
  :defer t
  :config
  (require 'ein-notebook)
  (require 'ein-subpackages)
  (require 'ob-ein))

(use-package enh-ruby-mode
  :defer t
  :mode (("\\.rb\\'" . enh-ruby-mode)
	 ("Gemfile" . enh-ruby-mode))
  :interpreter ("ruby" . enh-ruby-mode))

(use-package cc-mode :defer t)
(use-package tuareg :defer t)

(use-package rust-mode
  :defer t
  :mode (("\\.rs\\'" . rust-mode))
  :config (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(use-package lsp-mode
  :defer t
  :hook ((python-mode . lsp)
	 (enh-ruby-mode . lsp)
	 (c++-mode . lsp)
	 (tuareg-mode . lsp))
  :commands (lsp)
  :config
  (setq lsp-prefer-flymake nil
	lsp-auto-guess-root t
	lsp-enable-xref t
	lsp-enable-snippet t
	lsp-document-sync-method 'incremental
	lsp-ocaml-lang-server-command "ocamllsp"))
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (require 'lsp-ui-flycheck)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable t
	lsp-ui-peek-enable t
	lsp-ui-peek-peek-height 20
	lsp-ui-peek-list-width 50
	lsp-ui-peek-fontify 'on-demand
	lsp-ui-doc-header t
	lsp-ui-flycheck-enable t
	lsp-ui-sideline t
	lsp-ui-doc-include-signature t
	lsp-pyls-plugins-pycodestyle-enabled nil
	lsp-pyls-plugins-pylint-enabled nil
	lsp-pyls-plugins-pyflakes-enabled t
	lsp-pyls-plugins-jedi-environment t
	lsp-pyls-plugins-jedi-use-pyenv-environment t
	lsp-pyls-plugins-preload-modules t))
(use-package helm-lsp
  :after helm
  :defer t
  :commands helm-lsp-workspace-symbol
  :bind (("C-x p w" . 'helm-lsp-workspace-symbol)
	 ("C-x p g" . 'helm-lsp-global-workspace-symbol)))
(use-package company-lsp
  :config (push 'company-lsp company-backends)
  :commands comapny-lsp)
(use-package smart-jump
  :config (smart-jump-setup-default-registers))

(use-package todoist :defer t)
(use-package twittering-mode :defer t)

(use-package scss-mode
  :defer t
  :config (setq css-indent-offset 2))

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'")
  :config
  (setq markdown-command "github-markup"
	markdown-command-needs-filename t
	markdown-cotent-type "application/xhtml+xml"))
(use-package markdown-mode+
  :if (equal major-mode 'markdown-mode)
  :defer t)

(use-package yaml-mode
  :defer t
  :mode ("\\.ya?ml\\'"))

(use-package monokai-theme)

(use-package mew
  :defer t
  :config
  (setq mew-name "Takuro Niitsuma"
	mew-user "niitsuma.takuro.nm3"
	mew-ssl-verify-level 0
	mew-use-cached-passwd t
	mew-mailbox-type "imap"
	mew-login-domain "mailbox.naist.jp"
	mew-proto "%"
	mew-imap-user "takuro-n"
	mew-imap-server "mailbox.naist.jp"
	mew-imap-auth nil
	mew-imap-ssl t
	mew-imap-ssl-port 993
	mew-smtp-user "niitsuma.takuro.nm3@is.naist.jp"
	mew-smtp-server "mailpost.naist.jp"
	mew-smtp-auth nil
	mew-smtp-port "submission")
  (evil-make-overriding-map mew-message-mode-map 'normal))

(use-package w3m
  :defer t
  :config
  (require 'mew-w3m)
  (setq mew-prog-text/html 'w3m-region
	mew-prog-text/xml 'w3m-region
	mew-use-text/html t
	mew-mime-multipart-alternative-list '("Text/Html" "Text/Plain" ".*")
	mew-file-max-size 10000000)
  :hook (mew-message-hook . w3m-minor-mode))

(use-package dap-mode
  :config
  (autoload 'dap-python "python-mode" nil t))

(defun load-init-settings ()
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (global-hl-line-mode +1)
  (line-number-mode +1)
  (global-display-line-numbers-mode 1)
  (column-number-mode t)
  (size-indication-mode t)
  (menu-bar-mode -1)
  (show-paren-mode 1)

  (load-theme 'monokai t)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq display-line-numbers t)
  (setq undo-no-redo t)

  (setq abbrev-file-name "~/.abbrev_defs"
	save-abbrevs t)
  (define-key global-map (kbd "C-x C-n") 'open-now-todo-org))

(defun enable-to-move-window-with-key ()
  (windmove-default-keybindings)
  (global-set-key (kbd "C-x |")  'split-window-horizontally)
  (global-set-key (kbd "C-x -")  'split-window-vertically)
  (global-set-key (kbd "C-x x")  'delete-window))

(defun enable-to-copy-and-paste-on-system ()
  (setq select-enable-clipboard t
	interprogram-cut-function 'paste-to-osx
	interprogram-paste-function 'copy-from-osx))
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun open-now-todo-org ()
  (interactive)
  (find-file-other-window (expand-file-name "~/.todo-now.org")))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7675ffd2f5cb01a7aab53bcdd702fa019b56c764900f2eea0f74ccfc8e854386" default))))
(custom-set-faces

 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
