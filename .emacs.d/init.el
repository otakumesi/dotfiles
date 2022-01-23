(setq user-full-name "Takuro Niitsuma"
      user-mail-address "bakednt@gmail.com")

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
(use-package evil-commentary
  :init (evil-commentary-mode))
;; (use-package evil-magit :after evil)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-copy-envs '("PATH" "PYTHONPATH" "TASKPAPER_PATH"))
  :config (exec-path-from-shell-initialize))

(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
	ivy-use-selectable-prompt t
	ivy-extra-directories nil
	ivy-height 30
	enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  :bind (("C-s" . 'swiper)
	 ("C-c C-r" . 'ivy-resume)
	 ("C-x b" . 'ivy-switch-buffer)
	 ("C-c v" . 'ivy-push-view)
	 ("C-c V" . 'ivy-pop-view)))
(use-package counsel
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  ("<f1> f" . 'counsel-describe-function)
  ("<f1> v" . 'counsel-describe-variable)
  ("<f1> o" . 'counsel-describe-symbol)
  ("<f1> l" . 'counsel-find-library)
  ("<f2> i" . 'counsel-info-lookup-symbol)
  ("<f2> u" . 'counsel-unicode-char)
  ("C-c g" . 'counsel-git)
  ("C-c j" . 'counsel-git-grep)
  ("C-c k" . 'counsel-rg)
  ("C-c n" . 'counsel-fzf)
  ("C-c j" . 'counsel-file-jump)
  ("C-x l" . 'counsel-locate)
  ("C-S-o" . 'counsel-rhythmbox)
  ("M-y" . 'counsel-yank-pop)
  :after (ivy prescient))
(use-package swiper
  :config
  (setq swiper-include-line-number-in-search t))
(use-package counsel-projectile :defer t :after counsel)
(use-package avy :init (avy-setup-default))
(use-package prescient
  :config
  (setq prescient-sort-full-matches-first t)
  :after ivy)
(use-package ivy-prescient
  :init (ivy-prescient-mode)
  :config (setq ivy-prescient-enable-filtering nil)
  :after (ivy prescient))
(use-package company-prescient
  :init (company-prescient-mode)
  :after (ivy company))
(use-package counsel-gtags
  :defer t)

(use-package projectile
  :defer t
  :init (projectile-mode +1)
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package gtags :defer t)
(use-package magit :defer t)
(use-package forge
  :defer t
  :after magit)
(use-package eldoc
  :defer t
  :hook (prog-mode . eldoc-mode)
  :config
  (defun ad:eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  (advice-add 'eldoc-message :around #'ad:eldoc-message))
(use-package quickrun :defer t)
(use-package anzu :init (global-anzu-mode +1))
; (use-package whitespace
;   :init (global-whitespace-mode 1))
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
  :bind ("C-x C-r" . 'counsel-recentf))
(use-package recentf-ext :defer t)
(use-package company
  :init (global-company-mode)
  :config
  (setq  company-selection-wrap-around t
	 company-idle-delay 0.1
	 company-transformers nil
	 completion-ignore-case t
	 company-backends '((company-files company-keywords company-capf) (company-abbrev company-dabbrev))))
;; (use-package yasnippet :init (yas-global-mode 1))
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'symbols
	flycheck-indication-mode 'left-fringe
	flycheck-disabled-checkers nil
	flycheck-check-syntax-automatically '(save mode-enabled)))
(use-package expand-region :config (define-key evil-visual-state-map (kbd "C-v") #'er/expand-region))
(use-package popwin :init (popwin-mode 1))

(use-package pyvenv
  :defer t)
(use-package poetry
  :defer t
  :commands (poetry poetry-venv-workon))
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :hook (poetry-tracking-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq electric-indent-local-mode 1
	indent-tabs-mode nil
	python-indent-guess-indent-offset-verbose nil
	python-indent-offset 4
	python-indent 4
	tab-width 4))
(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)
	 ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
		("node" . js2-jsx-mode))
  :init
  (setq-default electric-indent-local-mode 1
		indent-tabs-mode nil
		js-indent-level 2
		js2-switch-indent-offset 2
		js2-basic-offset 2
		js2-indent-switch-body t))
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; (use-package ein
;;   :defer t
;;   :config
;;   (require 'ein-notebook)
;;   (require 'ein-subpackages)
;;   (require 'ob-ein))
(use-package stan-mode
  :defer t
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  :config
  (setq stan-indentation-offset 2))
(use-package company-stan
  :hook (stan-mode . company-stan-setup))
(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup))
(use-package flycheck-stan
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup)))

(use-package enh-ruby-mode
  :defer t
  :mode (("\\.rb\\'" . enh-ruby-mode)
	 ("\\.rake\\'" . enh-ruby-mode)
	 ("Rakefile" . enh-ruby-mode)
	 ("Gemfile" . enh-ruby-mode))
  :interpreter ("ruby" . enh-ruby-mode)
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package cc-mode :defer t)
(use-package tuareg :defer t)

(use-package rust-mode
  :defer t
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  (setq rust-format-on-save t))

(use-package cargo
  :defer t
  :if (equal major-mode 'rust-mode))

(use-package go-mode
  :defer t
  :mode (("\\.go\\'" . go-mode))
  :hook ((go-mode . go-eldoc-setup))
  :config (setq gofmt-command "goimports"))

(use-package lsp-python
  :defer t
  :init
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil
	lsp-pylsp-plugins-pylint-enabled nil
	lsp-pylsp-plugins-flake8-enabled nil
	lsp-pylsp-plugins-mccabe-enabled nil
	lsp-pylsp-plugins-pyflakes-enabled nil
	lsp-pylsp-plugins-pylint-enabled nil
	lsp-pylsp-plugins-jedi-environment (vc-root-dir)
	lsp-pylsp-plugins-jedi-completion-fuzzy t
	lsp-pylsp-plugins-preload-modules t))

(use-package lsp-mode
  :defer t
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :hook ((enh-ruby-mode . lsp)
	 (c++-mode . lsp)
	 (rjsx-mode . lsp)
	 (python-mode . lsp)
	 (js2-mode . lsp)
	 (js-mode . lsp)
	 (tuareg-mode . lsp)
	 (go-mode . lsp)
	 (rust-mode . lsp)
	 (tide-mode . lsp))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil
	lsp-response-timeout 5
	lsp-auto-guess-root t
	lsp-enable-xref t
	lsp-enable-snippet t
	lsp-log-io t
	lsp-document-sync-method lsp--sync-incremental
	lsp-solargraph-library-directories '("./.bundle" "~/.rbenv/" "/usr/lib/ruby/" "~/.rvm/" "~/.gem/")
	lsp-rust-server 'rust-analyzer
	lsp-ocaml-lang-server-command "ocamllsp")
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
		    :major-modes '(python-mode)
		    :remote? t
		    :server-id 'pylsp-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("typescript-language-server" "--stdio"))
		    :major-modes '(js-mode)
		    :remote? t
		    :server-id 'js-remote)))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (require 'lsp-ui-flycheck)
  (setq lsp-ui-peek-enable t
	lsp-ui-peek-peek-height 20
	lsp-ui-peek-list-width 50
	lsp-ui-peek-fontify 'on-demand
	lsp-ui-doc-enable t
	lsp-ui-doc-header t
	lsp-ui-doc-enable t
	lsp-ui-doc-include-signature t
	lsp-ui-flycheck-enable t
	lsp-ui-sideline-enable t
	lsp-ui-sideline-show-diagnostics nil
	lsp-ui-sideline-delay 0.5))
(use-package company-lsp
  :config
  (add-to-list 'company-lsp company-backends)
  (setq company-lsp-async t
	company-lsp-cache-candidates t)
  :commands comapny-lsp)

(use-package smart-jump
  :config (smart-jump-setup-default-registers))

(use-package todoist
  :defer t
  :config
  (setq todoist-token (shell-command-to-string "echo $TODOIST_TOKEN")))

(use-package twittering-mode :defer t)

(use-package scss-mode
  :defer t
  :config
  (setq css-indent-offset 2
	indent-tabs-mode nil))

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

;; (use-package yaml-mode
;;   :defer t
;;   :mode ("\\.ya?ml\\'"))

(use-package rjsx-mode
  :defer t
  :mode ("\\(components\\|pages\\)\\/.+\\.js\\'")
  :config
  (setq sgml-basic-offset 2
	js-indent-level 2))

(use-package typescript-mode
  :defer t
  :mode ("\\.tsx?\\'")
  :config
  (setq typescript-indent-level 2
	sgml-basic-offset 2))

(use-package docker
  :defer t
  :bind ("C-c d" . docker))
(use-package dockerfile-mode
  :defer t)
(use-package docker-compose-mode
  :defer t)
(use-package docker-tramp
  :defer t
  :config
  (require 'docker-tramp-compat)
  (setq docker-tramp-use-names t))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (setq company-tooltip-align-annotations t))

(use-package tide
  :defer t
  :hook ((typescript-mode . setup-tide-mode)
	 (before-save . tide-format-before-save)))

(use-package web-mode
  :defer t
  :mode ("\\.tsx\\'")
  :hook (web-mode . (lambda () (when (string-equal "tsx" (file-name-extension buffer-file-name))
		      (setup-tide-mode))))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (setq web-mode-enable-auto-closing t
	web-mode-enable-auto-pairing t
	web-mode-auto-close-style 2
	web-mode-tag-auto-close-style 2
	web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	tab-width 2))

(use-package emmet-mode
  :defer t
  :hook (sgml-mode css-mode)
  :config
  (setq emmet-indentation 2))
(use-package json-mode
  :defer t)

;; (use-package molokai-theme)
(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

(use-package symbol-overlay
  :defer t
  :bind ("M-i" . 'symbol-overlay-put))

(use-package find-file-in-project :defer t)

(use-package dumb-jump
  :defer t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))
(use-package delsel
  :hook (prog-mode . delete-selection-mode))
(use-package dash-at-point
  :defer t
  :bind ("C-c d" . 'dash-at-point)
  :config
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "python"))
  (add-to-list 'dash-at-point-mode-alist '(typescript-mode . "typescript"))
  (add-to-list 'dash-at-point-mode-alist '(javascript-mode . "javascript")))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :config (evil-set-undo-system 'undo-tree))

(use-package which-key
  :init
  (which-key-setup-minibuffer)
  (which-key-mode)
  :config
  (setq which-key-popup-type 'minibuffer
	which-key-use-C-h-commands t
	which-key-idle-delay 0.7
	which-key-sort-order 'which-key-prefix-then-key-order
	which-key-show-remaining-keys t
	which-key-allow-evil-operators t
	which-key-paging-prefixes '("C-x")
        which-key-paging-key "<f5>")
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  :bind (("C-h h" . 'which-key-show-top-level)
	 ("C-h m" . which-key-show-major-mode)))

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

  (add-to-list 'auto-mode-alist '("/\\.machinerc$" . sh-mode))
  (add-to-list 'auto-mode-alist '("/\\.functions$" . sh-mode))
  (add-to-list 'auto-mode-alist '("/\\.aliases$" . sh-mode))
  (add-to-list 'auto-mode-alist '("/\\.env$" . sh-mode))
  (add-to-list 'auto-mode-alist '("/\\.envrc$" . sh-mode))

  (defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
    "Start a program in a subprocess.  Return the process object for it.
Similar to `start-process-shell-command', but calls `start-file-process'."
    ;; On remote hosts, the local `shell-file-name' might be useless.
    (let ((command (mapconcat 'identity args " ")))
      (funcall start-file-process-shell-command name buffer command)))

  (advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

  (eval-after-load 'tramp
    '(progn
       (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
       (add-to-list 'tramp-remote-path "~/.pyenv/shims")
       (add-to-list 'tramp-remote-path "/usr/local/bin/")))

  (defalias 'yes-or-no-p 'y-or-n-p)

   (setq gc-cons-threshold (* 128 1024 1024)
	 garbage-collection-messages t)

  (setq inhibit-eol-conversion t)
  (setq vc-handled-backends '())
  (setq vc-follow-symlinks t)
  (setq tramp-auto-save-directory "/tmp")
  (setq display-line-numbers t)
  (setq css-indent-offset 2)
  (setq show-trailing-whitespace t)

  (setq abbrev-file-name "~/.abbrev_defs"
	save-abbrevs t)
  (define-key global-map (kbd "C-c t") 'open-taskpaper-file))

(defun enable-to-move-window-with-key ()
  (windmove-default-keybindings)
  (global-set-key (kbd "C-x |")  'split-window-horizontally)
  (global-set-key (kbd "C-x -")  'split-window-vertically)
  (global-set-key (kbd "C-x x")  'delete-window))

(defun enable-to-copy-and-paste-on-system ()
  "Enable to sync copy and paste between OSX and Emacs."
  (setq select-enable-clipboard t
	interprogram-cut-function 'paste-to-osx
	interprogram-paste-function 'copy-from-osx))
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  )
(custom-set-faces

 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
