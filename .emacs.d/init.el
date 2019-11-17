(setq user-full-name "Takuro Niitsuma"
      user-mail-address "bakednt@gmail.com")

(prefer-coding-system 'utf-8) (set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)
(windmove-default-keybindings)
(global-set-key (kbd "C-x |")  'split-window-horizontally)
(global-set-key (kbd "C-x -")  'split-window-vertically)
(global-set-key (kbd "C-x x")  'delete-window)

(setq select-enable-clipboard t
      interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq abbrev-file-name "~/.abbrev_defs"
      save-abbrevs t)

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

(use-package evil :config (evil-mode 1))
(use-package dracula-theme :config (load-theme 'dracula t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match    t)
  (helm-mode 1)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini))
(use-package helm-tramp
  :config (setq tramp-default-method "ssh")
  :bind ("C-x C-s" . 'helm-tramp))
(use-package helm-projectile
  :config (helm-projectile-on)
  :bind ("C-x p h" . 'helm-projectile))

(use-package magit :defer t)
(use-package smartparens
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config))
(use-package recentf
  :init (recentf-mode 1)
  :bind ("C-x C-r" . 'helm-recentf))
(use-package company :init (global-company-mode))
(use-package yasnippet :init (yas-global-mode 1))
(use-package flycheck
  :config
  (setq
   flycheck-display-errors-delay 1
   flycheck-highlighting-mode 'lines
   flycheck-check-syntax-automatically '(save))
  :init (global-flycheck-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq electric-indent-local-mode 1
	indent-tabs-mode nil
	python-indent 4
	tab-width 4))

(use-package jedi-core
  :defer t
  :config
  (setq jedi:complete-on-dot t
	jedi:use-shortcuts t)
  :hook (python-mode . jedi:setup))
(use-package company-jedi
  :defer t
  :config
  (add-to-list 'company-backends 'company-jedi))
(use-package pipenv
  :defer t
  :hook (python-mode . pipenv-mode)
  :custom (pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))
(use-package ein
  :defer t
  :config
  (require 'ein-notebook)
  (require 'ein-subpackages))

(use-package lsp-mode
  :hook ((python-mode . lsp))
  :commands lsp)
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)
(use-package helm-lsp
  :defer t
  :commands helm-lsp-workspace-symbol)
(use-package company-lsp
  :config (push 'company-lsp company-backends)
  :commands comapny-lsp)

(use-package todoist :defer t)
