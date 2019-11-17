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
  :config
  (evil-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package dracula-theme
    :config
    (load-theme 'dracula t))

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini))
(use-package helm-projectile
  :config (helm-projectile-on))

(use-package magit)
(use-package smartparens
  :config (require 'smartparens-config)
  :hook ((emacs-lisp-mode . smartparens-mode)))
(use-package recentf
  :config (recentf-mode 1)
  :bind ("C-x r" . 'helm-recentf))

(use-package company
  :init (global-company-mode))

(use-package flycheck
  :config
  (setq
   flycheck-display-errors-delay 1
   flycheck-highlighting-mode 'lines
   flycheck-check-syntax-automatically '(save))
  :init (global-flycheck-mode))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook
	    '(lambda ()
	       (setq indent-tabs-mode nil)
	       (setq python-indent 4)
	       (setq tab-width 4))))
(use-package jedi-core)
(use-package company-jedi
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  :hook (python-mode-hook . my/python-mode-hook))

(use-package lsp-mode
  :hook ((python-mode . lsp))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)
(use-package company-lsp
  :config (push 'company-lsp company-backends)
  :commands comapny-lsp)
