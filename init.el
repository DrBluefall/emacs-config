(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-screen t)

(set-face-attribute 'default nil :font "mononoki Nerd Font" :height 110)
(set-face-attribute 'fixed-pitch nil :font "mononoki Nerd Font" :height 110)

(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110)

(add-to-list 'default-frame-alist '(alpha-background . 85))

(setq-default fill-column 100)

(defvar prisco/user-local-directory
  (expand-file-name ".local/" user-emacs-directory))
(unless (file-directory-p prisco/user-local-directory)
  (make-directory prisco/user-local-directory))

(defvar prisco/keymap-leader "SPC")
(defvar prisco/keymap-local-leader "SPC m")
(defvar prisco/keymap-global-leader "C-c SPC")
(defvar prisco/keymap-global-local-leader "C-c SPC m")

(setq byte-compile-warnings nil)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq epg-pinentry-mode 'loopback)

(defvar straight-base-dir
  (expand-file-name "share/" prisco/user-local-directory))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
	use-package-always-defer t)

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory
	(expand-file-name "etc" prisco/user-local-directory)
	no-littering-var-directory
	(expand-file-name "var" prisco/user-local-directory)
	custom-file
	(no-littering-expand-etc-file-name "custom.el")))

(use-package general
  :demand t)

(use-package which-key
  :demand t
  :hook (after-init . which-key-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 40))

(use-package doom-themes
  :demand t
  :init
  (setq custom-theme-directory
	(concat (expand-file-name user-emacs-directory)
		"themes"))
  (load-theme 'doom-priscode t))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :hook (after-init . evil-mode))

(use-package evil-collection
  :after evil
  :hook (evil-mode . evil-collection-init))

(use-package org
  :config
  (require 'org-tempo)
  (setq org-hide-emphasis-markers t))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom (org-modern-block-fringe nil))

(use-package visual-fill-column
  :custom (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode)
  :init
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package mixed-pitch
  :hook ((org-mode text-mode) . mixed-pitch-mode))

(defun prisco/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
			(expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
	(org-babel-tangle))))
(add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook
			#'prisco/org-babel-tangle-config)))

(use-package ivy
  :hook (after-init . ivy-mode))

(use-package ivy-rich
  :after ivy
  :hook (after-init . ivy-rich-mode))

(use-package counsel
  :after ivy
  :hook (after-init . counsel-mode))

(use-package helpful
  :config
  (with-eval-after-load 'counsel
    (setq counsel-describe-function-function #'helpful-function
	  counsel-describe-variable-function #'helpful-variable)))

(use-package company
  :hook ((prog-mode . company-mode)
	 (text-mode . company-mode))
  :bind (("TAB" . #'company-indent-or-complete-common)
	 :map company-active-map
	 ("TAB" . #'company-complete-common-or-cycle)
	 ("<backtab>" . (lambda ()
			  (interactive)
			  (company-complete-common-or-cycle -1))))
  :config
  (setq company-idle-delay 0.5))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package lsp-mode
  :hook ((prog-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))

  :init
  (setq lsp-keymap-prefix (concat prisco/keymap-leader " c l")))

(use-package lsp-ui)

(use-package projectile
  :hook (after-init . projectile-mode))

(use-package magit)

(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t
	vterm-always-compile-module t))

(use-package rustic)

(use-package elm-mode
  :hook (elm-mode . elm-format-on-save-mode))

(general-create-definer prisco/leader-def
  :states '(normal visual emacs)
  :prefix prisco/keymap-leader
  :global-prefix prisco/keymap-global-leader)

(general-create-definer prisco/localleader-def
  :states '(normal visual emacs)
  :prefix prisco/keymap-local-leader
  :global-prefix prisco/keymap-global-local-leader)

(prisco/leader-def
  "f"  '(:ignore t :wk "Find file...")
  "ff" 'find-file
  "fC" '((lambda ()
	   (interactive)
	   (find-file (expand-file-name "README.org" user-emacs-directory)))
	 :wk "Open emacs config"))

(prisco/leader-def
  "q" '(:ignore t :wk "Quit...")
  "qq" '(kill-emacs :wk "Quit Emacs")
  "h" '(:ignore t :wk "Help...")
  "hf" 'helpful-function
  "hv" 'helpful-variable
  "hk" 'helpful-key
  "hx" 'helpful-command
  "o"  '(:ignore t :wk "Open...")
  "ot" 'vterm
  "oT" 'vterm-other-window
  "of" 'make-frame-command)

(prisco/leader-def
  :keymaps 'projectile-mode-map
  "p" 'projectile-command-map)

;; Needed because of lsp-command-map being weird compared to projectile-command-map.
(prisco/leader-def
  :package 'lsp
  :definer 'minor-mode
  :keymaps 'lsp-mode
  "cl" '(:keymap lsp-command-map))

(prisco/leader-def
  "g" '(:ignore t :wk "Version control...")
  "gs" '(magit-status :wk "Repository status buffer")
  "gc" '(magit-commit-create :wk "Commit"))

(prisco/localleader-def
  :keymaps 'org-mode-map
  "i"  '(:ignore t :wk "Insert structure...")
  "ih" '(org-insert-heading :wk "Insert a heading")
  "ei" '(org-edit-special :wk "Edit item in structure"))

(prisco/localleader-def
  :keymaps 'org-src-mode-map
  "cq" '(org-edit-src-exit :wk "Save and exit buffer")
  "ca" '(org-edit-src-abort :wk "Discard edits and exit buffer"))
