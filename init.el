;; ~*~ lexical-binding: t; no-byte-compile: t ~*~

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-screen t)

(set-face-attribute 'default nil :font "mononoki Nerd Font" :height 110)
(set-face-attribute 'fixed-pitch nil :font "mononoki Nerd Font" :height 110)

(set-face-attribute 'variable-pitch nil :font "Splatoon2:antialias=true" :height 120)

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

(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

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
  :hook (org-mode . variable-pitch-mode)
  :hook (org-mode . flyspell-mode)
  :config
  (require 'org-tempo)
  (setq org-hide-emphasis-markers t)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit fixed-pitch shadow))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))))
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom (org-modern-block-fringe nil))

(use-package visual-fill-column
  :custom (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode)
  :init
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package org-appear
  :straight (org-appear :type git
		      :host github
		      :repo "awth13/org-appear")
  :init
  (setq org-appear-trigger 'manual
	org-appear-autolinks t)
  :hook (org-mode . org-appear-mode)
  :hook (org-mode . (lambda ()
		      ;; This is necessary for org-appear to
		      ;; play nicely with Evil Mode.
		      (add-hook 'evil-insert-state-entry-hook
				#'org-appear-manual-start
				nil
				t)
		      (add-hook 'evil-insert-state-exit-hook
				#'org-appear-manual-stop
				nil
				t))))

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
  :hook (after-init . global-flycheck-mode)
  :hook ((org-src-mode emacs-lisp-mode)
	 .
	 (lambda ()
		       (when (or (bound-and-true-p org-src-mode)
				 (string= (buffer-name) "*scratch*"))
			 (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))))))

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

(defun prisco/flyspell-save-word-to-personal-dict ()
  (interactive)
  (let ((current-location (point))
	(word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save
			   nil
			   (car word)
			   current-location
			   (cadr word)
			   (caddr word)
			   current-location))))

(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(add-hook 'prog-mode-hook 'electric-pair-local-mode)

(use-package rustic)

(use-package elm-mode
  :hook (elm-mode . elm-format-on-save-mode))

(use-package sly
  :config
  (let ((roswell-exec (executable-find "ros")))
    (when roswell-exec
      (load (expand-file-name "~/.roswell/helper.el"))
      (setq inferior-lisp-program "ros -Q run"))))

(use-package auctex
  :hook ((tex-mode . visual-line-mode)
	 (tex-mode . visual-fill-column-mode)
	 (tex-mode . flyspell-mode))
  :config
  (setq TeX-auto-save t
	TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable)
  (setq-default pdf-latex-command "xelatex"))

(use-package lsp-latex
  :after (lsp auctex)
  :hook ((tex-mode latex-mode) . lsp))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(general-create-definer prisco/leader-def
  :states '(normal visual emacs)
  :prefix prisco/keymap-leader
  :non-normal-prefix prisco/keymap-global-leader)

(general-create-definer prisco/localleader-def
  :states '(normal visual emacs)
  :prefix prisco/keymap-local-leader
  :non-normal-prefix prisco/keymap-global-local-leader)

(prisco/leader-def
  "f"  '(:ignore t :wk "Find file...")
  "ff" 'counsel-find-file
  "fr" 'counsel-recentf
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
  "ei" '(org-edit-special :wk "Edit item in structure")
  "de" '(org-export-dispatch :wk "Export File..."))

(prisco/localleader-def
  :keymaps 'org-src-mode-map
  "cq" '(org-edit-src-exit :wk "Save and exit buffer")
  "ca" '(org-edit-src-abort :wk "Discard edits and exit buffer"))

(prisco/localleader-def
  :keymaps 'flyspell-mode-map
  "fs"  '(:ignore t :wk "Flyspell...")
  "fsb" '(flyspell-buffer :wk "Scan buffer")
  "fsd" '(prisco/flyspell-save-word-to-personal-dict :wk "Save word")
  "fsc" '(flyspell-correct-wrapper :wk "Correct word"))
