(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-screen t)

(set-face-attribute 'default nil :font "mononoki Nerd Font" :height 110)
(set-face-attribute 'fixed-pitch nil :font "mononoki Nerd Font" :height 110)

(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110)

(add-to-list 'default-frame-alist '(alpha-background . 85))

(defvar prisco/user-local-directory
  (expand-file-name ".local/" user-emacs-directory))
(unless (file-directory-p prisco/user-local-directory)
  (make-directory prisco/user-local-directory))

(defvar prisco/keymap-leader "SPC")
(defvar prisco/keymap-local-leader "SPC m")
(defvar prisco/keymap-global-leader "C-SPC")
(defvar prisco/keymap-global-local-leader "C-SPC m")

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
  (require 'org-tempo))

(defun prisco/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
			(expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
	(org-babel-tangle))))
(add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook
			#'prisco/org-babel-tangle-config)))

(general-create-definer prisco/leader-def
  :keymaps '(normal visual insert emacs)
  :prefix prisco/keymap-leader
  :global-prefix prisco/keymap-global-leader)

(general-create-definer prisco/localleader-def
  :keymaps '(normal visual insert emacs)
  :prefix prisco/keymap-local-leader
  :prefix prisco/keymap-global-local-leader)

(prisco/leader-def
  "f"  '(:ignore t :wk "Find file...")
  "ff" 'find-file)
