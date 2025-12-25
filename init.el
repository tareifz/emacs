;;; package --- Summary
;;; Commentary:

;; ████████╗░█████╗░██████╗░███████╗██╗███████╗███████╗
;; ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██║██╔════╝╚════██║
;; ░░░██║░░░███████║██████╔╝█████╗░░██║█████╗░░░░███╔═╝
;; ░░░██║░░░██╔══██║██╔══██╗██╔══╝░░██║██╔══╝░░██╔══╝░░
;; ░░░██║░░░██║░░██║██║░░██║███████╗██║██║░░░░░███████╗
;; ░░░╚═╝░░░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝╚═╝╚═╝░░░░░╚══════╝

;; Copyright (C) 2025 Tareif Al-Zamil <root@tareifz.me>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;; Created At: 2024-03-11

;;; Tareifz Emacs config

(require 'package)

;;; Code:
(setq package-archives
			'(("gnu" . "https://elpa.gnu.org/packages/")
				("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
	(progn
		(package-refresh-contents)
		(package-install 'use-package)))

(require 'use-package)
(require 'bind-key)

(setq use-package-always-ensure t)

(use-package emacs
	:config
	(add-to-list 'custom-theme-load-path
							 (concat user-emacs-directory "themes"))
	(add-to-list 'load-path (concat user-emacs-directory "lisp"))

	;; General Emacs configs
	(setq user-full-name "Tareif Al-Zamil")
	(setq user-mail-address "root@tareifz.me")
	(setq inhibit-splash-screen 1)
	(setq initial-scratch-message nil)
	(setq initial-major-mode 'fundamental-mode)
	(setq bookmark-save-flag 1)
	(setq make-backup-files nil)
	(setq backup-inhibited t)
	(setq auto-save-default nil)

	;; Set UTF-8 encoding.
	(set-language-environment 'utf-8)
	(set-terminal-coding-system 'utf-8)
	(setq locale-coding-system 'utf-8)
	(set-default-coding-systems 'utf-8)
	(set-selection-coding-system 'utf-8)
	(prefer-coding-system 'utf-8)
	;; y or n
	(fset 'yes-or-no-p 'y-or-n-p)
	;; use ibuffer
	(defalias 'list-buffers 'ibuffer)

	(electric-pair-mode t) ;; enable only for non-lisps
	(show-paren-mode t)
	(delete-selection-mode t)
	(global-hl-line-mode t)
	(global-auto-revert-mode t)
	(global-display-line-numbers-mode t)

	(setq-default ring-bell-function 'ignore)
	(setq-default indent-tabs-mode t)
	(setq-default tab-width 2)
	(setq-default default-tab-width 2)
	(setq-default tab-always-indent nil)
	(setq-default js-indent-level 2)

	;; MacOS remapping
	(setopt mac-option-modifier nil)
	(setopt mac-command-modifier 'meta)
	(setopt mac-right-option-modifier nil)
	(setopt mac-right-command-modifier 'meta)

	(setq-default custom-file
								(concat user-emacs-directory
												"auto-generated-customized-settings.el"))

	(unless (file-exists-p custom-file)
		(with-temp-buffer (write-file custom-file)))
	(load-file custom-file)


	(when (daemonp)
		(add-hook 'after-make-frame-functions
							(lambda (frame)
								(select-frame frame)
								(tz/set-ui))))

	(tz/set-ui)
	:preface
	(defun tz/load-only-theme ()
		"Disable all themes and then load a single theme interactively."
		(interactive)
		(while custom-enabled-themes
			(disable-theme (car custom-enabled-themes)))
		(call-interactively 'load-theme))

	(defun tz/comment-file ()
		"Comment All the file lines."
		(comment-region (point-min)
										(point-max)))

	(defun tz/insert-file-template ()
		"Insert template file into the current buffer when the buffer is empty."
		(when (and (= (point-max) (point-min))
							 (not (string= (buffer-name) "*scratch*")))
			(let* ((filename (buffer-name))
						 (current-year (format-time-string "%Y"))
						 (current-date (format-time-string "%Y-%m-%d"))
						 (filename-without-extension (string-remove-suffix ".el" filename)))
				(insert-file-contents (concat user-emacs-directory "templates/general-template.txt"))
				(replace-regexp-in-region "<%filename%>" filename)
				(replace-regexp-in-region "<%current-year%>" current-year)
				(replace-regexp-in-region "<%current-date%>" current-date)
				(replace-regexp-in-region "<%filename-without-extention%>" filename-without-extension)
				(tz/comment-file))))

	(defun tz/set-ui ()
		"Set UI settings (fonts, hide bars, ...)."
		(interactive)
		(menu-bar-mode -1)
		(tool-bar-mode -1)
		(toggle-scroll-bar -1)
		;; (set-frame-font "Fira Code")
		(set-frame-font "Gitlab Mono")
		(set-face-attribute 'default nil :height 120))

	(defun tz/indent-buffer ()
		"Indnet buffer."
		(interactive)
		(indent-region (point-min) (point-max)))

	(defun tz/tabify-buffer ()
		"Convert buffer spaces to tabs."
		(interactive)
		(tabify (point-min) (point-max)))

	(defun tz/untabify-buffer ()
		"Convert buffer tabs to spaces."
		(interactive)
		(untabify (point-min) (point-max)))

	:hook
	((before-save     . whitespace-cleanup)
	 (before-save     . (lambda () (delete-trailing-whitespace)))
	 (before-save     . tz/indent-buffer)
	 (prog-mode       . (lambda () (setq show-trailing-whitespace t)))
	 (crystal-mode    . tz/insert-file-template)
	 (clojure-mode    . tz/insert-file-template)
	 (emacs-lisp-mode . tz/insert-file-template)
	 (lisp-mode       . tz/insert-file-template)))

(use-package whitespace
	:diminish whitespace-mode
	:config
	(setopt whitespace-style '(face trailing tabs))
	(custom-set-faces
	 '(whitespace-tab ((t (:background "red")))))
	(global-whitespace-mode t)	;; need adjustments
	)

(use-package align
	:bind
	("C-x a a" . align-entire)
	:preface
	(defun align-with-spaces (ogfn &rest args)
		(let ((indent-tabs-mode nil))
			(apply ogfn args)))
	:config
	(advice-add 'align :around #'align-with-spaces))

(use-package try)
(use-package rust-mode)
(use-package typescript-mode)
(use-package crystal-mode)
(use-package zig-mode)
(use-package fish-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package clojure-mode)
(use-package eldoc
	:diminish eldoc-mode)

(use-package diminish
	:config
	(diminish 'hi-lock-mode))

(use-package rainbow-mode
	:diminish rainbow-mode
	:hook (prog-mode . rainbow-mode)
	:config
	(setopt rainbow-x-colors nil))

(use-package rainbow-delimiters
	:requires rainbow-mode
	:hook (prog-mode . rainbow-delimiters-mode))

(use-package diff-hl
	:config
	(global-diff-hl-mode)
	(diff-hl-flydiff-mode))

(use-package hl-todo
	:config
	(global-hl-todo-mode))

(use-package highlight-thing
	:diminish highlight-thing-mode
	:hook
	(prog-mode . highlight-thing-mode))

(use-package switch-window
	:bind
	("C-x o" . switch-window))

(use-package ido-vertical-mode
	:config
	(setq ido-enable-flex-matching t)
	(setq ido-everywhere t)
	(setq ido-vertical-show-count t)
	(setq ido-use-faces t)
	(set-face-attribute 'ido-vertical-first-match-face nil
											:background nil
											:foreground "orange")
	(set-face-attribute 'ido-vertical-only-match-face nil
											:background nil
											:foreground nil)
	(set-face-attribute 'ido-vertical-match-face nil
											:foreground nil)
	(ido-mode 1)
	(ido-vertical-mode 1))

(use-package smex
	:bind
	("M-x"         . smex)
	("M-X"         . smex-major-mode-commands)
	;; old M-x
	("C-c C-c M-x" . execute-extended-command))

(use-package which-key
	:diminish which-key-mode
	:config
	(which-key-mode)
	(which-key-setup-side-window-right-bottom))

(use-package docker
	:ensure t
	:bind ("C-c d" . docker))

(use-package yascroll
	:config
	(global-yascroll-bar-mode 1))

(use-package auto-package-update
	:config
	(auto-package-update-maybe))

(use-package expand-region
	:bind ("C-=" . er/expand-region))

(use-package multiple-cursors
	:bind
	("C->"     . mc/mark-next-like-this)
	("C-<"     . mc/mark-previous-like-this)
	("C-c C-<" . mc/mark-all-like-this))

(use-package paredit
	:hook
	(emacs-lisp-mode . paredit-mode)
	(lisp-mode       . paredit-mode)
	(scheme-mode     . paredit-mode)
	(clojure-mode    . paredit-mode))

(use-package aggressive-indent
	:disabled
	:hook
	(emacs-lisp-mode . aggressive-indent-mode)
	(lisp-mode       . aggressive-indent-mode)
	(clojure-mode    . aggressive-indent-mode)
	(scheme-mode     . aggressive-indent-mode)
	(sly-mode        . aggressive-indent-mode))

;; this package is used to move lines up and down
;; and move words right and left
(use-package drag-stuff
	:diminish drag-stuff-mode
	:config
	(drag-stuff-global-mode 1)
	(drag-stuff-define-keys))

(use-package restclient
	:config
	(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode)))

(use-package almost-mono-themes
	:disabled
	:config
	(load-theme 'almost-mono-cream t)
	(set-face-attribute 'mode-line nil :box nil))

(use-package ef-themes
	:config
	(load-theme 'ef-summer t)
	;; (load-theme 'ef-tritanopia-light t)
	)

;;; init.el ends here
