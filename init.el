;;; package --- Summary

;;; Commentary:
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
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'load-path "~/.emacs.d/lisp")

  (load "ui")
  (load "editing")
  (load "completion")

  ;; General Emacs configs
  (setq user-full-name "Tareif Al-Zamil"
        user-mail-address "root@tareifz.me"
        inhibit-splash-screen 1
        initial-scratch-message nil
        initial-major-mode 'emacs-lisp-mode
        bookmark-save-flag 1
        make-backup-files nil
        backup-inhibited t
        auto-save-default nil)
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
  ;;
  (electric-pair-mode 1) ;; enable only for non-lisps
  (show-paren-mode t)
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (global-display-line-numbers-mode t)

  (setq-default linum-format " %d "
                ring-bell-function 'ignore
                indent-tabs-mode nil
                highlight-tabs t
                tab-width 2
                default-tab-width 2
                tab-always-indent nil
                ;; Javescript indent level
                js-indent-level 2
                ;; Custom settings file
                custom-file "~/.emacs.d/auto-generated-customized-settings.el")
  ;; mac os remapping
  (custom-set-variables
   '(mac-option-modifier nil)
   '(mac-command-modifier 'meta)
   '(mac-right-option-modifier nil)
   '(mac-right-command-modifier 'meta))

  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load-file custom-file)

  ;; Vertico configs??
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; corfu
  ;; (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (tz/set-ui))))

  ;; Set the UI
  (tz/set-ui)
  :hook ((before-save . whitespace-cleanup)
         (before-save . (lambda () (delete-trailing-whitespace)))
         (crystal-mode . tz/insert-file-template)
         (clojure-mode . tz/insert-file-template)
         (emacs-lisp-mode . tz/insert-file-template)
         (lisp-mode . tz/insert-file-template))
  :preface
  (defun tz/insert-file-template ()
    "Insert template file into the current buffer when the buffer is empty."
    (when (and (= (point-max) (point-min))
               (not (string= (buffer-name) "*scratch*")))
      (let* ((filename (buffer-name))
             (current-year (format-time-string "%Y"))
             (current-date (format-time-string "%Y-%m-%d"))
             (filename-without-extension (string-remove-suffix ".el" filename)))
        (insert-file-contents (concat "~/.emacs.d/templates/general-template.txt"))
        (replace-regexp-in-region "<%filename%>" filename)
        (replace-regexp-in-region "<%current-year%>" current-year)
        (replace-regexp-in-region "<%current-date%>" current-date)
        (replace-regexp-in-region "<%filename-without-extention%>" filename-without-extension)
        (tz/comment-file))))

  (defun tz/comment-file ()
    "Comment All the file lines."
    (comment-region (point-min)
                    (point-max)))

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args))))

(use-package try)
(use-package rust-mode)
(use-package typescript-mode)
(use-package crystal-mode)
(use-package fish-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package clojure-mode)
(use-package cider)
(use-package eldoc
  :diminish eldoc-mode)

(use-package switch-window
  :bind
  ("C-x o" . switch-window))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

(use-package flycheck
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))

(use-package auto-package-update
  :config
  (auto-package-update-maybe))

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode)))

(use-package sly
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))
(use-package sly-asdf)
(use-package sly-quicklisp)
;; expand macros in file C-c M-e
(use-package sly-macrostep)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

;;; init.el ends here
