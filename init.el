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

  ;; General Emacs configs
  (setq user-full-name "Tareif Al-Zamil"
        user-mail-address "root@tareifz.me"
        inhibit-splash-screen 1
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode
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

  (defun tz/set-ui ()
  "Set UI settings (fonts, hide bars, ...)."
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (set-frame-font "Fira Code Medium")
  (set-face-attribute 'default nil :height 110))

  (defun tz/load-only-theme ()
    "Disable all themes and then load a single theme interactively."
    (interactive)
    (while custom-enabled-themes
      (disable-theme (car custom-enabled-themes)))
    (call-interactively 'load-theme))

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
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
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
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;; (use-package paredit
;;   :hook
;;   (emacs-lisp-mode . paredit-mode)
;;   (lisp-mode . paredit-mode)
;;   (scheme-mode . paredit-mode)
;;   (clojure-mode . paredit-mode))

;; (use-package aggressive-indent
;;   :hook
;;   (emacs-lisp-mode . aggressive-indent-mode)
;;   (lisp-mode . aggressive-indent-mode)
;;   (clojure-mode . aggressive-indent-mode)
;;   (scheme-mode . aggressive-indent-mode)
;;   (sly-mode . aggressive-indent-mode))

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode)))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; (use-package almost-mono-themes
;;   :config
;;   (load-theme 'almost-mono-cream t)
;;   (set-face-attribute 'mode-line nil :box nil))
(use-package ef-themes
  :config
  (load-theme 'ef-summer t)
  ;; (load-theme 'ef-tritanopia-light t)
  )

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; https://github.com/minad/tempel   !important
;; (use-package temple)

;;; init.el ends here
