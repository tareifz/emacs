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

;; General Emacs configs
(setq use-package-always-ensure t)
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

(global-whitespace-mode t) ;; need adjustments

(setq-default ring-bell-function 'ignore)
(setq-default indent-tabs-mode t)



(default-value 'delete-selection-mode)
