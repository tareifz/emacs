;;; package --- Summary
;;; Commentary:

;; ████████╗░█████╗░██████╗░███████╗██╗███████╗███████╗
;; ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██║██╔════╝╚════██║
;; ░░░██║░░░███████║██████╔╝█████╗░░██║█████╗░░░░███╔═╝
;; ░░░██║░░░██╔══██║██╔══██╗██╔══╝░░██║██╔══╝░░██╔══╝░░
;; ░░░██║░░░██║░░██║██║░░██║███████╗██║██║░░░░░███████╗
;; ░░░╚═╝░░░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝╚═╝╚═╝░░░░░╚══════╝

;; Copyright (C) 2024 Tareif Al-Zamil <root@tareifz.me>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;; Created At: 2024-03-10

;; ui related settings

;;; Code:

(defun tz/set-ui ()
  "Set UI settings (fonts, hide bars, ...)."
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (set-frame-font "Fira Code")
  (set-face-attribute 'default nil :height 150))

(defun tz/load-only-theme ()
  "Disable all themes and then load a single theme interactively."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (call-interactively 'load-theme))

(use-package diminish
  :config
  (diminish 'hi-lock-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (setq rainbow-x-colors nil))

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

(use-package almost-mono-themes
  :config
  (load-theme 'almost-mono-cream t)
  (set-face-attribute 'mode-line nil :box nil))

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;;; ui.el ends here
