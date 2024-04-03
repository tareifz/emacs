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


;; Created At: 2024-03-11

;; Customizations related to editing files.

;;; Code:

(use-package paredit
  :hook
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (scheme-mode . paredit-mode)
  (clojure-mode . paredit-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode)
  (clojure-mode . aggressive-indent-mode)
  (scheme-mode . aggressive-indent-mode)
  (sly-mode . aggressive-indent-mode))

;; this package is used to move lines up and down
;; and move words right and left
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;;; editing.el Ends here.
