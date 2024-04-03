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


;; Created At: 2024-04-02

;; Completion framework configurations.

;;; Code:

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)         ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)          ;; Enable auto completion
  (corfu-separator ?\s)   ;; Orderless field separator
  (corfu-scroll-margin 5) ;; Use scroll margin
  (corfu-echo-mode t)     ;; Show candidate documentation in echo area
  (corfu-echo-delay 0)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  :init
  (global-corfu-mode))

;; icons before candidate name.
(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; https://github.com/minad/tempel   !important
;; (use-package temple)

;;; completion.el ends here.
