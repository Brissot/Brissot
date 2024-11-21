;; stop the annoying start screen from starting
(setq inhibit-startup-screen t)
(setq initial-startup-message "")

;; no top bars
(tool-bar-mode 0)
(menu-bar-mode 0)

;; 2 spaces only, ok?
(setq standard-indent 2)
(setq tab-width 2)
(setq indent-tabs-mode nil)

;; give me column numbers
(setq column-number-mode t)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default show-trailing-whitespace t)

;; remember
(save-place-mode 1)
(setq backup-directory-alist `(("." . "~/.config/emacs/backups")))
(setq auto-save-list-file-prefix "~/.config/emacs/autosave")
(setq auto-save-file-name-transforms
      '((".*" "~/.config/emacs/autosave" t)))

;; mouse and system integration stuff
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq mouse-wheel-flip-direction t)
(setq mouse-wheel-tilt-scroll t)
(setq mouse-yank-at-point t)
(setq select-enable-clipboard t)

;; 80 column line
(setq-default display-fill-column-indicator-column 80) ;; emacs 0 indexed cols
(setq-default display-fill-column-indicator-character ?🖤)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; completions
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; solarized light <3 - just let custom.el manage this section
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(solarized-light))
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     default)))

;; apple's font  - let custom.el manage this part
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "SF Mono" :foundry "APPL"
                :slant normal :weight normal :height 158 :width normal)))))


;;;;
;;    leaving "emacs out of the box" area
;;;;

;; melpa package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; no littering!
(require 'no-littering)
(let ((dir (no-littering-expand-var-file-name "lock-files/")))
 (make-directory dir t)
 (setq lock-file-name-transforms `((".*" ,dir t))))

;; all languages
(require 'lsp-mode)
(use-package lsp-ui :commands lsp-ui-mode)

;; javascript
;; maybe worth inspecting in the future:
;;   https://wavesurfer.xyz/blog/emacs-javascript
; (require 'js2-mode)
; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'lsp-deferred)

;; python
;; https://slinkp.com/python-emacs-lsp-20231229.html
(add-hook 'python-mode-hook 'lsp-deferred)
