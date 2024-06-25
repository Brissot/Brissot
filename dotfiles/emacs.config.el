;; stop the annoying start screen from starting
(setq inhibit-startup-screen t)
(setq initial-startup-message "")

;; 2 spaces only, ok?
(setq standard-indent 2)
(setq column-number-mode t)
(setq tab-width 2)
(setq indent-tabs-mode nil)

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

;; no littering!
(require 'no-littering)
(let ((dir (no-littering-expand-var-file-name "lock-files/")))
 (make-directory dir t)
 (setq lock-file-name-transforms `((".*" ,dir t))))

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
