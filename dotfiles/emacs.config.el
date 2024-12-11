;; Sid Su's EMACS Config
;;
;; You may look at this and wonder, "Aren't you just back at VSCODE using this
;; config? You're mostly right, and I do want to remove parts of this config
;; bit by bit, but it's like training wheels. I need to be able to keep
;; which files are important in my head,  and until that happens, I need the
;; little sidebar on the side. I need to be able to keep the open files in my
;; head, and until that happens, the tabbed topbar needs to be there


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

;; red highlight on trailing whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; on save, newline at the end of the file and remove trailing whitespace
(setq-default require-final-newline t)
(setq-default mode-require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; when paragraph filling (fitting within a certain amount of columns with M-q,
;; this makes it so that it goes within 80 lines. ChatGPT says the first
;; variable helps with the space at the end, but it doesn't
(setq-default display-fill-column-indicator-character ?~)
(setq-default display-fill-column-indicator-column 80)
(display-fill-column-indicator-mode t)


;; remember
(save-place-mode 1)
(setq backup-directory-alist `(("." . "~/.config/emacs/backups")))
(setq auto-save-list-file-prefix "~/.config/emacs/autosave")
(setq auto-save-file-name-transforms
      '((".*" "~/.config/emacs/autosave" t)))

;; remember session
;(desktop-load-default)
;(desktop-read)

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

;; tabs
(global-tab-line-mode t)

;; Customize tab-line faces
;; Customize tab-line faces with padding
(custom-set-faces
 '(tab-line
   ((t (:background "#fdf6e3" :foreground "#586e75" :box (:line-width -1 :color "#93a1a1") :height 0.9))))
 '(tab-line-tab
   ((t (:background "#eee8d5" :foreground "#268bd2" :box (:line-width 4 :color "#93a1a1")))))
 '(tab-line-tab-inactive
   ((t (:background "#fdf6e3" :foreground "#93a1a1" :box (:line-width 4 :color "#93a1a1")))))
 '(tab-line-tab-current
   ((t (:background "#eee8d5" :foreground "#b58900" :box (:line-width 4 :color "#93a1a1")))))
 '(tab-line-highlight
   ((t (:background "#eee8d5" :foreground "#d33682" :box (:line-width 4 :color "#93a1a1"))))))

;; file explorer
(setq treemacs-position 'right)
(global-set-key (kbd "<f8>") 'treemacs)
(setq treemacs-width 20) ; Set the width of the Treemacs window
(setq treemacs-follow-mode t) ; Automatically follow the current file
(setq treemacs-filewatch-mode t) ; Watch for changes in the project
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-position 'right))

;; duplicate lines
(defun duplicate-line ()
  "Duplicate the current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
	(line (let ((s (thing-at-point 'line t)))
		(if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(custom-set-faces
 '(treemacs-git-unmodified-face
   ((t (:height 0.4 :inherit default)))) ; Unmodified files
 '(treemacs-git-modified-face
   ((t (:height 0.4 :foreground "#268bd2" :inherit default)))) ; Modified files
 '(treemacs-git-ignored-face
   ((t (:height 0.4 :foreground "#93a1a1" :inherit default)))) ; Ignored files
 '(treemacs-git-untracked-face
   ((t (:height 0.4 :foreground "#cb4b16" :inherit default)))) ; Untracked files
 '(treemacs-git-added-face
   ((t (:height 0.4 :foreground "#859900" :inherit default)))) ; Newly added files
 '(treemacs-git-renamed-face
   ((t (:height 0.4 :foreground "#b58900" :inherit default)))) ; Renamed files
 '(treemacs-git-conflict-face
   ((t (:height 0.4 :foreground "#6c71c4" :inherit default)))) ; Files with conflicts
 '(treemacs-git-deleted-face
   ((t (:height 0.4 :foreground "#dc322f" :inherit default)))) ; Solarized Red
 '(treemacs-root-face
   ((t (:height 0.4 :inherit default)))) ; Root node
 '(treemacs-directory-face
   ((t (:height 0.4 :inherit default)))) ; Directory names
 '(treemacs-file-face
   ((t (:height 0.4 :inherit default)))) ; File names
 '(treemacs-root-active-face
   ((t (:height 0.4 :weight bold :inherit default)))) ; Active root node
 )

(setq treemacs-resize-icons 10) ; Set icon size in pixels (default is 22)

;; C-, to dupe line
(global-set-key (kbd "C-,") 'duplicate-line)

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
                :slant normal :weight normal :height 140 :width normal)))))


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
;(require 'lsp-mode)
;(use-package lsp-ui :commands lsp-ui-mode)

;; javascript
;; maybe worth inspecting in the future:
;;   https://wavesurfer.xyz/blog/emacs-javascript
; (require 'js2-mode)
; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;(add-hook 'js-mode-hook 'lsp-deferred)

;; python
;; https://slinkp.com/python-emacs-lsp-20231229.html
;(add-hook 'python-mode-hook 'lsp-deferred)
