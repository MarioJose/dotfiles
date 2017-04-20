;;; Environment settings
;;; --------------------

; Disable startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

; Disable sound notification
(setq visible-bell t)

; Disable scrool bar
(scroll-bar-mode 0)

; Tool bar with icon only
(setq tool-bar-style 'image)

; Fonts
(set-face-attribute 'default nil :font "Source Code Pro-12")
(set-face-attribute 'mode-line nil :font "Source Code Pro-10")
(set-face-attribute 'mode-line-inactive nil :font "Source Code Pro-10")

; Frame (GUI window) position
; Width: 80 text wide, 2 (left and right) text to border and 1 to new line
; white character. Total: 93
(add-to-list 'default-frame-alist '(width . 93))
(add-to-list 'default-frame-alist '(top . 25))
(add-to-list 'default-frame-alist '(left . 0))
;(add-to-list 'default-frame-alist '(height . 60))

; Window (frame) position
(setq centered t)
(setq-default left-margin-width 1 right-margin-width 1) ; in column
(setq-default fringes-outside-margins 1)
;(set-face-attribute 'fringe nil :background "#212121" :foreground "#494949")
(setq-default left-fringe-width 50 right-fringe-width 50) ; in pixel
(fringe-mode 0)

; Set up defaults for the Latin-1 character set
(set-language-environment "Latin-1")

; Show line numbers and column position
;(global-linum-mode 1) ; line number in line
(eval-after-load "linum"
  '(progn
    (set-face-attribute 'linum nil :height 100)
    (setq linum-format 'dynamic)))
(column-number-mode 1)

; Copy/paste to/from external programs
(setq x-select-enable-clipboard t)

; Enable ctrl+c crtl+v
;(cua-mode t)

; Clear the trash
(setq delete-auto-save-files t)
(setq make-backup-files nil)

; Disable auto save and backup
(setq auto-save-default nil)
(setq backup-inhibited t)

; Scroll automatically
(setq scroll-step 2)
(setq scroll-conservatively 2)
(setq scroll-down-aggressively 0.01) ; 0 to 1
(setq scroll-up-aggressively 0.01) ; 0 to 1

;; Scroll at shell
; Scroll when insertion and yank commands
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
; Point jump to the end of the buffer
(setq comint-move-point-for-output t)


;;; Text editing
;;; ------------

; Spell program and dictionay
(setq ispell-program-name "aspell")
(setq ispell-list-command "--list")
(setq ispell-dictionary "british")

; Syntax hilighting
(global-font-lock-mode 1)

; Wrap line
(global-visual-line-mode 0)
(setq line-move-visual t)
(setq-default fill-column 80)
(setq fill-column 80)

; Cursor highlight
;(global-hl-line-mode 1)

; Space indent
(setq-default tab-width 2) ; how tabs looks likes
(setq-default tab-stop-list (number-sequence 2 120 2)) ; number of spaces for tabs
(setq-default indent-tabs-mode nil) ; use space to tabs indentation
;(setq tabify nil) ; change spaces to tabs (when appropriate)
;(setq untify t) ; change tabs to spaces

; Bracket match highlight
(show-paren-mode 1)

; Setting themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'zenburn t)
(load-theme 'github t)


;;; Customize hooks
;;; ---------------

; prog-mode
(add-hook 'prog-mode-hook 
  (lambda () 
  ; Line spacing
  (setq line-spacing 0.25)
  ; Spell check comments
  (flyspell-prog-mode)
  ; Auto fill comments
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode)
  ; Whitespace
  (setq whitespace-style '(newline newline-mark))
  (whitespace-mode)
  )
)

; text-mode
(add-hook 'text-mode-hook
  (lambda()
  ; Line spacing
  (setq line-spacing 0.25)
  ; Whitespace
  (setq whitespace-style '(face spaces tabs newline space-mark tab-mark newline-mark))
  (whitespace-mode)
  ; Auto-fill
  ;(auto-fill-mode)
  ; Spell check
  (flyspell-mode)
  ; Active fringes
  (fringe-mode 1)
  ; Load theme
  ;(load-theme 'github t)
  )
)

; TeX-mode
(add-hook 'TeX-mode-hook
  (lambda()
  ; Enable parse on load
  (setq TeX-parse-self t)
  ; Enable parse on save
  (setq TeX-auto-save t)
  (setq-default TeX-master nil)
  ; PDF mode (rather than DVI)
  (setq TeX-PDF-mode t)
  ; Disable auto-fill
  (auto-fill-mode 0)
  ; Enable Flyspell program mode for emacs lisp mode
  (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
  ; Automatically activate TeX-fold-mode
  ;(TeX-fold-mode 1)
  ; Disable language-specific hyphen insertion.
  (setq LaTeX-babel-hyphen nil);
  ; LaTeX Math 
  (LaTeX-math-mode)
  ; RefTeX
  (reftex-mode)
  (imenu-add-menubar-index)
  ; Interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  ; RefTeX default format
  (setq reftex-cite-format 'natbib)
  ; Key to autocomplete
  (local-set-key [C-tab] 'TeX-complete-symbol)
  ; Set font
  (set-face-attribute 'default nil :font "DejaVu Sans-13")
  (set-face-attribute 'mode-line nil :font "DejaVu Sans-11")
  (set-face-attribute 'mode-line-inactive nil :font "DejaVu Sans-11")
  )
)

; ess-mode
(add-hook 'ess-mode-hook
  (lambda()
  (setq ess-history-file "~/R/.Rhistory")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  ; Help buffers go into one frame
  (setq ess-help-own-frame 'one)
  ; Disable different indentation to #, ##, ###
  (setq ess-fancy-comments nil)

  ; Indentation
  (setq ess-indent-level 2)

  (setq ansi-color-for-comint-mode 'filter)

  ; Autocomplete
  (setq ess-tab-complete-in-script t)
  (setq ess-use-auto-complete 'script-only)
  ;(auto-complete-mode)
  
  ; Key map
  (local-set-key (kbd "C-<return>") 'ess-eval-line-and-step)
  (local-set-key (kbd "C-c C-S-r") 'ess-start-R)

  ; Open help in browser
  (setq inferior-ess-r-help-command "help(\"%s\", help_type=\"html\")\n")

  (custom-set-variables '(current-language-environment "UTF-8"))

  (run-hooks 'prog-mode-hook)
  )
)


;;; Load packages
;;; ------------

; Packages installer
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; R
(require 'ess-site)
; Show functions arguments
;(require 'ess-eldoc)
(require 'auto-complete)

;; Git
(require 'git)
(require 'magit)

;; Markdown
; Load Markdown package
;(add-to-list 'load-path "~/.emacs.d/pkgs/markdonw/")
; Enable Markdown mode
;(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
;(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; R Markdown
; Load R Markdown package (polymode)
(setq load-path
  (append '("~/.emacs.d/pkgs/polymode/"  "~/.emacs.d/pkgs/polymode/modes/")
            load-path))
(require 'poly-R)
(require 'poly-markdown)
; Enable R Markdown mode
(autoload 'poly-markdown-mode "poly-markdown-mode" "Major mode for editing R-Markdown files" t)
; R modes
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; Pandoc
; Load Pandoc package
(add-to-list 'load-path "~/.emacs.d/pkgs/pandoc/")
(autoload 'pandoc-mode "pandoc-mode" "Pandoc" t)


;;; Functions
;;; ---------

; Start ESS preferences
(defun ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
    (progn
      (delete-other-windows)
      (setq winCod (selected-window))
      (setq bufCod (buffer-name))
      (setq winR (split-window winCod (floor (* 0.83 (window-height)))))
      
      (other-window 1) ; go to new window
      (R) ; start R
      (set-window-buffer winR "*R*") ; set R buffer to R window
      (resize-fringes 0) ; remove fringe fom R window

      (other-window -1) ; back to main window
      (set-window-buffer winCod bufCod) ; set Code buffer to Code window
    )
  )
)

; Resize fringes
(defun resize-fringes (&optional arg &optional ts)
  (interactive "P")
  (if (equal arg 0)
    (progn ; if true
      (set-window-fringes (selected-window) 0 0 1)
    )
    (progn ; if false
      (if (equal ts nil) (setq ts 80)) ;default text width
      ; plus 3 for: 2 (left and right) text to border and 1 to new line white character
      (let ((fringe-size (/ (- (frame-pixel-width) (* (+ 3 ts) (frame-char-width))) 2)))
        (set-window-fringes (selected-window) fringe-size fringe-size 1)
      )
    )
  )
)


; Bells and whistles (distraction free)
(defvar-local naked-status 0)
(defun naked-emacs (&optional nakes-status)
  (interactive)
  (tool-bar-mode naked-status)
  (menu-bar-mode naked-status)
  (scroll-bar-mode naked-status)
  (if (equal naked-status 1) (setq naked-status 0) (setq naked-status 1))
)


; Full screen
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
      (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
  )
)
(global-set-key [f11] 'toggle-fullscreen)


