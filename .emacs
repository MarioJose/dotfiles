; Running in Emacs 25

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

; Frame (GUI window) size and position
; Width: 80 text wide, 2 (left and right) text for border, 1 for new line white
; character and 10 for fringies. Total: 93
; Size
(add-to-list 'initial-frame-alist '(width . 93))
(add-to-list 'initial-frame-alist '(fullheight))
(add-to-list 'default-frame-alist '(width . 93))
(add-to-list 'default-frame-alist '(fullheight))
; Position
;(add-to-list 'initial-frame-alist '(left . 0))
;(add-to-list 'initial-frame-alist '(top . 25))

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

; Keep window size when autocomplete window is opened and destroied
(setq window-combination-resize t)


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
  (setq whitespace-style '(tabs tab-mark newline newline-mark))
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
  )
)

; TeX-mode
(add-hook 'TeX-mode-hook
  (lambda()
  ;; AUCTeX
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

; Markdown mode
(add-hook 'markdown-mode-hook
  (lambda()
  ; RefTeX
  (reftex-mode)
  ; RefTeX citation format
  (eval-after-load 'reftex-vars
    '(progn 
     (setq reftex-cite-format '((?\C-m . "@%l")
                                (?p . "[@%l]")
                                (?t . "@%l")
                                (?y . "[-@%l]")))
     (setq reftex-cite-key-separator "; @")
     ))
  )
)

; python-mode
(add-hook 'python-mode-hook
  (lambda()
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
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
  (setq ac-use-quick-help nil) ; Not use quick help
  (setq ac-auto-start nil) ; Disable automatically completation
  (define-key ac-mode-map "\M-/" 'auto-complete) ; Key to autocomplete
  (auto-complete-mode)
  
  ; Key map
  (local-set-key (kbd "C-<return>") 'ess-eval-line-and-step)
  (local-set-key (kbd "C-c C-S-r") 'ess-start-R)

  ; Open help in browser
  (setq inferior-ess-r-help-command "help(\"%s\", help_type=\"html\")\n")

  (custom-set-variables '(current-language-environment "UTF-8"))

  (run-hooks 'prog-mode-hook)
  )
)

; markup Rd-mode
(add-hook 'Rd-mode-hook
  (lambda()
  (run-hooks 'text-mode-hook)
  ; Auto fill
  (auto-fill-mode 0)
  )
)


;;; Load packages
;;; ------------

;; Packages installer
(require 'package)
; Package repository
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
; Python integration
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
; Initialize packages
(package-initialize)

;; R
(require 'ess-site)
(require 'auto-complete)

;; Phyton
(elpy-enable)

;; Git
(require 'git)
(require 'magit)

;; Markdown and R Markdown
; Load R Markdown package (polymode)
(require 'poly-R)
(require 'poly-markdown)
; Auto mode
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

; Auto mode to C for Arduino source
(add-to-list 'auto-mode-alist '("\\.ino" . c-mode))

;; Pandoc
; Load Pandoc package
(autoload 'pandoc-mode "pandoc-mode" "Pandoc" t)


;;; Functions
;;; ---------

; Start ESS preferences
(defun ess-start-R (&optional arg)
  (interactive "P")
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
    (progn
      (delete-other-windows)
      (setq winCod (selected-window))
      (setq bufCod (buffer-name))
      (if (equal arg nil)
        (progn
          (setq winR (split-window-below (floor (* 0.83 (window-height)))))
        )
        (progn
          (setq winR (split-window-right))
        )
      )
      (other-window 1) ; go to new window
      (R) ; start R
      (set-window-buffer winR "*R*") ; set R buffer to R window
      (resize-fringes 0) ; remove fringe from R window
      (set-window-dedicated-p winR 1)

      (other-window -1) ; back to main window
      (set-window-buffer winCod bufCod) ; set Code buffer to Code window
    )
  )
)

; Split window with code and R
(defun ess-split-window-R (&optional arg)
  (interactive "P")
  (setq winCod (selected-window))
  (setq bufCod (buffer-name))

  (if (equal arg nil)
    (progn
      (setq winR (split-window-below (floor (* 0.83 (window-height)))))
     )
    (progn
      (setq winR (split-window-right))
    )
  )
  (other-window 1)
  (switch-to-buffer "*R*")
  (resize-fringes 0)
  (set-window-dedicated-p winCod 1)

  (other-window -1)
  (switch-to-buffer bufCod)
)

; Set window with dedicated buffer
(defun toggle-window-dedicated ()
  "Define or undefine selected window as dedicated"
  (interactive)
  (setq window (selected-window))
  (if (equal (window-dedicated-p window) nil)
    (progn 
      (set-window-dedicated-p window 1)
      (message "Now, window is dedicated")
    )
    (progn
      (set-window-dedicated-p window nil)
      (message "Now, window is not dedicated")
    )
  )
)
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

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
(global-set-key (kbd "C-c r f") 'resize-fringes)


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

;;; Custom variables (track installed packages)
(setq custom-file "~/.emacs.d/custom_variables.el")
(load custom-file)


