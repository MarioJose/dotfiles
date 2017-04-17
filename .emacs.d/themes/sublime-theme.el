
(deftheme sublime
  "sublime theme")

(custom-theme-set-faces
  'sublime
  '(default ((t (:background "#171717" :foreground "#CFBFAD"))))
  '(cursor ((t (:background "#FCE94F"))))
  '(border ((t (:background "#1A1A1A"))))
  '(mouse ((t (:background "black"))))
  '(fringe ((t (:background "#1a1a1a"))))
  '(mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
  '(region ((t (:foreground "#404040" :background "#CC9900"))))
  '(font-lock-builtin-face ((t (:foreground "#52e3f6"))))
  '(font-lock-comment-face ((t (:foreground "#ffffff"))))
  '(font-lock-function-name-face ((t (:foreground "#edd400"))))
  '(font-lock-keyword-face ((t (:foreground "#ff007f"))))
  '(font-lock-string-face ((t (:foreground "#ece47e"))))
  '(font-lock-type-face ((t (:foreground"#8ae234"))))
  '(font-lock-variable-name-face ((t (:foreground "#8ae234"))))
  '(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
  '(font-lock-warning-face ((t (:foreground "Red" :bold t))))
)

; Autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sublime)
