;; DesertEx Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate DesertEx colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/desertex-emacs/color-theme-desertex.el")
;;
;; And then (color-theme-desertex) to activate it.
;;
;; Several areas still require improvement such as recognition of code that ruby-mode doesn't
;; yet pick up (eg. parent classes), Rails/Merb keywords, or non Ruby code related areas
;; (eg. dired, HTML, etc). Please feel free to customize further and send in any improvements,
;; patches most welcome.
;;
;; MIT License Copyright (c) 2008 Marcus Crafter <crafterm@redartisan.com>
;; Credits due to the excellent TextMate DesertEx theme

(require 'color-theme)

;;;###autoload
(defun color-theme-desertex ()
  "Color theme by Ivan Porto Carrero, based off the Vim DesertEx theme, created 2008-04-18"
  (interactive)
  (color-theme-install
   '(color-theme-desertex
     ((background-color . "#2B2B2B")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "yellow")
      (foreground-color . "#EEE8AA")
      (mouse-color . "sienna1"))
     (default ((t (:background "#2B2B2B" :foreground "#EEE8AA"))))
     (blue ((t (:foreground "#FA8072"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t :slant italic))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "black" :foreground "white"))))
     (font-lock-builtin-face ((t (:foreground "white"))))
     (font-lock-comment-face ((t (:foreground "#7CCD7C"))))
     (font-lock-constant-face ((t (:foreground "#FA8072"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#87CEEB"))))
     (font-lock-keyword-face ((t (:foreground "#FFA54F"))))
     (font-lock-preprocessor-face ((t (:foreground "#EE799F"))))
     (font-lock-reference-face ((t (:foreground "#7EC0EE"))))
 
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 
     (font-lock-string-face ((t (:foreground "#CD5C5C"))))
     (font-lock-type-face ((t (:foreground "#86CEFA"))))
     (font-lock-variable-name-face ((t (:foreground "#EEDC82"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#303030" :foreground "black"))))
     (region ((t (:background "#444444"))))
     (mode-line ((t (:background "#C2BFA5" :foreground "black"))))
     (highlight ((t (:background "#222222"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (cursor ((t (:background "yellow", :foreground "black"))))
     (zmacs-region ((t (:background "snow" :foreground "blue")))))))

