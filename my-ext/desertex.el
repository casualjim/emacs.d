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
      (foreground-color . "#bebebe")
      (mouse-color . "sienna1"))
     (default ((t (:background "#2B2B2B" :foreground "#bebebe"))))
     (blue ((t (:foreground "#00bfff"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t :slant italic))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "black" :foreground "white"))))
     (font-lock-builtin-face ((t (:foreground "white"))))
     (font-lock-comment-face ((t (:foreground "#8b8378"))))
     (font-lock-constant-face ((t (:foreground "#7CCD7C")))) ;7ec0ee
     (font-lock-doc-string-face ((t (:foreground "#7ccd7c"))))
     (font-lock-doc-face ((t (:foreground "#8b8378"))))
     (font-lock-function-name-face ((t (:foreground "#7ec0ee"))))
     (font-lock-keyword-face ((t (:foreground "#ee799f"))))
     (font-lock-keyword ((t (:foreground "#eedc82"))))
     (font-lock-preprocessor-face ((t (:foreground "#ee799f"))))
     (font-lock-reference-face ((t (:foreground "#eedc82"))))
 
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 
     (font-lock-ido-first-match-face ((t (:foreground "#eedc82"))))
     (font-lock-ido-only-match-face ((t (:foreground "#ee799f"))))
     (font-lock-ido-subdir-face ((t (:foreground "#7ccd7c"))))
 

     (font-lock-string-face ((t (:foreground "#eedc82"))))
     (font-lock-type-face ((t (:foreground "#76eec6"))))
     (font-lock-variable-name-face ((t (:foreground "#fa8072"))))
     (font-lock-warning-face ((t (:bold t :foreground "white" :background: "#FF0000"))))
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

(defalias 'desertex #'color-theme-zenburn)

(provide 'desertex)