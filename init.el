;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(require 'anything-match-plugin)
(require 'anything-config)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
(if (file-exists-p user-specific-config) (load user-specific-config))


;(el-get 'sync)
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
(setq ido-use-filename-at-point t) ;; prefer file names near point


(tool-bar-mode -1)


(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d/my-ext")
(add-to-list 'load-path "~/.emacs.d/scala-mode2")
; (add-to-list 'load-path "~/.emacs.d/my-ext/yasnippet-0.6.1c")
;     (require 'yasnippet)
;     (yas/initialize)
;     (yas/load-directory "~/.emacs.d/my-ext/yasnippet-0.6.1c/snippets")
(add-to-list 'load-path "~/.emacs.d/ensime/elisp")

(require 'color-theme)
; (load-file "~/.emacs.d/my-ext/tomorrow-night-theme.el")
; (load-theme 'monokai)
(load-file "~/.emacs.d/my-ext/desertex.el")
(color-theme-desertex)

(set-face-attribute 'default nil :family "Menlo" :height 120 :weight 'normal)
; (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")

(require 'recentf)
    (recentf-mode 1)


(global-linum-mode 1)

(require 'scala-mode)
;;(require 'scala-mode-auto)
;;(add-hook 'scala-mode-hook '(lambda () (yas/minor-mode-on)))
(require 'sbt)
(require  'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq ensime-sem-high-faces '())
;(inhibit-read-only t)
(require 'protobuf-mode)

(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
  (lambda () (c-add-style "my-style" my-protobuf-style t)))

(require 'highline)
(highline-mode 1)

;;
;; window movement etc {{{
(global-set-key (kbd "M-J") 'windmove-down)
(global-set-key (kbd "M-H") 'windmove-left)
(global-set-key (kbd "M-K") 'windmove-up)
(global-set-key (kbd "M-L") 'windmove-right)
(global-set-key (kbd "M-Q") 'delete-window)
(global-set-key (kbd "M-R") 'save-buffer)
(global-set-key (kbd "<S-return>") (lambda () (interactive) (move-end-of-line nil) (newline)))
(global-set-key (kbd "M-p") (lambda () (interactive) (save-buffer) (blender-execute-file (buffer-file-name))))


;; }}}

; (setq indicate-empty-lines t)
; (setq inhibit-startup-screen t)
; (linum-format "  %d  ")
; (puppet-indent-level tab-width)
; (recentf-max-saved-items 75)
; (require-final-newline t)
; (ruby-indent-level tab-width)
; (show-paren-delay 0)
; (tab-width 2))


;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-inhibited t t)
 '(cursor-type (quote bar) t)
 '(custom-safe-themes (quote ("e023ca8cf9513e55396e9cff94b2c1daa22dd978d61c9d8f9f566c8b4faec979" "cec5a574cd1c687c34820d184c904f824cc45649fa25370f5fe7fc2fd1bec726" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" default)))
 '(echo-keystrokes 0.01)
 '(fill-column 78)
 '(frame-title-format (quote ("%f - " user-real-login-name "@" system-name)) t)
 '(ido-auto-merge-work-directories-length nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(linum-format "  %d  ")
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(puppet-indent-level tab-width)
 '(recentf-max-saved-items 75)
 '(require-final-newline t)
 '(ruby-indent-level tab-width)
 '(show-paren-delay 0)
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default indent-tabs-mode nil)
; (global-set-key (kbd "C-x f") 'find-file-in-project)

;    (font-lock-builtin-face ((t (:foreground "white"))))
     ; (font-lock-comment-face ((t (:foreground "#8b8378"))))
     ; (font-lock-constant-face ((t (:foreground "#7CCD7C")))) ;7ec0ee
     ; (font-lock-doc-string-face ((t (:foreground "#7ccd7c"))))
     ; (font-lock-doc-face ((t (:foreground "#8b8378"))))
     ; (font-lock-function-name-face ((t (:foreground "#7ec0ee"))))
     ; (font-lock-keyword-face ((t (:foreground "#ee799f"))))
     ; (font-lock-keyword ((t (:foreground "#eedc82"))))
     ; (font-lock-preprocessor-face ((t (:foreground "#ee799f"))))
     ; (font-lock-reference-face ((t (:foreground "#eedc82"))))
 
     ; (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     ; (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 
     ; (font-lock-ido-first-match ((t (:foreground ,"#eedc82"))))
     ; (font-lock-ido-only-match ((t (:foreground ,"#ee799f"))))
     ; (font-lock-ido-subdir ((t (:foreground ,"#7ccd7c"))))
 

     ; (font-lock-string-face ((t (:foreground "#eedc82"))))
     ; (font-lock-type-face ((t (:foreground "#76eec6"))))
     ; (font-lock-variable-name-face ((t (:foreground "#fa8072"))))

(setq ensime-sem-high-faces
  '(
   (var . (:slant italic :foreground "#fa8072"))
   (val . (:foreground "#cdbe70"))
   (varField . (:foreground "#fa8072" :bold t :slant italic))
   (valField . (:foreground "#cdbe70" :slant italic))
   (functionCall . (:foreground "#7ec0ee" :slant italic))
   (param . (:foreground "#ee7942"))
   (class . (:foreground "#76eec6"))
   (trait . (:foreground "#7CCD7C"))
   (object . (:italic t :foreground "#76eec6"))
   (package . font-lock-preprocessor-face)
   ))

(add-hook 'scala-mode-hook '(lambda ()

  ;; Bind the 'newline-and-indent' command to RET (aka 'enter'). This
  ;; is normally also available as C-j. The 'newline-and-indent'
  ;; command has the following functionality: 1) it removes trailing
  ;; whitespace from the current line, 2) it create a new line, and 3)
  ;; indents it.  An alternative is the
  ;; 'reindent-then-newline-and-indent' command.
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; Bind the 'join-line' command to C-M-j. This command is normally
  ;; bound to M-^ which is hard to access, especially on some European
  ;; keyboards. The 'join-line' command has the effect or joining the
  ;; current line with the previous while fixing whitespace at the
  ;; joint.
  (local-set-key (kbd "C-M-j") 'join-line)

  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a 
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  (require 'whitespace)

  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  ;;(whitespace-mode)

  ;;(setq whitespace-style (tabs trailing))

  ;; and other bindings here
))

(add-to-list 'load-path "~/.emacs.d/scalatra-mode/")

(add-hook 'scala-mode-hook '(lambda ()
  (require 'scalatra-mode)
  (scalatra-mode)
))
