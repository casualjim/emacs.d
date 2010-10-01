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
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
;;(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
(load-file "~/.emacs.d/my-ext/cedet-1.0pre6/common/cedet.el")


;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

(setq el-get-sources
    '( pov-mode vimpulse)
)

(el-get 'sync)
(ido-mode)

(tool-bar-mode)

;; reload file keep at position
(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))
 
(global-set-key "\C-c\C-r" 'reload-file)

(fset 'yes-or-no-p 'y-or-n-p)
(require 'recentf)
(recentf-mode 1)
;; recent files
(global-set-key (kbd "M-o M-r") 'recentf-open-files)


(require 'color-theme)
(load-file "~/.emacs.d/my-ext/sunburst.el")
(color-theme-sunburst)

(add-to-list 'load-path "~/.emacs.d/my-ext")
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(add-to-list 'load-path "~/.emacs.d/my-ext/yasnippet-0.6.1c")
    (require 'yasnippet)
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/my-ext/yasnippet-0.6.1c/snippets")
(add-to-list 'load-path "~/.emacs.d/ensime/elisp")

(set-default-font "-outline-Consolas-normal-r-normal-normal-17-97-96-96-c-*-iso8859-1")

;;(require 'vimpulse)

(require 'setnu+)
(if (fboundp 'setnu-mode) (setnu-mode 1))

(require 'scala-mode-auto)
(add-hook 'scala-mode-hook '(lambda () (yas/minor-mode-on)))
(require 'sbt)
(require  'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
'(inhibit-read-only t)
(require 'sr-speedbar)
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



;;; init.el ends here

