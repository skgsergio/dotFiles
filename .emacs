(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; OS X options (not using OS X anymore but...)
(defun set-osx-keys()
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-key-is-meta nil)
  (setq mac-option-modifier 'none)
  (setq x-select-enable-clipboard t))

(if (eq system-type 'darwin) (set-osx-keys))

;; Remove trailing withespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Meta+e "See" tabs
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(global-set-key "\M-e" 'whitespace-mode)

;; Ctrl+l : Goto Line
(global-set-key "\C-l" 'goto-line)

;; Ctrl+c f : Toggle flymake
(global-set-key "\C-cf" 'flymake-mode)

;; Line numbers
(global-linum-mode 1)

;; KILL ALL THE BELLS!!!!!11
(setq visible-bell t)

;; Indent Fucking Whole Buffer
(defun iwb ()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Indent buffer: Done.")
  )

(global-set-key "\M-i" 'iwb)

;; Acute to html entities
(defun acute2html ()
  "Convert acutes to HTML entities"
  (interactive)
  (save-excursion
    (dolist (c '(("á" . "&aacute;")
                 ("é" . "&eacute;")
                 ("í" . "&iacute;")
                 ("ó" . "&oacute;")
                 ("ú" . "&uacute;")
                 ("ñ" . "&ntilde;")
                 ("Á" . "&Aacute;")
                 ("É" . "&Eacute;")
                 ("Í" . "&Iacute;")
                 ("Ó" . "&Oacute;")
                 ("Ú" . "&Uacute;")
                 ("Ñ" . "&Ntilde;")))
      (goto-char (point-min))
      (replace-string (car c) (cdr c)))
    (message "Acute2html: Done."))
  )

;; Flyspell for LaTeX mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Some elpa repos, just in case.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("tromeyelpa" . "http://tromey.com/elpa/"))

;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/site-lisp/popup-el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(require 'auto-complete-config)
(ac-config-default)

;; nxHtml
(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")

;; html5-el
(add-to-list 'load-path "~/.emacs.d/site-lisp/html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/site-lisp/html5-el/schemas.xml"))

(require 'whattf-dt)

;; Mumamo fixes
(setq mumamo-per-buffer-local-vars
      (delq 'buffer-file-name mumamo-per-buffer-local-vars))
(setq mumamo-background-colors nil)

;; scss-mode, life is easier with scss
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/scss-mode"))
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-sass-command "~/.gem/ruby/2.1.0/bin/sass")

;; Poweline, add cuteness to emacs
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-powerline")
(require 'powerline)
(setq powerline-arrow-shape 'arrow)

;; VIM-Modeline, lets respect the vim users special stuff
(add-to-list 'load-path "~/.emacs.d/site-lisp/vim-modeline")
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

;; DIE TOOL BAR!!
(tool-bar-mode -1)

;; Enable Flymake, Pyflakes, Chktex
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/bin/pylint-auto" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)
               )

  (defun flymake-get-tex-args (file-name)
    (list "chktex" (list "-g0" "-r" "-l" (expand-file-name "~/.chktexrc") "-I" "-q" "-v0" file-name)))

  (push
   '("^\\(\.+\.tex\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
     1 2 3 4) flymake-err-line-patterns)

  )

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Theme
(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)
(global-hl-line-mode t)

;; Some styles
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; Some vars.
(custom-set-variables
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(cursor-color "#657b83")
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(foreground-color "#657b83")
 '(inhibit-startup-screen t))
