(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; OS X options (not using OS X anymore but...)
(defun set-osx-keys()
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-key-is-meta nil)
  (setq mac-option-modifier 'none)
  (setq x-select-enable-clipboard t))

(if (eq system-type 'darwin) (set-osx-keys))

;; No startup screen
(setq inhibit-startup-screen t)

;; DIE TOOL BAR!!
(tool-bar-mode -1)

;; KILL ALL THE BELLS!!!!!11
(setq visible-bell t)

;; Line numbers
(global-linum-mode 1)

;; *~ files are useless, I always work over VCS
(setq make-backup-files nil)

;; Meta+e : "See" tabs
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(global-set-key "\M-e" 'whitespace-mode)

;; Ctrl+l : Goto Line
(global-set-key "\C-l" 'goto-line)

;; Ctrl+c f : Toggle flymake
(global-set-key "\C-cf" 'flymake-mode)

;; Remove trailing withespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Flyspell for LaTeX mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

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

;; Some elpa repos, just in case.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("tromeyelpa" . "http://tromey.com/elpa/"))

;; Enable odt exporting in org-mode
(require 'ox-odt)

;; Theme
(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)
(global-hl-line-mode t)

;; Poweline, add cuteness to emacs
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-powerline")
(require 'powerline)
(setq powerline-arrow-shape 'arrow)

(set-face-attribute 'mode-line nil
		    :background "#9ACD32"
		    :foreground "#030303"
		    :inverse-video nil
		    :box nil)
(set-face-attribute 'mode-line-inactive nil
		    :background "#1c1c1c"
		    :foreground "#f9f9f9"
		    :inverse-video nil
		    :box nil)

;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/site-lisp/popup-el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(require 'auto-complete-config)
(ac-config-default)

;; web-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/web-mode")
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; html5-el
(add-to-list 'load-path "~/.emacs.d/site-lisp/html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/site-lisp/html5-el/schemas.xml"))

(require 'whattf-dt)

;; scss-mode, life is easier with scss
(add-to-list 'load-path "~/.emacs.d/site-lisp/scss-mode")
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-sass-command "~/.gem/ruby/2.1.0/bin/sass")

;; Zen-coding/Emmet
(add-to-list 'load-path "~/.emacs.d/site-lisp/emmet-mode")
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; Turtle RDF mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/ttl-mode")
(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
(add-hook 'ttl-mode-hook 'turn-on-font-lock)
(setq auto-mode-alist
      (append
       (list
        '("\\.n3" . ttl-mode)
        '("\\.ttl" . ttl-mode))
       auto-mode-alist))

;; SPARQL mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/sparql-mode")
(autoload 'sparql-mode "sparql-mode.el" "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))
(add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode))

;; VIM-Modeline, lets respect the vim users special stuff
(add-to-list 'load-path "~/.emacs.d/site-lisp/vim-modeline")
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

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
