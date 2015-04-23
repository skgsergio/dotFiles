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

;; Highlight current line
(global-hl-line-mode t)

;; Don't indent with tabs ffs!
(setq-default indent-tabs-mode nil)

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

;; Flyspell for LaTeX and Org mode
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'flyspell-mode-hook 'flyspell-buffer)

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

;; Powerline
(add-to-list 'load-path "~/.emacs.d/site-lisp/powerline")
(require 'powerline)

;; moe-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/moe-theme-el/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/moe-theme-el/")
(require 'moe-theme)
(moe-theme-set-color 'green)
(powerline-moe-theme)
(moe-dark)

;; org-mode: ODT exporting
(require 'ox-odt)

;; org-mode: Latex exporting
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("linenos" "true")
        ("breaklines" "true")))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Markdown-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; html5-el
(add-to-list 'load-path "~/.emacs.d/site-lisp/html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/site-lisp/html5-el/schemas.xml"))

(require 'whattf-dt)

;; less-css-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/less-css-mode")
(require 'less-css-mode)
(setq less-css-lessc-command (expand-file-name "~/node_modules/.bin/lessc"))
(setq less-css-compile-at-save t)
(add-to-list 'ac-modes 'less-css-mode)
(add-hook 'less-css-mode-hook 'ac-css-mode-setup)

;; Zen-coding/Emmet
(add-to-list 'load-path "~/.emacs.d/site-lisp/emmet-mode")
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; PKGBUILD mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/pkgbuild-mode")
(require 'pkgbuild-mode)

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
