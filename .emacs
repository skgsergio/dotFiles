;;; .emacs -- My Emacs config
;;
;; Author: Sergio Conde <skgsergio(at)gmail(dot)com>
;; URL: https://github.com/skgsergio/dotFiles
;;
;;; Commentary:
;;
;; This is my Emacs config that I've been using and creating over the years.
;; I'm not using the package manager because I've been using git submodules
;; for a while when it made part of Emacs, also I like it more that way.
;;
;;; Code:
;;

;; Add all subdirectories into the load-path
(let ((base "~/.emacs.d/site-lisp"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; macOS config
(when (string-equal system-type "darwin")
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-key-is-meta nil)
  (setq mac-option-modifier 'none)
  (setq x-select-enable-clipboard t)
  (setenv "PATH" (concat "/usr/local/bin:/Library/TeX/texbin:" (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (message "Loaded macOS config."))

;; Style selector
(defconst custom-style "doom") ; "doom", "moe", 'nil

;; Check if I'm in CARTO env
(defconst carto-env (if (file-exists-p "~/.emacs.d/carto") t nil))

;; No startup screen
(setq inhibit-startup-screen t)

;; DIE {TOOL,SCROLL} BAR!!
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; KILL ALL THE BELLS!!!!!11
(setq ring-bell-function 'ignore)

;; Line numbers
(global-linum-mode 1)

;; Highlight current line
(global-hl-line-mode t)

;; Don't indent with tabs ffs!
(setq-default indent-tabs-mode nil)

;; Don't create backup files (*~)
(setq make-backup-files nil)

;; Mark ugly stuff
(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode t)

;; Cut lines at 80th column in text modes
(setq-default fill-column 80)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Ctrl+l : Goto Line
(global-set-key "\C-l" 'goto-line)

;; Remove trailing withespaces on save
(defun enable-delete-trailing-save-hook()
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (message "Enabled delete-trailing-whitespace before-save-hook."))

(unless carto-env
  (enable-delete-trailing-save-hook))

;; Flyspell for LaTeX and Org mode
(require 'ispell)

(setq ispell-dictionary "english")
(defun spell-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "english") "spanish" "english")))
    (ispell-change-dictionary change)
    (flyspell-buffer)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key "\C-cf" 'spell-switch-dictionary)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'flyspell-mode-hook 'flyspell-buffer)

;; Indent Fucking Whole Buffer
(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Indent buffer: Done.")
  )

(global-set-key "\M-i" 'iwb)

;; Some elpa repos, just in case.
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Projectile
(require 'projectile)

;; NeoTree
(require 'neotree)

(setq neo-force-change-root t)
(setq neo-autorefresh t)

(defun neotree-toggle-with-projectile ()
  "Toggle neotree using projectile."
  (interactive)
  (let ((nttp-file-name (buffer-file-name)))
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (if (and (fboundp 'projectile-project-p)
               (projectile-project-p)
               (fboundp 'projectile-project-root))
          (neo-global--open-dir (projectile-project-root))
        (neo-global--open-dir (file-name-directory nttp-file-name)))
      (neotree-find nttp-file-name))))

(global-set-key [f8] 'neotree-toggle-with-projectile)

;; Emojify
(require 'emojify)

(setq emojify-emoji-styles '(unicode))
(add-hook 'after-init-hook 'global-emojify-mode)

;; Git-gutter
(require 'git-gutter-fringe)

(global-git-gutter-mode)
(setq git-gutter-fr:side 'left-fringe)

;; Auto-Complete
(require 'auto-complete-config)

(ac-config-default)

;; EditorConfig
(require 'editorconfig)

(editorconfig-mode 1)

;; ag: The Silver Searcher
(require 'ag)

(setq ag-highlight-search 't)
(setq ag-reuse-buffers 't)

;; org-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")

;; org-mode: Latex exporting
(require 'ox-latex)

(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-caption-above nil)

(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("linenos" "true")
        ("breaklines" "true")))

(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf %f"))

;; org-mode: RevealJS
(require 'ox-reveal)

;; org-mode: Markdown
(require 'ox-md)

;; org-mode: reStructuredText
(require 'ox-rst)

(when carto-env
  (setq org-rst-headline-underline-characters '(?= ?- ?~ ?. ?^ ?: ?' ?\ ?_)))

;; rust-mode
(require 'rust-mode)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Go
(require 'go-mode)

(add-hook 'before-save-hook 'gofmt-before-save)

;; Markdown-mode
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; YAML mode
(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; terraform-mode
(require 'terraform-mode)

(add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)

;; web-mode
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

;; Zen-coding/Emmet
(require 'emmet-mode)

(add-hook 'web-mode-hook 'emmet-mode)

;; Flycheck
(require 'flycheck)

(global-flycheck-mode)

;; Style
(when (string-equal custom-style "doom")
  ;; Spaceline
  (require 'spaceline-all-the-icons)

  (setq spaceline-all-the-icons-separator-type 'none)
  (spaceline-all-the-icons-theme)

  (spaceline-all-the-icons--setup-neotree)

  (spaceline-toggle-all-the-icons-projectile-off)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-hud-off)
  (spaceline-toggle-all-the-icons-time-off)
  (spaceline-toggle-all-the-icons-buffer-position-on)

  (when (string-equal system-type "darwin")
    (setq powerline-image-apple-rgb t))

  ;; Doom
  (require 'doom-themes)

  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-tomorrow-night t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(when (string-equal custom-style "moe")
  ;; Powerline
  (require 'powerline)

  (setq powerline-default-separator nil)

  (when (string-equal system-type "darwin")
    (setq powerline-image-apple-rgb t))

  ;; moe-theme
  (require 'moe-theme)

  (moe-theme-set-color 'green)
  (powerline-moe-theme)
  (moe-dark))

;;; .emacs ends here
