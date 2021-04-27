;;; .emacs -- My Emacs config
;;
;; Author: Sergio Conde <skgsergio(at)gmail(dot)com>
;; URL: https://github.com/skgsergio/dotFiles
;;
;;; Commentary:
;;
;; This is my Emacs config that I've been using and creating over the years.
;;
;;; Code:
;;

;;; Set a high GC threshold to have a quicker boot time
(setq gc-cons-threshold 1000000000)

;;; OS Config

;; macOS config
(when (string-equal system-type "darwin")
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-key-is-meta nil)
  (setq mac-option-modifier 'none)
  (setq x-select-enable-clipboard t)
  (setenv "PATH" (concat  (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin"))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (when (string-equal window-system "ns")
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (message "Loaded macOS config."))

;; WSL config
(when (and (string-equal system-type "gnu/linux")
           (string-match "microsoft"
                         (with-temp-buffer (shell-command "uname -r" t)
                                           (goto-char (point-max))
                                           (delete-char -1)
                                           (buffer-string))))
  (defun browse-url-windows-chrome-from-wsl (url &optional new-window)
    (shell-command (format "explorer.exe \"%s\"; true" url)))
  (setq browse-url-browser-function 'browse-url-windows-chrome-from-wsl)
  (message "Loaded WSL config."))

;;; Base config

;; Keep custom variables out of this file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; No startup screen
(setq inhibit-startup-screen t)

;; DIE {TOOL,SCROLL} BAR!!
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; KILL ALL THE BELLS!!!!!11
(setq ring-bell-function 'ignore)

;; Line numbers
(global-display-line-numbers-mode 1)

;; Highlight current line
(global-hl-line-mode t)

;; Don't indent with tabs ffs!
(setq-default indent-tabs-mode nil)

;; Display tabs as 4 characters
(setq-default tab-width 4)

;; Don't create backup files (*~)
(setq make-backup-files nil)

;; Always follow symlinks
(setq find-file-visit-truename t)

;; Ctrl+l : Goto Line
(global-set-key "\C-l" 'goto-line)

;; Remove trailing withespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Cut lines at 80th column in text modes
(setq-default fill-column 80)

(add-hook 'text-mode-hook 'auto-fill-mode)

;; Indent whole buffer
(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Indent buffer: Done.")
  )

(global-set-key "\M-i" 'iwb)

;; Mark ugly stuff
(require 'whitespace)

(setq whitespace-style '(face empty trailing))
(global-whitespace-mode t)

;; Flyspell
(require 'ispell)

(setq ispell-dictionary "american")
(defun spell-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "american") "castellano" "american")))
    (ispell-change-dictionary change)
    (flyspell-buffer)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key "\C-cf" 'spell-switch-dictionary)

(add-hook 'flyspell-mode-hook 'flyspell-buffer)

;;; Configure package

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)
(require 'use-package-ensure)

(setq use-package-always-ensure t)

;; Projectile
(use-package projectile)

;; Tremacs
(use-package treemacs
  :bind (:map global-map
              ("C-x t t" . treemacs))
  )

(use-package treemacs-projectile)

;; Emojify
(use-package emojify
  :init (progn
          (global-emojify-mode)
          (setq emojify-emoji-styles '(unicode)))
  )

;; Git-gutter
(use-package git-gutter
  :init (progn
          (global-git-gutter-mode)
          (setq git-gutter:added-sign "+")
          (setq git-gutter:deleted-sign "-")
          (setq git-gutter:modified-sign "~")
          (setq git-gutter:separator-sign " "))
  )

;; ag: The Silver Searcher
(use-package ag
  :init (progn
          (setq ag-highlight-search 't)
          (setq ag-reuse-buffers 't))
  )

;; org-mode
(use-package org
  :init (progn
          (require 'ox-latex)
          (add-hook 'org-mode-hook 'flyspell-mode)
          (add-hook 'org-mode-hook 'auto-fill-mode)
          (add-to-list 'org-latex-packages-alist '("" "minted"))
          (setq org-latex-caption-above nil)
          (setq org-latex-listings 'minted)
          (setq org-latex-minted-options
                '(("linenos" "true")
                  ("breaklines" "true")))
          (setq org-latex-pdf-process
                '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf %f")))
  )

;; org-mode: RevealJS
(use-package ox-reveal)

;; htmlize for exporting code highlighted to html
(use-package htmlize)

;; Markdown-mode
(use-package markdown-mode
  :init (progn
          (add-hook 'markdown-mode-hook 'flyspell-mode)
          (add-hook 'markdown-mode-hook 'auto-fill-mode))
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  )

;; YAML mode
(use-package yaml-mode
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode))
  )

;; terraform-mode
(use-package terraform-mode
  :init (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  )

;; dockerfile-mode
(use-package dockerfile-mode
  :mode ("^Dockerfile" . dockerfile-mode)
  )

;; web-mode
(use-package web-mode
  :mode (("\\.phtml$" . web-mode)
         ("\\.php$" . web-mode)
         ("\\.[agj]sp$" . web-mode)
         ("\\.as[cp]x$" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.mustache$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.css$" . web-mode)
         ("\\.html$" . web-mode)
         ("\\.js$" . web-mode)
         ("\\.json$" . web-mode)
         ("\\.html.j2$" . web-mode))
  :init (progn
          (setq web-mode-markup-indent-offset 2)
          (setq web-mode-css-indent-offset 2)
          (setq web-mode-code-indent-offset 2))
  )

;; EditorConfig
(use-package editorconfig
  :init (editorconfig-mode 1)
  )

;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  )

;; Company
(use-package company
  :init (progn
          (global-company-mode)
          (setq company-idle-delay 0)
          (setq company-minimum-prefix-length 1))
  )

;; LSP
(use-package lsp-mode
  :init (progn
          (setq lsp-enable-snippet nil)
          (setq read-process-output-max (* (* 1024 1024) 2)))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package lsp-treemacs
  :init (lsp-treemacs-sync-mode 1)
  :bind (:map global-map
              ("C-x t e" . lsp-treemacs-errors-list)
              ("C-x t s" . lsp-treemacs-symbols)
              ("C-x t r" . lsp-treemacs-references)
              ("C-x t i" . lsp-treemacs-implementations)
              ("C-x t c" . lsp-treemacs-call-hierarchy)
              ("C-x t y" . lsp-treemacs-type-hierarchy)
              ("C-x t d" . lsp-treemacs-deps-list))
  )

;; Python + LSP Python MS Server
(use-package python-mode
  :init (add-hook 'python-mode-hook 'lsp)
  )

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  )

;; Go + LSP
(use-package go-mode
  :init (progn
          (defun lsp-go-install-save-hooks ()
            (add-hook 'before-save-hook 'lsp-format-buffer t t)
            (add-hook 'before-save-hook 'lsp-organize-imports t t))
          (add-hook 'go-mode-hook 'lsp)
          (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))
  )


;;;; Style

;; Spaceline
(use-package spaceline-all-the-icons
  :init (progn
          (spaceline-all-the-icons-theme)
          (spaceline-toggle-all-the-icons-buffer-position-on)
          (spaceline-toggle-all-the-icons-projectile-on)
          (spaceline-toggle-all-the-icons-buffer-path-on)
          (spaceline-toggle-all-the-icons-hud-off)
          (spaceline-toggle-all-the-icons-time-off)
          (setq spaceline-all-the-icons-separator-type 'none)
          (setq spaceline-all-the-icons-icon-set-modified 'circle)
          (setq spaceline-all-the-icons-hide-long-buffer-path t)
          (when (string-equal system-type "darwin")
            (setq powerline-image-apple-rgb t)))
  )

;; Doom
(use-package doom-themes
  :init (progn
          (setq doom-themes-enable-bold t
                doom-themes-enable-italic t)
          (load-theme 'doom-tomorrow-night t)
          (doom-themes-treemacs-config)
          (doom-themes-org-config)
          (doom-themes-visual-bell-config))
  )

;; Customize colors
(set-face-foreground 'git-gutter:modified "orange")
(set-face-attribute 'spaceline-highlight-face nil :foreground "black" :background "dark orange")

;; Customize fonts
(setq my-font "Source Code Pro")
(set-face-attribute 'default nil :family my-font :height 110)
(set-face-attribute 'fixed-pitch-serif nil :family my-font)

;;; Run GC and set a lower GC threshold
(garbage-collect)

;; Keep GC thresholda bit higher to keep LSP happy
(setq gc-cons-threshold 100000000)

;;; .emacs ends here
