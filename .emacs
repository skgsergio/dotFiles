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

;;; Native Comp

;; Compile all site-lisp on demand
(setq native-comp-deferred-compilation t)
(setq native-compile-prune-cache t)

;;; OS Config

;; macOS config
(when (string-equal "darwin" system-type)
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
(when (and (string-equal "gnu/linux" system-type)
           (string-match "[Mm]icrosoft" operating-system-release))
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "wslview")
  (message "Loaded WSL config."))

;; Linux config
(when (and (string-equal "gnu/linux" system-type)
           (not (string-match "[Mm]icrosoft" operating-system-release)))
  (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'default-frame-alist '(drag-internal-border . t))
  (add-to-list 'default-frame-alist '(internal-border-width . 2))
  (add-to-list 'default-frame-alist '(alpha-background . 98))
  (message "Loaded Linux config."))

;;; Base config

;; Set error as the minimum level that is shown immediately
(setq warning-minimum-level :error)

;; Keep custom variables out of this file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; No startup screen
(setq inhibit-startup-screen t)

;; DIE {MENU,TOOL,SCROLL} BAR!!
(menu-bar-mode -1)
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

(global-set-key (kbd "M-i") 'iwb)

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
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "C-c f") 'spell-switch-dictionary)

(add-hook 'flyspell-mode-hook 'flyspell-buffer)

;; ido + icomplete
(ido-mode 1)
(icomplete-mode 1)

;;; Configure package

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)

(setq use-package-always-ensure t)

;; vc-use-package (to be removed in emacs 30)
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Magit
(use-package magit)

;; Projectile
(use-package projectile)

;; Tremacs
(use-package treemacs
  :bind
  (:map global-map
        ("C-x t t" . treemacs)))

(use-package treemacs-projectile)

;; Emojify
(use-package emojify
  :config
  (setq emojify-emoji-styles '(unicode))
  :hook
  (after-init . global-emojify-mode))

;; Git-gutter
(use-package git-gutter
  :config
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (setq git-gutter:modified-sign "~")
  (setq git-gutter:separator-sign " ")
  (set-face-foreground 'git-gutter:modified "orange")
  :hook
  (after-init . global-git-gutter-mode))

;; ag: The Silver Searcher
(use-package ag
  :config
  (setq ag-highlight-search 't)
  (setq ag-reuse-buffers 't))

;; multiple-cursors
(use-package multiple-cursors
  :bind
  (:map global-map
        ("C-S-c C-S-c" . 'mc/edit-lines)
        ("C->" . 'mc/mark-next-like-this)
        ("C-<" . 'mc/mark-previous-like-this)
        ("C-c C-<" . 'mc/mark-all-like-this)))

;; org-mode
; As org is builtin we have to do this to force use-package fetching the package from the repo
(use-package org
  :init
  (assq-delete-all 'org package--builtins)
  (assq-delete-all 'org package--builtin-versions)
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-caption-above nil)
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("linenos" "true")
          ("breaklines" "true")))
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf %f"))
  :hook
  (org-mode . flyspell-mode)
  (org-mode . auto-fill-mode))

;; org-mode: RevealJS
(use-package ox-reveal)

;; htmlize for exporting code highlighted to html
(use-package htmlize)

;; web-mode
(use-package web-mode
  :mode
  ("\\.phtml$" . web-mode)
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

;; EditorConfig
(use-package editorconfig
  :hook
  (after-init . editorconfig-mode))

;; Flycheck
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

;; Yasnippet
(use-package yasnippet
  :hook
  (after-init . yas-global-mode))

;; Company
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :hook
  (after-init . global-company-mode))

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (setq read-process-output-max (* 1024 1024 3))
  (setq lsp-idle-delay 0.500)
  (setq lsp-enable-links t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-semantic-tokens-honor-refresh-requests t)
  :hook
  (c-mode . lsp-deferred)
  (sh-mode . lsp-deferred))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :bind
  (:map global-map
        ("C-x t e" . lsp-treemacs-errors-list)
        ("C-x t s" . lsp-treemacs-symbols)
        ("C-x t r" . lsp-treemacs-references)
        ("C-x t i" . lsp-treemacs-implementations)
        ("C-x t c" . lsp-treemacs-call-hierarchy)
        ("C-x t y" . lsp-treemacs-type-hierarchy)
        ("C-x t d" . lsp-treemacs-deps-list))
  :hook
  (lsp-mode . lsp-treemacs-sync-mode))

;; Python
(use-package python-mode
  :mode
  ("\\.py$" . python-mode)
  :hook
  (python-mode . lsp-deferred))

;; Go
(use-package go-mode
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (add-hook 'before-save-hook 'lsp-organize-imports t t))
  :hook
  (go-mode . lsp-go-install-save-hooks)
  (go-mode . lsp-deferred))

;; Rust
(use-package rust-mode
  :hook
  (rust-mode . lsp-deferred))

;; terraform-mode
(use-package terraform-mode
  :config
  (setq lsp-terraform-ls-enable-show-reference t)
  (setq lsp-terraform-ls-prefill-required-fields t)
  :hook
  (terraform-mode . terraform-format-on-save-mode)
  (terraform-mode . lsp-deferred))

;; jsonnet
(use-package jsonnet-mode
  :init
  (require 'lsp-jsonnet)
  :hook
  (jsonnet-mode . lsp-deferred))

;; LSP Grammarly
(use-package lsp-grammarly
  :init
  (require 'lsp-grammarly)
  :hook
  (text-mode . lsp-deferred))

;; Markdown-mode
(use-package markdown-mode
  :mode
  ("\\.markdown$" . markdown-mode)
  ("\\.md$" . markdown-mode)
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . auto-fill-mode)
  (markdown-mode . lsp-deferred))

;; YAML mode
(use-package yaml-mode
  :mode
  ("\\.ya?ml$" . yaml-mode)
  :hook
  (yaml-mode . lsp-deferred))

;; dockerfile-mode
(use-package dockerfile-mode
  :mode
  ("^Dockerfile" . dockerfile-mode)
  ("Dockerfile$" . dockerfile-mode)
  :hook
  (dockerfile-mode . lsp-deferred))

;; Typescript
(use-package typescript-mode
  :hook
  (typescript-mode . lsp-deferred))

;; Varnish
(use-package vcl-mode
  :mode
  ("\\.vtc" . vcl-mode))

;; Tinybird
(use-package tinybird-mode
  :vc
  (tinybird-mode :url "https://github.com/skgsergio/tinybird-mode" :rev :newest))

;;; AI

(use-package copilot
  :vc
  (copilot :url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :hook
  (prog-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)
        ("C-n" . 'copilot-next-completion)
        ("C-p" . 'copilot-previous-completion)))

;;; Style

;; Spaceline
(use-package nerd-icons)

(use-package doom-modeline
  :config
  (setq doom-modeline-time nil
        doom-modeline-env-version nil)
  :hook
  (after-init . doom-modeline-mode))

;; Doom
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  (load-theme 'doom-material-dark t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))

;; Customize fonts
(defvar my-font-family "Source Code Pro")
(defvar my-font-size 110)

(set-face-attribute 'default nil :family my-font-family :height my-font-size)
(set-face-attribute 'fixed-pitch-serif nil :family my-font-family)

;; Keybindings for increasing font size (for screen sharing and so)
(global-set-key (kbd "M-+") (lambda () (interactive) (set-face-attribute 'default nil :height (+ my-font-size 20))))
(global-set-key (kbd "M--") (lambda () (interactive) (set-face-attribute 'default nil :height my-font-size)))

;;; Run GC and set a lower GC threshold
(garbage-collect)

;; Keep GC threshold a bit higher to keep LSP happy
(setq gc-cons-threshold 100000000)

;;; .emacs ends here
