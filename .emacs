(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; "See" tabs
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(global-set-key "\M-w" 'whitespace-mode)

;; KILL ALL THE BELLS!!!!!11
(setq visible-bell t)

;; Corrector horrografico en LaTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Indent Fucking Whole Buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Ctrl+L : Goto Line
(global-set-key "\C-l" 'goto-line)

;; Ctrl+U : Undo
(global-set-key "\C-u" 'undo)

;;;; PLUGINS ;;;;

;; Elpa repos
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("tromeyelpa" . "http://tromey.com/elpa/"))

;; Twitter follower twitter follower...
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-timer-interval 300)
(setq twittering-icon-mode t)
(setq twittering-url-show-status nil)
(setq twittering-initial-timeline-spec-string
      '(":direct_messages"
        ":replies"
	":home")
)
(add-hook 'twittering-new-tweets-hook (
  lambda () (
	     let ((n twittering-new-tweets-count))
	      (start-process "twittering-notify" nil "notify-send"
			     "-i" "/usr/share/emacs/23.3/etc/images/icons/hicolor/32x32/apps/emacs.xpm"
			     "Twitter"
			     (format "Hay %d nuevo%s tweet%s" n (if (> n 1) "s" "") (if (> n 1) "s" ""))
	      )
  )
))

;; Doxymacs
(add-hook 'c-mode-common-hook'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; Auto-Complete
(require 'auto-complete-config)
(ac-config-default)

;; nxHtml
(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")

;; Theme
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)
(global-hl-line-mode t)

;; Poweline
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-powerline")
(require 'powerline)

(setq powerline-arrow-shape 'arrow)

;; DIE TOOL BAR!!
(tool-bar-mode -1)

;; Flymake + Pyflakes

(when (load "flymake" t)
 (defun flymake-pyflakes-init ()
 (let* ((temp-file (flymake-init-create-temp-buffer-copy
 'flymake-create-temp-inplace))
 (local-file (file-relative-name
 temp-file
 (file-name-directory buffer-file-name))))
 (list "pyflakes" (list local-file))))
 
 (add-to-list 'flymake-allowed-file-name-masks
 '("\\.py\\'" flymake-pyflakes-init)))
 
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Some styles
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
(custom-set-variables
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(cursor-color "#657b83")
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(foreground-color "#657b83"))
