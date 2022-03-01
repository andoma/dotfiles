;; -*- mode: Lisp -*-

;;; Emfy 0.1.0-dev <https://github.com/susam/emfy>

;; Customize user interface.
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

 ;; mac specific settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t
        default-input-method "MacOSX")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/opt/homebrew/bin/"))
  )

;; Theme.
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")

;; Interactively do things.
;;(ido-mode 1)
;;(ido-everywhere)
;;(setq ido-enable-flex-matching t)

;; Scroll behavour
(setq scroll-step 1)
(setq compilation-scroll-output t)


;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Write customizations to a separate file instead of this file.
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Install packages.
(setq package-list '(markdown-mode paredit rainbow-delimiters clang-format+ projectile scad-mode))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Enable Paredit.
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Custom key-binding.
(global-set-key [end]  'end-of-buffer )
(global-set-key [home] 'beginning-of-buffer )
(global-set-key [f8]   'vc-git-grep )
(global-set-key [f9]   'dabbrev-expand )
(global-set-key [f10]  'compile )
(global-set-key [f11]  'previous-error )
(global-set-key [f12]  'next-error )
(global-set-key "\C-n" 'goto-line )

;; Clang-format stuff
(require 'clang-format+)
(setq clang-format-style "file")
(add-hook 'c-mode-common-hook #'clang-format+-mode)



(c-add-style "myc++"
         '((c-basic-offset . 4)
           (c-comment-only-line-offset . 0)
           (c-hanging-braces-alist . ((substatement-open before after)))
           (c-offsets-alist . ((topmost-intro        . 0)
                   (topmost-intro-cont   . 0)
                   (substatement         . 4)
                   (substatement-open    . 0)
                   (statement-case-open  . 4)
                   (statement-cont       . 4)
                   (access-label         . -4)
                   (inclass              . 4)
                   (inline-open          . 4)
                   (innamespace          . 0)
                   (inlambda             . 0)
                   ))))

(defun my-c++-mode-hook ()
  (c-set-style "myc++"))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Projectile
;;(require 'projectile)
;;(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))
