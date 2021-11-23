
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(modify-frame-parameters nil '((wait-for-wm . nil)))
(global-set-key "\C-n" 'goto-line)
(global-set-key "\C-b" 'dabbrev-expand)
(global-set-key [end]  'end-of-buffer )
(global-set-key [home] 'beginning-of-buffer )
(global-set-key [f9]   'dabbrev-expand )
(global-set-key [f10]  'compile )
(global-set-key [f11]  'previous-error )
(global-set-key [f12]  'next-error )
(which-func-mode t)
(require 'iso-transl)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(global-font-lock-mode t)

(column-number-mode t)
(setq scroll-step 1)
(setq compilation-scroll-output t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default js-indent-level 2)

(defun my-c-mode-hook ()
  (interactive)
  (setq imenu-auto-rescan t)
  (local-set-key "\C-l" (lambda () 
                           (interactive)
                           (imenu--make-index-alist nil)
                           (recenter))))



(setq auto-mode-alist (append '(("\\.gnus$" . lisp-mode)
				("^[mM]akefile" . makefile-mode)
				("\\.view$" . js-mode)
				("\\.cu$" . c++-mode)
				("\\.mst$" . html-mode)
				)
			      auto-mode-alist))

(add-hook 'c-mode-hook 'my-c-mode-hook)


(put 'narrow-to-region 'disabled nil)
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(package-selected-packages (quote (typescript-mode)))
 '(tool-bar-mode nil))

(setq load-path (cons (expand-file-name "~/dotfiles/emacs.d") load-path))
(require 'go-mode)
(require 'yaml-mode)
(require 'color-theme)
(color-theme-initialize)
(color-theme-taylor)

;;(require 'rust-mode)
;;(setq rust-format-on-save t)
;;(setq exec-path (append exec-path '("~/.cargo/bin")))

(require 'scad-mode)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t
        default-input-method "MacOSX")

  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

)

(c-add-style "myc++"
         '((c-basic-offset . 2)
           (c-comment-only-line-offset . 0)
           (c-hanging-braces-alist . ((substatement-open before after)))
           (c-offsets-alist . ((topmost-intro        . 0)
                   (topmost-intro-cont   . 0)
                   (substatement         . 2)
                   (substatement-open    . 0)
                   (statement-case-open  . 2)
                   (statement-cont       . 2)
                   (access-label         . -2)
                   (inclass              . 2)
                   (inline-open          . 2)
                   (innamespace          . 0)
                   ))))


(c-add-style "llvm"
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



(c-set-offset 'innamespace 0)

;; There's something similar (but fancier) in vc-git.el: vc-git-grep

;; -I means don't search through binary files

;; --no-color, oddly enough, is required to allow emacs to colorize the output

(defcustom git-grep-switches "-I -n -F --no-color"
  "Switches to pass to `git grep'."
  :type 'string)

(defcustom git-grep-default-work-tree (expand-file-name "~/showtime")
  "Top of your favorite git working tree.  \\[git-grep] will search from here if it cannot figure out where else to look."
  :type 'directory
  )

(when (require 'vc-git nil t)

  ;; Uncomment this to try out the built-in-to-Emacs function.
  ;;(defalias 'git-grep 'vc-git-grep)

  (defun git-grep (command-args)
    (interactive
     (let ((root (vc-git-root default-directory)))
       (when (not root)
         (setq root git-grep-default-work-tree)
         (message "git-grep: %s doesn't look like a git working tree; searching from %s instead" default-directory root))
       (list (read-shell-command "Run git-grep (like this): "
                                 (format (concat
                                          "cd %s && "
                                          "git --no-pager grep %s -e %s")
                                         root
                                         git-grep-switches
                                         (let ((thing (and

                                        ; don't snarf stuff from the
                                        ; buffer if we're not looking
                                        ; at a file.  Perhaps we
                                        ; should also check to see if
                                        ; the file is part of a git
                                        ; repo.
                                                       buffer-file-name
                                                       (thing-at-point 'symbol))))
                                           (or (and thing (progn
                                                            (set-text-properties 0 (length thing) nil thing)
                                                            (shell-quote-argument (regexp-quote thing))))
                                               "")))
                                 'git-grep-history))))
    (let ((grep-use-null-device nil))
      (grep command-args))))


(global-set-key [f8]   'git-grep )


(defun my-switch-c-h ()
 (interactive)
 (let* ((name (buffer-file-name)))
   (if (string-match "\\.c$" name)
       (progn
         (setq name (concat (substring name 0 (1- (length name))) "h"))
         (find-file name))
     (if (string-match "\\.h$" name)
         (progn
           (setq name (concat (substring name 0 (1- (length name))) "c"))
           (find-file name))))))

(global-set-key [f4] 'my-switch-c-h)




(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:foreground "brightred")))))
