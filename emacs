(modify-frame-parameters nil '((wait-for-wm . nil)))

(global-set-key "\C-n" 'goto-line)
(global-set-key "\C-b" 'dabbrev-expand)
(global-set-key [end]  'end-of-buffer )
(global-set-key [home] 'beginning-of-buffer )
(global-set-key [f9]   'dabbrev-expand )
(global-set-key [f10]  'compile )
(global-set-key [f11]  'previous-error )
(global-set-key [f12]  'next-error )
(set-default-font "fixed")
(set-cursor-color "white")
(set-mouse-color "white")
(which-func-mode t)

(tool-bar-mode -1)
(global-font-lock-mode t)

(column-number-mode t)
(setq scroll-step 1)

(defun my-c-mode-hook ()
  (interactive)
  (setq imenu-auto-rescan t)
  (local-set-key "\C-l" (lambda () 
                           (interactive)
                           (imenu--make-index-alist nil)
                           (recenter))))



(setq auto-mode-alist (append '(("\\.gnus$" . lisp-mode)
				("^[mM]akefile" . makefile-mode))
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
 '(load-home-init-file t t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "bitstream" :family "Courier 10 Pitch")))))

(setq load-path (cons (expand-file-name "~andoma/.dotfiles/emacs.d") load-path))
(require 'c-style)

(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
