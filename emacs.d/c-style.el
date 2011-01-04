;;******************************************
;;
;; c-style.el
;;
;; This file contains various functions for
;; handling different c-styles.
;;
;; Erik Änggård
;;
;;******************************************

;;
;; This c-style is more correct than the "bsd" style included with emacs.
;; Verify against /usr/share/misc/style !
;;

(defconst netbsd-knf-style
  '((c-auto-newline . nil)
;;    (c-tab-always-indent . nil)
    (c-recognize-knr-p . t)
    (c-basic-offset . 8)
    (c-comment-only-line-offset . 0)
    (c-cleanup-list . (brace-else-brace
		       empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    (c-hanging-braces-alist . ((defun-open . (before after))
			       (defun-close . (before))
			       (class-open . (after))
			       (class-close . nil)
			       (inline-open . nil)
			       (inline-close . nil)
			       (block-open . (after))
			       (block-close . (before))
			       (substatement-open . nil)
			       (statement-case-open . nil)
			       (brace-list-open . nil)
			       (brace-list-close . nil)
			       (brace-list-intro . nil)
			       (brace-list-entry . nil)
			       ))
    (c-offsets-alist . ((knr-argdecl-intro . +)
			(arglist-cont-nonempty . 4)
			(knr-argdecl . 0)
			(block-open . -)
			(label . -)
			(statement-cont . 4)
			)))
  "NetBSD KNF Style")

(c-add-style "netbsd-knf" netbsd-knf-style nil)

(defconst ffmpeg-style
  '((indent-tabs-mode nil)
    (show-trailing-whitespace t)
    (setq c-basic-offset 4)))

(c-add-style "ffmpeg" ffmpeg-style nil)

;(defun knf-c-mode-hook ()
;  ;; Add style and set it for current buffer
;  (c-add-style "netbsd-knf" netbsd-knf-style t)
;  ;; Offset customizations that are not in this style (??)
;  (c-set-offset 'member-init-intro '++)
;  ;; Other stuff
;  (setq tab-width 8
;	indent-tabs-mode t)
;  (c-toggle-auto-hungry-state 1)
;  ;;  (define-key c-mode-map "\C-m" 'newline-and-indent)
;)

;(add-hook 'c-mode-hook 'knf-c-mode-hook)


;;
;; Various functions for switching c-style
;;

(defun c-style-switch ()
  "Switch between BSD, GNU and user c-style."
  (interactive)
  (if (equal "netbsd-knf" c-indentation-style)
      (c-set-style "gnu")
    (if (equal "gnu" c-indentation-style)
	(c-set-style "user")
      (c-set-style "netbsd-knf")))
  (message "c-style: %s" c-indentation-style)
  (setq mode-name (concat "C [" c-indentation-style "]")))


;;
;; Add hook to c-mode that tries to figure out what c-style (bsd or gnu)
;; to use by reading first 5000 characters of the buffer.
;;
(defun count-bsd (str cnt)
  "count bsd indents in string"
  (let ((d (string-match "^\t[^ \t]" str)))
    (if d
	(count-bsd (substring str (+ d 2)) (+ cnt 1))
      cnt)))

(defun count-gnu (str cnt)
  "count gnu indents in string"
  (let ((d (string-match "^  [^ \t]" str)))
    (if d
	(count-gnu (substring str (+ d 2)) (+ cnt 1))
      cnt)))


(defun count-user (str cnt)
  "count gnu indents in string"
  (let ((d (string-match "^    [^ \t]" str)))
    (if d
	(count-user (substring str (+ d 4)) (+ cnt 1))
      cnt)))

(defun c-style-select-hook ()
  "Select netbsd-knf, user (net-snmp) or gnu c-style depending on buffer contents."
  (let ((s (max 1 (min (buffer-size) 5000))))
    (let ((bsdcnt (count-bsd (buffer-substring 1 s) 0))
	  (gnucnt (count-gnu (buffer-substring 1 s) 0))
	  (usercnt (count-user (buffer-substring 1 s) 0)))
;    (message-box "bsd=%d gnu=%d user=%d style=%s" 
;		 bsdcnt gnucnt usercnt c-indentation-style)
    (if (and (> gnucnt bsdcnt) (> gnucnt usercnt))
	(c-set-style "gnu")
      (if (and (> bsdcnt gnucnt) (> bsdcnt usercnt))
	  (c-set-style "netbsd-knf")
	(if (and (> usercnt gnucnt) (> usercnt bsdcnt))
	    (c-set-style "user")
	  (c-set-style c-default-style))))
    (setq mode-name (concat "C [" c-indentation-style "]")))))

(add-hook 'c-mode-hook 'c-style-select-hook)

(provide 'c-style)

;;
;; Add something like this to your .emacs or .xemacs/init.el
;; (Replace ~eriang/emacs below with the path to the directory containing 
;; this file and replace [f10] with the function key of your liking).
;;

;(setq load-path (cons (expand-file-name "~eriang/emacs") load-path))
;(require 'c-style)
;(global-set-key [f10] 'c-style-switch)

