;;; kalj-core.el --- Basic behaviour, appearance, encoding, backups

;; Split verbatim from ~/.emacs; loaded from there.

;;-----------------------------------------------------------------------------
;; general behavior/appearance mods
;;-----------------------------------------------------------------------------

;; make org-mode default mode
;; (setq default-major-mode 'org-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; no splash screen
(setq inhibit-splash-screen t)

;; no dialog boxes
(setq use-dialog-box nil)

;; remove toolbar - handled in .Xdefaults, BUT the stupid emacsclient/server
;; framework doesn't get this and still uses it...
(tool-bar-mode -1)

;; disable useless C-z behavior when not in a terminal
(if (display-graphic-p)
    (global-unset-key (kbd "C-x C-z")))

;; remove menu bar (can't be handled in Xresources, since should hold for -nw also)
(menu-bar-mode -1)

;; Change title bar to ~/file-path if the current buffer is a
;; real file or buffer name if it is just a buffer.
(setq frame-title-format
      '(:eval
        (if buffer-file-name
            (replace-regexp-in-string (getenv "HOME") "~"
                                      buffer-file-name)
          (buffer-name))))


;; inverse colors theme
(defun my-black-mode ()
  (interactive)
  (set-background-color "black")
  (set-foreground-color "#c7c4c4")
  (set-cursor-color "#c7c4c4")
  )

(defun my-white-mode ()
  (interactive)
  (set-background-color "white")
  (set-foreground-color "black")
  (set-cursor-color "black")
  )

(defun my-light-mode ()
  (interactive)
  (set-background-color "#ffffe0")
  (set-foreground-color "black")
  (set-cursor-color "black")
  )

;; activate highlighting of selection
(setq transient-mark-mode t)

(setq-default truncate-partial-width-windows nil)

(setq scroll-conservatively 5)

;; Cursor in same relative row and column during PgUP/DN
(setq scroll-preserve-screen-position t)

;; Start scrolling when 2 lines from top/bottom
(setq scroll-margin 2)

;; Always paste at the cursor
(setq mouse-yank-at-point t)

(delete-selection-mode t)

;; enable standard mouse copy/paste behavior
(setq select-enable-primary t)

;; remove annoying tooltips!
(tooltip-mode -1)

;; set the fill column
(setq-default fill-column 100)

;; Only use spaces for indentation
(setq-default indent-tabs-mode nil)

(defvar my-basic-offset 4)

(setq-default tab-width my-basic-offset)

;; trailing whitespaces are evil
;; therefore, delete them on save
;; BUT, only if .dir-local.el doesn't protest

(defvar dont-delete-trailing-whitespace t)
(make-variable-buffer-local 'dont-delete-trailing-whitespace)

(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if dont-delete-trailing-whitespace is unset."
  (or dont-delete-trailing-whitespace
       (progn
     (delete-trailing-whitespace)
     )))

(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)

;; Show column number in modeline
(setq column-number-mode t)

;; required on old emacs (at least on uppmax):
(global-font-lock-mode t)

;; Always flash for parens
(show-paren-mode 1)

;; provide an error trace if loading .emacs fails
(setq debug-on-error t)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; ignore case when completing buffer names
(setq read-buffer-completion-ignore-case 1)

;; always follow vc symlinks
(setq vc-follow-symlinks t)

;;-----------------------------------------------------------------------------
;; Encoding systems
;;-----------------------------------------------------------------------------

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;;-----------------------------------------------------------------------------
;; Backup and autosave folders
;;-----------------------------------------------------------------------------

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!

(defvar autosave-dir
  (concat "/tmp/emacs_autosaves-" (user-real-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
       (concat "#" (file-name-nondirectory buffer-file-name) "#")
      (concat "#%" (buffer-name) "#"))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups-" (user-real-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;;-----------------------------------------------------------------------------
;; dead keys
;;-----------------------------------------------------------------------------

(require 'iso-transl)

(provide 'kalj-core)
;;; kalj-core.el ends here
