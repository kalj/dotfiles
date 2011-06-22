; .emacs --

;;-----------------------------------------------------------------------------
;; User variables
;;-----------------------------------------------------------------------------

(setq user-mail-address "k.ljungkvist@gmail.com")

;;-----------------------------------------------------------------------------
;; general behavior/appearance mods
;;-----------------------------------------------------------------------------

;; (defvar *emacs-load-start* (current-time))

;; make org-mode default mode
;; (setq default-major-mode 'org-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; no splash screen
(setq inhibit-splash-screen t)

;; remove toolbar - handled in .Xdefaults, BUT the stupid emacsclient/server
;; framework doesn't get this and still uses it...
(tool-bar-mode -1)

;; remove the scrollbar - handled in .Xdefaults
;; (scroll-bar-mode -1)

;; remove menu bar (can't be handled in Xresources, since should hold for -nw also)
;; (if (not window-system)
    (menu-bar-mode -1)
  ;; nil)

;; Change title bar to ~/file-path if the current buffer is a
;; real file or buffer name if it is just a buffer.
(setq frame-title-format
      '(:eval
        (if buffer-file-name
            (replace-regexp-in-string (getenv "HOME") "~"
				      buffer-file-name)
          (buffer-name))))

;; default font and size - handled in .Xdefaults
;; (set-default-font "Monospace-8")

;;(setq initial-frame-alist '((width . 82) (height . 47)))

;; inverse colors theme
(defun my-black-mode ()
  (interactive)
  (set-face-foreground 'modeline "black")
  (set-background-color "black")
  (set-foreground-color "#c7c4c4")
  (set-cursor-color "#c7c4c4")
  )

(defun my-white-mode ()
  (interactive)
  (set-face-foreground 'modeline "black")
  (set-background-color "white")
  (set-foreground-color "black")
  (set-cursor-color "black")
  )

(defun my-light-mode ()
  (interactive)
  (set-face-foreground 'modeline "black")
  (set-background-color "#ffffe0")
  (set-foreground-color "black")
  (set-cursor-color "black")
  )

;; enable - handled in .Xdefaults
;; (if window-system
    ;; (my-day-mode))

;; activate highlighting of selection
(setq transient-mark-mode t)

(setq-default truncate-partial-width-windows nil)

;; (setq scroll-step 2)
(setq scroll-conservatively 5)

;; Cursor in same relative row and column during PgUP/DN
(setq scroll-preserve-screen-position t)

;; Start scrolling when 2 lines from top/bottom
(setq scroll-margin 2)

;; Always paste at the cursor
(setq mouse-yank-at-point t)

(delete-selection-mode t)

;; (mouse-avoidance-mode 'exile)

;; remove annoying tooltips!
(tooltip-mode -1)

;; set the fill column to 80
(setq default-fill-column 80)

;; Only use spaces for indentation
(setq indent-tabs-mode nil)

;; Don't hide pointer when typing
;; (setq make-pointer-invisible nil)

;; Show column number in modeline
(setq column-number-mode t)

;; required on old emacs (at least on uppmax):
(global-font-lock-mode t)

;; Always flash for parens and define a more distinctive color
;; (show-paren-mode 1)
;; (set-face-foreground 'show-paren-match-face "#3f0000")

;; provide an error trace if loading .emacs fails
(setq debug-on-error t)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; ignore case when completing buffer names
(setq read-buffer-completion-ignore-case 1)

;;-----------------------------------------------------------------------------
;; Global key bindings
;;-----------------------------------------------------------------------------

;; move up or down a paragraph
(global-set-key (kbd "C-M-n") 'forward-paragraph)
(global-set-key (kbd "C-M-p") 'backward-paragraph)

;; Fix selection when moving around with caps lock enabled
(global-set-key (kbd "C-S-p") 'previous-line)
(global-set-key (kbd "C-S-n") 'next-line)
(global-set-key (kbd "C-S-f") 'forward-char)
(global-set-key (kbd "C-S-b") 'backward-char)
(global-set-key (kbd "C-S-a") 'move-beginning-of-line)
(global-set-key (kbd "C-S-e") 'move-end-of-line)
(global-set-key (kbd "M-S-f") 'forward-word)
(global-set-key (kbd "M-S-b") 'backward-word)

;; replace ispell-word by interactive ispell
(global-set-key (kbd "M-$") 'ispell)

;; Activation of vi-mode
;; (global-set-key (kbd "C-z") 'vi-mode)

(when (> emacs-major-version 21)
  ;; Improve the comment-dwim command. Partially with help from here:
  ;; http://www.emacswiki.org/emacs/CommentingCode
  (defun toggle-comment-line()
    "Toggles the commentation of the current line."
    (interactive "*")
    (comment-or-uncomment-region (line-beginning-position)
				 (line-end-position)))
  (defun my-comment-dwim (&optional arg)
    "Improvement of the comment-dwim command.
If no region is selected and current line is not blank then run
`toggle-comment-line' of current line, otherwise run
`comment-dwim'. Replaces default behaviour of comment-dwim, which
then inserts a comment at the end of the line."
    (interactive "*P")
    (comment-normalize-vars)
    (if (and (not (region-active-p))
	     (not (save-excursion (forward-line 0) (looking-at "^[ \t]*$"))))
	(toggle-comment-line)
      (comment-dwim arg)))

  (global-set-key "\M-;" 'my-comment-dwim)
  )

;; make the f* keys do something useful
(defun my-initial-split()
  "Replaces current window layout with one horizontal split."
  (interactive)
  (delete-other-windows nil)
  (split-window-horizontally nil)
  (switch-to-buffer-other-window nil)
  )

;; fix font colors
(global-set-key [f5] 'font-lock-fontify-buffer)

;; compile code using f12 and f11
(global-set-key [f12] 'compile)
(global-set-key [f11] 'recompile)

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
	  ;; and delete the window
	  (delete-window (get-buffer-window (get-buffer "*compilation*"))))
	;; and return to whatever were looking at before
	;; (replace-buffer-in-windows "*compilation*"))
	;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))

;; Quicker access to go-to line
;; (global-set-key (kbd "M-g") 'goto-line)

;; From anrxc: Menu bar toggle, as in his vimperator setup
(global-set-key (kbd "<M-down>") 'menu-bar-mode)
(global-set-key (kbd "<M-up>") 'menu-bar-mode)

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
;; Command for reloading this file
;;-----------------------------------------------------------------------------

(defun reload-init-file ()
  "Reload emacs init file (~/.emacs)"
  (interactive)
  (load-file "~/.emacs")
  )

(defun edit-init-file ()
  "Opens emacs init file for editing (~/.emacs)"
  (interactive)
  (find-file "~/.emacs")
  )

;; bind to keys
(global-set-key "\C-c\C-r" 'reload-init-file)
(global-set-key "\C-c\C-e" 'edit-init-file)

;;-----------------------------------------------------------------------------
;; vi emulation
;;-----------------------------------------------------------------------------

;; Try out viper-mode!
;; (setq viper-mode t)
;; (require 'viper)

;; Activation of vi-mode
(global-set-key (kbd "<escape>") 'vi-mode)
;; (global-set-key (kbd "C-z") 'vi-mode)

;;-----------------------------------------------------------------------------
;; first add ~/.emacs.d to load-path
;;-----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d")

;;-----------------------------------------------------------------------------
;; haskell-mode
;;-----------------------------------------------------------------------------

(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;-----------------------------------------------------------------------------
;; org-mode customizations
;;-----------------------------------------------------------------------------

;; (setq org-format-latex-header
;;       "\\documentclass[a4paper]{article}
;; \\usepackage[utf8]{inputenc}
;; \\usepackage[T1]{fontenc}
;; \\usepackage[english]{babel}
;; \\usepackage[margin=3cm]{geometry}
;; \\usepackage{palatino}
;; \\usepackage{fullpage}         % do not remove
;; \\usepackage[usenames]{color}
;; \\usepackage{amsmath,amsfonts,mathrsfs,amssymb}
;; \\usepackage{latexsym}
;; \\usepackage[mathscr]{eucal}
;; \\pagestyle{empty}             % do not remove")

;;-----------------------------------------------------------------------------
;; c-mode customizations
;;-----------------------------------------------------------------------------

;; use c++-mode as default for .h header files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; customizations for all c-related modes
(defun my-c-mode-common-hook ()
  ;; make standard indentation offset :=4
  (setq c-basic-offset 4)
  ;; find-other-file key binding in c-mode
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  ;; Changes the indentation of substatement parantheses
  (c-set-offset 'substatement-open 0)
  (setq-default cc-other-file-alist '(
				      ("\\.cc\\'"
				       (".hh" ".h"))
				      ("\\.hh\\'"
				       (".cc" ".C"))
				      ("\\.c\\'"
				       (".h"))
				      ("\\.h\\'"
				       (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))
				      ("\\.C\\'"
				       (".H" ".hh" ".h"))
				      ("\\.H\\'"
				       (".C" ".CC" ".cpp"))
				      ("\\.CC\\'"
				       (".HH" ".H" ".hh" ".h"))
				      ("\\.HH\\'"
				       (".CC"))
				      ("\\.c\\+\\+\\'"
				       (".h++" ".hh" ".h"))
				      ("\\.h\\+\\+\\'"
				       (".c++"))
				      ("\\.cpp\\'"
				       (".hpp" ".hh" ".h" ".H"))
				      ("\\.hpp\\'"
				       (".cpp"))
				      ("\\.cxx\\'"
				       (".hxx" ".hh" ".h"))
				      ("\\.hxx\\'"
				       (".cxx")))))

;; realize
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-mode-hook ()
  ;; Changes the indentation of substatement parantheses
  (c-set-offset 'substatement-open 0))
(add-hook 'java-mode-hook 'my-java-mode-hook)


;;-----------------------------------------------------------------------------
;; Fortran
;;-----------------------------------------------------------------------------

(add-hook 'f90-mode-hook '(lambda ()
			    (local-unset-key (kbd "C-j"))))


;;-----------------------------------------------------------------------------
;; matlab
;;-----------------------------------------------------------------------------

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(setq matlab-mode-hook
      '(lambda ()
;; 	 (setq matlab-indent-function t)       ; if you want function bodies
;; 					; indented
;; 	 (setq fill-column 76)         ; where auto-fill should wrap
;; 	 (turn-on-auto-fill)
	 (local-unset-key (kbd "M-;"))
	 (setq matlab-indent-level 4)
	 ))


(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)


;;-----------------------------------------------------------------------------
;; abc-mode
;;-----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.abc\\'"  . abc-mode))
(add-to-list 'auto-mode-alist '("\\.abp\\'"  . abc-mode))
(autoload 'abc-mode "abc-mode" "abc music files" t)
;; (add-to-list 'auto-insert-alist '(abc-mode . abc-skeleton))

;;-----------------------------------------------------------------------------
;; LaTeX
;;-----------------------------------------------------------------------------

;; these explicit loads are not so bad since they don't really load anything,
;; but rather set some autoloads
(load "auctex.el" nil t t)

(load "preview-latex.el" nil t t)

;; (setq tex-dvi-view-command "xdvi")

(eval-after-load "latex"
  '(progn
     (define-key LaTeX-math-keymap "/" 'LaTeX-math-frac)
     (define-key LaTeX-math-keymap "2" 'LaTeX-math-sqrt)
     (setq ispell-tex-skip-alists
	   (list
	    (append
	     (car ispell-tex-skip-alists) ;tell ispell to ignore content of this:
	     '(
	       ("\\[" . "\\]")
	       ;; ("\\\\verb\\\\|" . "\\\\|")
	       ("\\\\eqref" ispell-tex-arg-end)
	       ("\\\\secref" ispell-tex-arg-end)
	       ("\\\\liref" ispell-tex-arg-end)
	       ("\\\\fgref" ispell-tex-arg-end)
	       ("\\\\tbref" ispell-tex-arg-end)
	       ("\\\\alref" ispell-tex-arg-end)
	       ;; ("\\\\label" ispell-tex-arg-end)
	       ;; ("\\\\" ispell-tex-arg-end)
	       ))
	    (cadr ispell-tex-skip-alists)))
	   ))

(setq-default TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "xdg-open") (output-html "xdg-open"))))

(setq-default TeX-PDF-mode t)

(defun my-latex-mode-hook()
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  ;; (setq TeX-command-default "XeLaTeX")
  (setq TeX-save-query nil)
  ;; (setq TeX-show-compilation t)
  )


(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)

;;-----------------------------------------------------------------------------
;; use cperl-mode instead of perl-mode, or maybe not...
;;-----------------------------------------------------------------------------

;; (defalias 'perl-mode 'cperl-mode)

;;-----------------------------------------------------------------------------
;; Auto completion, yasnippet,
;; rope/ropemacs/pymacs, etc
;;-----------------------------------------------------------------------------

;; TODO: move this to an external file, and shut off loading of pymacs except
;; when running python code. And also disable automatic popups (as this really
;; slows things down...)


; (add-to-list 'load-path
; 	     "~/.emacs.d/plugins/yasnippet-0.6.1c")

; (when (> emacs-major-version 21)
;   (require 'yasnippet)
;   (yas/initialize)
;   (yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
; )

;; ;; [sic] Almost people hate rope to use `C-x p'.
;; (unless (boundp 'ropemacs-global-prefix)
;;     (setq ropemacs-global-prefix nil))

;; (eval-after-load "python-mode"
;;   '(progn
;;      (pymacs-load "ropemacs" "rope-")
;;   ))

;; (setq ropemacs-enable-autoimport t)


;; (when (require 'auto-complete nil t)
;;  (require 'auto-complete-yasnippet)
;;   (require 'auto-complete-python)
;;   (ac-ropemacs-init)
;;   (require 'auto-complete-emacs-lisp)
;;   (require 'auto-complete-cpp)

;;   (global-auto-complete-mode t)
;; ;  (set-face-background 'ac-menu-face "lightgray")
;; ;  (set-face-underline 'ac-menu-face "darkgray")
;;   (set-face-background 'ac-selection-face "steelblue")
;;   (define-key ac-complete-mode-map "\t" 'ac-expand)
;;   (define-key ac-complete-mode-map "\r" 'ac-complete)
;;   (define-key ac-complete-mode-map "\M-n" 'ac-next)
;;   (define-key ac-complete-mode-map "\M-p" 'ac-previous)
;;   (setq ac-auto-start nil)
;;   (setq ac-dwim t)
;;   (set-default 'ac-sources '(ac-source-yasnippet
;; 			     ac-source-abbrev ac-source-words-in-buffer))

;;   (setq ac-modes
;; 	(append ac-modes
;; 		'(eshell-mode
;; 		  ;;org-mode
;; 		  )))
;;   ;; (add-to-list 'ac-trigger-commands 'org-self-insert-command)

;;   (add-hook 'emacs-lisp-mode-hook
;; 	    (lambda ()
;; 	      (setq ac-sources '(ac-source-yasnippet
;; 				 ac-source-abbrev
;; 				 ac-source-words-in-buffer
;; 				 ac-source-symbols))))

;;   (add-hook 'eshell-mode-hook
;;   	    (lambda ()
;;   	      (setq ac-sources '(ac-source-yasnippet
;;   				 ac-source-abbrev
;;   				 ac-source-files-in-current-dir
;;   				 ac-source-words-in-buffer))))
;;   )

;;-----------------------------------------------------------------------------
;; shell mode customizations
;;-----------------------------------------------------------------------------

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;;-----------------------------------------------------------------------------
;; css mode customizations
;;-----------------------------------------------------------------------------

;; css mode customizations
(setq cssm-indent-function #'cssm-c-style-indenter)

;;-----------------------------------------------------------------------------
;; SGML mode customization
;;-----------------------------------------------------------------------------

;; (defun my-sgml-mode-hook ()
  ;; Changes the indentation of substatement parantheses
  ;; (setq sgml-basic-offset 8))

;; (add-hook 'sgml-mode-hook 'my-sgml-mode-hook)

;;-----------------------------------------------------------------------------
;; Javascript mode customization
;;-----------------------------------------------------------------------------

(add-hook 'js-mode-hook (lambda ()
			  (setq tab-width 4)))
;;-----------------------------------------------------------------------------
;; Templates
;;-----------------------------------------------------------------------------

;; make emacs auto-load templates
(require 'template)
(template-initialize)

;;-----------------------------------------------------------------------------
;; PKGBUILD mode
;;-----------------------------------------------------------------------------

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;;-----------------------------------------------------------------------------
;; Ediff
;;-----------------------------------------------------------------------------

;; Disable the ediff popup and integrate it instead (Xmonad doesn't handle this
;; very well).
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Split horizontally by default
(setq ediff-split-window-function 'split-window-horizontally)

;;-----------------------------------------------------------------------------
;; ediff command-line switch
;;-----------------------------------------------------------------------------

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;;-----------------------------------------------------------------------------
;; Tramp mode
;;-----------------------------------------------------------------------------

;; (require 'tramp)
;; (add-to-list 'tramp-default-proxies-alist
	     ;; '("localhost.localdomain" "\\`root\\'" "/sudo:%h:")
	     ;; '(nil "\\`root\\'" "/ssh:%h:"))

;;-----------------------------------------------------------------------------
;; Thunderbird email mode
;;-----------------------------------------------------------------------------

(require 'tbemail)

;;-----------------------------------------------------------------------------
;; Custom
;;-----------------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)

;; (message "My .emacs loaded in %ds"
;; 	 (destructuring-bind (hi lo ms) (current-time)
;; 	   (- (+ hi lo) (+ (first *emacs-load-start*)
;; 			   (second *emacs-load-start*)))))

(setq indent-tabs-mode nil)

