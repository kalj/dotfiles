; .emacs --

;; (defvar *emacs-load-start* (current-time))

;;-----------------------------------------------------------------------------
;; User variables
;;-----------------------------------------------------------------------------

(setq user-mail-address "k.ljungkvist@gmail.com")

;;-----------------------------------------------------------------------------
;; general behavior/appearance mods
;;-----------------------------------------------------------------------------

;; make org-mode default mode
;; (setq default-major-mode 'org-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; no splash screen
(setq inhibit-splash-screen t)

;; remove toolbar - handled in .Xdefaults, BUT the stupid emacsclient/server
;; framework doesn't get this and still uses it...
(tool-bar-mode -1)

;; disable useless C-z behavior when not in a terminal
(if window-system
    (global-unset-key (kbd "C-z")))

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

;; Make emacs play nice with the cut/paste buffers of X/Linux
(global-set-key [mouse-2] 'mouse-yank-primary)
(setq select-active-regions t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; remove annoying tooltips!
(tooltip-mode -1)

;; set the fill column to 80
(setq default-fill-column 80)

;; Only use spaces for indentation
(setq-default indent-tabs-mode nil)


;; trailing whitespaces are also evil
;; therefore, delete them on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
;; vi emulation
;;-----------------------------------------------------------------------------

;; Evil mode
(add-to-list 'load-path "~/.emacs.d/plugins/evil")
(require 'evil)
(evil-mode 1)

;; completely wipe all the evil insert-mode bindings
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;; Add an escape to switch out of insert mode
(define-key evil-insert-state-map "\C-[" 'evil-force-normal-state)
; Add another escape at f8
(global-set-key (kbd "<f8>") 'evil-force-normal-state)
; and at C-å
(global-set-key (kbd "C-å") 'evil-force-normal-state)


;; make esc quit stuff (i.e. replace C-g)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-abort)

(define-key evil-visual-state-map "q" 'my-comment-dwim)
(define-key evil-normal-state-map "q" 'my-comment-dwim)


;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

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

;; make C-u and C-w behave like in Unix / bash
(global-set-key (kbd "C-u")
                (lambda ()
                  (interactive)
                  (kill-line 0)))

(defun unix-werase-or-kill (arg)
  (interactive "*p")
  (if (and transient-mark-mode
           mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'unix-werase-or-kill)


(global-unset-key (kbd "C-x C-n"))

;; replace ispell-word by interactive ispell
(global-set-key (kbd "M-$") 'ispell)

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

  (defun my-comment-dwim-keep-mark (&optional arg)
    (interactive "*P")
    (my-comment-dwim arg)
    (setq deactivate-mark nil))

  (global-set-key "\M-;" 'my-comment-dwim)
  (global-set-key "\M-:" 'my-comment-dwim-keep-mark)
  )

(defun kill-ring-save-keep-mark(beg end)
  (interactive "r")
  (kill-ring-save beg end)
  (setq deactivate-mark nil))

  (global-set-key "\M-W" 'kill-ring-save-keep-mark)


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

;; make C-tab / C-M-tab cycle buffers
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-M-tab>") 'previous-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

;;-----------------------------------------------------------------------------
;; Change text size of everything
;;-----------------------------------------------------------------------------

(defun sacha/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun sacha/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))

(global-set-key (kbd "C-x C-.") 'sacha/increase-font-size)
(global-set-key (kbd "C-.")     'sacha/increase-font-size)
(global-set-key (kbd "C-x C-,") 'sacha/decrease-font-size)
(global-set-key (kbd "C-,")     'sacha/decrease-font-size)

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
;; Use ido to fascilitate buffer switching / file opening
;;-----------------------------------------------------------------------------

(require 'ido)
(setq ido-case-fold t)
(setq ido-enable-flex-matching t)
(ido-mode)
(setq ido-auto-merge-work-directories-length -1)

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
;; first add ~/.emacs.d/plugins to load-path
;;-----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/plugins")

;;-----------------------------------------------------------------------------
;; haskell-mode
;;-----------------------------------------------------------------------------

(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;-----------------------------------------------------------------------------
;; org-mode
;;-----------------------------------------------------------------------------

(require 'org)

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

;; Remap org-mode meta keys for convenience
(evil-declare-key 'normal org-mode-map
                  (kbd "M-l") 'org-metaright
                  (kbd "M-h") 'org-metaleft
                  (kbd "M-k") 'org-metaup
                  (kbd "M-j") 'org-metadown
                  (kbd "M-L") 'org-shiftmetaright
                  (kbd "M-H") 'org-shiftmetaleft
                  (kbd "M-K") 'org-shiftmetaup
                  (kbd "M-J") 'org-shiftmetadown)


;; org agenda -- leave in emacs mode but add j & k
(defun my-org-mode-hook ()
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line))
(add-hook 'org-mode-hook 'my-org-mode-hook)


(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-hide-leading-stars t)
(setq org-startup-folded 'content)

(setq org-agenda-custom-commands
      '(
        ("A" "Agenda + Todos"
         ((agenda)
          (todo "TODO")
          ))))

(setq org-agenda-files '("~/docs/work/todo.org" "~/docs/work/todo.org_archive"))

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
                                       (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".cu"))
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
                                       (".cxx"))
                                      ("\\.cu\\'"
                                       (".h")))))

;; realize
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-mode-hook ()
  ;; Changes the indentation of substatement parantheses
  (c-set-offset 'substatement-open 0))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(autoload 'cuda-mode "cuda-mode.el" "Cuda mode." t)
(setq auto-mode-alist (append '(("\\.cu$" . cuda-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cuh$" . cuda-mode)) auto-mode-alist))

;; upc
(setq auto-mode-alist (append '(("\\.upc$" . c-mode)) auto-mode-alist))

;;-----------------------------------------------------------------------------
;; Fortran
;;-----------------------------------------------------------------------------

(add-hook 'f90-mode-hook '(lambda ()
                            (local-unset-key (kbd "C-j"))))


;;-----------------------------------------------------------------------------
;; matlab
;;-----------------------------------------------------------------------------

(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(setq matlab-mode-hook
      '(lambda ()
;;       (setq matlab-indent-function t)       ; if you want function bodies
;;                                      ; indented
;;       (setq fill-column 76)         ; where auto-fill should wrap
;;       (turn-on-auto-fill)
         (local-unset-key (kbd "M-;"))
         (setq matlab-indent-level 4)
         (local-set-key (kbd "C-j") 'matlab-return)
         ))


;;-----------------------------------------------------------------------------
;; abc-mode
;;-----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.abc\\'"  . abc-mode))
(add-to-list 'auto-mode-alist '("\\.abp\\'"  . abc-mode))
(autoload 'abc-mode "abc-mode" "abc music files" t)

(defun abc-show-current-ps()
  (interactive)
  (save-buffer)
  (shell-command
   (read-from-minibuffer
    "Options: "
    (concat "evince" " "
            (replace-regexp-in-string "\.abc$" ".ps" buffer-file-name)
            " &"
            ))))



(setq abc-mode-hook
      '(lambda ()
         (local-set-key (kbd "C-c C-c") 'abc-run-abc2ps-all)
         (local-set-key (kbd "C-c C-v") 'abc-show-current-ps)
         ))


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
     (add-to-list 'LaTeX-verbatim-environments "comment")
     (add-to-list 'TeX-expand-list
                  '("%(RubberPDF)"
                    (lambda ()
                      (if
                          (not TeX-PDF-mode)
                          ""
                        "--pdf"))))
     (add-to-list 'TeX-command-list
                  '("Rubber" "rubber %(RubberPDF) %t" TeX-run-shell nil t) t)
     ))

(setq-default TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv")
                                                 (output-dvi "xdvi") (output-pdf "xdg-open")
                                                 (output-html "xdg-open"))))

(setq-default TeX-PDF-mode t)
(setq-default TeX-command-default "Rubber")

(defun my-latex-mode-hook()
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  ;; (setq TeX-command-default "XeLaTeX")
  (setq TeX-save-query nil)
  ;; (setq TeX-show-compilation t)
  (orgtbl-mode)
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


(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")

(when (> emacs-major-version 21)
  (require 'yasnippet)
  (yas/initialize)
  (yas-global-mode 1)
  )

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
;;                           ac-source-abbrev ac-source-words-in-buffer))

;;   (setq ac-modes
;;      (append ac-modes
;;              '(eshell-mode
;;                ;;org-mode
;;                )))
;;   ;; (add-to-list 'ac-trigger-commands 'org-self-insert-command)

;;   (add-hook 'emacs-lisp-mode-hook
;;          (lambda ()
;;            (setq ac-sources '(ac-source-yasnippet
;;                               ac-source-abbrev
;;                               ac-source-words-in-buffer
;;                               ac-source-symbols))))

;;   (add-hook 'eshell-mode-hook
;;          (lambda ()
;;            (setq ac-sources '(ac-source-yasnippet
;;                               ac-source-abbrev
;;                               ac-source-files-in-current-dir
;;                               ac-source-words-in-buffer))))
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
;; Arduino mode
;;-----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/plugins/arduino-mode")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

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
;; dead keys
;;-----------------------------------------------------------------------------

(require 'iso-transl)

;;-----------------------------------------------------------------------------
;; Custom
;;-----------------------------------------------------------------------------

;; (message "My .emacs loaded in %ds"
;;       (destructuring-bind (hi lo ms) (current-time)
;;         (- (+ hi lo) (+ (first *emacs-load-start*)
;;                         (second *emacs-load-start*)))))

