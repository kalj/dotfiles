; .emacs --

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

;; enable standard mouse copy/paste behavior
(setq x-select-enable-primary t)

;; remove annoying tooltips!
(tooltip-mode -1)

;; set the fill column to 80
(setq default-fill-column 80)

;; Only use spaces for indentation
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

;; trailing whitespaces are evil
;; therefore, delete them on save
;; BUT, only if .dir-local.el doesn't protest

(setq dont-delete-trailing-whitespace nil)
(make-variable-buffer-local 'dont-delete-trailing-whitespace)

(defun maybe-delete-trailing-whitespace () ""
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
;; packages
;;-----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package diminish)

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


;; (global-unset-key (kbd "C-x C-n"))

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

;; fix font colors
(global-set-key [f5] 'font-lock-fontify-buffer)


;; Quicker access to go-to line
;; (global-set-key (kbd "M-g") 'goto-line)

;; From anrxc: Menu bar toggle, as in his vimperator setup
(global-set-key (kbd "<M-down>") 'menu-bar-mode)
(global-set-key (kbd "<M-up>") 'menu-bar-mode)

;;-------------------------------------------------------------------
;; compile code using f12 and f11
;;-------------------------------------------------------------------

(use-package compile
  :bind (( [f12] . compile)
         ([f11] . recompile))
  :config
  ; skip anything but errors
  (setq compilation-skip-threshold 2)

  (setq compilation-scroll-output t)

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
  )

;;-----------------------------------------------------------------------------
;; Font size
;;-----------------------------------------------------------------------------

; Change text size of everything
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
(global-set-key (kbd "C-x C-,") 'sacha/decrease-font-size)

;;-----------------------------------------------------------------------------
;; which-key
;;-----------------------------------------------------------------------------

(use-package which-key
  :demand
  :config
  (add-hook 'after-init-hook 'which-key-mode))

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

(use-package ido
  :diminish ido-mode
  :config
  (setq ido-case-fold t
        ido-use-faces t
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1)

  ;; (setq ido-use-filename-at-point nil)
  ;; (setq ido-use-virtual-buffers t))
  ;; (ido-ubiquitous-mode 1)
  (ido-mode)
  (ido-everywhere)
)

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
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "C-c e") 'edit-init-file)

;;-----------------------------------------------------------------------------
;; org-mode
;;-----------------------------------------------------------------------------

;; disabled for now, see git history for setup

;;-------------------------------------------------------------------
;; function for converting from camelcase to snake case
;;-------------------------------------------------------------------

(defun to-snake-case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
    (message "No region selected")))

;;-----------------------------------------------------------------------------
;; c-mode customizations
;;-----------------------------------------------------------------------------

;; use c++-mode as default for .h header files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; customizations for all c-related modes
(defun my-c-mode-common-hook ()
  ;; make standard indentation offset :=4
  (setq c-basic-offset 4)

  ;; make enums etc indent correctly
  (c-set-offset 'brace-list-intro '+)

  ;; also make argument lists starting on a new line indent correctly
  (c-set-offset 'arglist-intro '+)

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
                                       (".h" ".cuh"))
                                      ("\\.cuh\\'"
                                       (".cu" ".cut"))
                                      ("\\.cut\\'"
                                       (".h" ".cuh"))))
  (setq ff-search-directories
        '("." "../src" "../source" "../../source" "../include" "../include/*"))
  )

;; realize
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-mode-hook ()
  ;; Changes the indentation of substatement parantheses
  (c-set-offset 'substatement-open 0))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(use-package cuda-mode
  :mode "\\.cu[th]?$"
  )

;;-----------------------------------------------------------------------------
;; clang format
;;-----------------------------------------------------------------------------

(use-package clang-format)

;; (add-hook 'c-mode-common-hook
;;           (function (lambda ()
;;                       (add-hook 'before-save-hook
;;                                 'clang-format-buffer))))

;;-----------------------------------------------------------------------------
;; Fortran
;;-----------------------------------------------------------------------------

(add-hook 'f90-mode-hook '(lambda ()
                            (local-unset-key (kbd "C-j"))))

;;-----------------------------------------------------------------------------
;; octave
;;-----------------------------------------------------------------------------

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(defun octave-my-setting ()
  (setq octave-comment-char ?%)
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (if (display-graphic-p)
	  (font-lock-mode 1))
  (setq octave-block-offset 4
        indent-tabs-mode nil))
(add-hook 'octave-mode-hook 'octave-my-setting)

;;-----------------------------------------------------------------------------
;; matlab
;;-----------------------------------------------------------------------------

;; (setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
;; (setq matlab-mode-hook
;;       '(lambda ()
;; ;;       (setq matlab-indent-function t)       ; if you want function bodies
;; ;;                                      ; indented
;; ;;       (setq fill-column 76)         ; where auto-fill should wrap
;; ;;       (turn-on-auto-fill)
;;          (local-unset-key (kbd "M-;"))
;;          (setq matlab-indent-level 4)
;;          (local-set-key (kbd "C-j") 'matlab-return)
;;          ))


;;-----------------------------------------------------------------------------
;; abc-mode
;;-----------------------------------------------------------------------------

(defun abc-show-current-ps()
  (interactive)
  (save-buffer)
  (shell-command
   (read-from-minibuffer
    "Options: "
    (concat "atril" " "
            (replace-regexp-in-string "\.abc$" ".ps" buffer-file-name)
            " &"
            ))))

(use-package abc-mode
  :mode ("\\.abc\\'" "\\.abp\\'")
  :bind (:map abc-mode-map
              ("C-c C-c" . abc-run-abc2ps-all)
              ("C-c C-v" . abc-show-current-ps))
  )



;; (add-to-list 'auto-insert-alist '(abc-mode . abc-skeleton))

;;-----------------------------------------------------------------------------
;; LaTeX
;;-----------------------------------------------------------------------------

(use-package tex
  :ensure auctex
  :config

;; (eval-after-load "latex"
;;   '(progn
;;      (define-key LaTeX-math-keymap "/" 'LaTeX-math-frac)
;;      (define-key LaTeX-math-keymap "2" 'LaTeX-math-sqrt)
;;      (setq ispell-tex-skip-alists
;;            (list
;;             (append
;;              (car ispell-tex-skip-alists) ;tell ispell to ignore content of this:
;;              '(
;;                ("\\[" . "\\]")
;;                ;; ("\\\\verb\\\\|" . "\\\\|")
;;                ("\\\\eqref" ispell-tex-arg-end)
;;                ("\\\\secref" ispell-tex-arg-end)
;;                ("\\\\liref" ispell-tex-arg-end)
;;                ("\\\\fgref" ispell-tex-arg-end)
;;                ("\\\\tbref" ispell-tex-arg-end)
;;                ("\\\\alref" ispell-tex-arg-end)
;;                ;; ("\\\\label" ispell-tex-arg-end)
;;                ;; ("\\\\" ispell-tex-arg-end)
;;                ))
;;             (cadr ispell-tex-skip-alists)))
;;      (add-to-list 'LaTeX-verbatim-environments "comment")
;;      (add-to-list 'TeX-expand-list
;;                   '("%(RubberPDF)"
;;                     (lambda ()
;;                       (if
;;                           (not TeX-PDF-mode)
;;                           ""
;;                         "--pdf"))))
;;      (add-to-list 'TeX-command-list
;;                   '("Rubber" "rubber %(RubberPDF) %t" TeX-run-shell nil t) t)
;;      ))

;; (setq-default TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv")
;;                                                  (output-dvi "xdvi") (output-pdf "xdg-open")
;;                                                  (output-html "xdg-open"))))

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
  )



;;-----------------------------------------------------------------------------
;; use cperl-mode instead of perl-mode, or maybe not...
;;-----------------------------------------------------------------------------

;; (defalias 'perl-mode 'cperl-mode)

;;-----------------------------------------------------------------------------
;; Dockerfile mode
;;-----------------------------------------------------------------------------

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;-----------------------------------------------------------------------------
;; Auto completion, yasnippet,
;; rope/ropemacs/pymacs, etc
;;-----------------------------------------------------------------------------

(use-package yasnippet
  :config

  (yas-global-mode 1)
  ; personal snippets
  (add-to-list #'yas-snippet-dirs "~/.emacs.d/snippets")
  )

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

(use-package arduino-mode
  :mode "\\.\\(pde\\|ino\\)$")

;;-----------------------------------------------------------------------------
;; Templates
;;-----------------------------------------------------------------------------

;; make emacs auto-load templates
; TODO: fails to install from melpa
(use-package template
  :load-path "~/.emacs.d/plugins"
  :config
  (template-initialize)
  )

;;-----------------------------------------------------------------------------
;; PKGBUILD mode
;;-----------------------------------------------------------------------------

(use-package pkgbuild-mode
  :mode "/PKGBUILD$")

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
;; dead keys
;;-----------------------------------------------------------------------------

(require 'iso-transl)

;;-----------------------------------------------------------------------------
;; ctags
;;-----------------------------------------------------------------------------

(setq path-to-ctags "ctags-exuberant") ;; <- your ctags path here

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((d (directory-file-name dir-name)))
    (shell-command
     (format "%s -f %s/TAGS -e -R %s" path-to-ctags d d))
    )
  )

(global-set-key [f8] 'create-tags)

;;-----------------------------------------------------------------------------
;; intel hex mode
;;-----------------------------------------------------------------------------
(use-package intel-hex-mode) ; TODO: defer?

;;-----------------------------------------------------------------------------
;; verilog
;;-----------------------------------------------------------------------------

(setq save-abbrevs nil)
(defun my-verilog-setup ()
    (clear-abbrev-table verilog-mode-abbrev-table))
(add-hook 'verilog-mode-hook #'my-verilog-setup)

;;-----------------------------------------------------------------------------
;; cmake-mode
;;-----------------------------------------------------------------------------

(use-package cmake-mode
  :init
  (setq cmake-tab-width 4)
  :hook
  (cmake-mode . lsp-deferred)
  )

;;-----------------------------------------------------------------------------
;; rust-mode
;;-----------------------------------------------------------------------------

(use-package rust-mode)

;;-----------------------------------------------------------------------------
;; haskell
;;-----------------------------------------------------------------------------

(use-package haskell-mode)

;;-----------------------------------------------------------------------------
;; markdown-mode
;;-----------------------------------------------------------------------------

(use-package markdown-mode)

;;-----------------------------------------------------------------------------
;; csv-mode
;;-----------------------------------------------------------------------------

(use-package csv-mode)

;;-----------------------------------------------------------------------------
;; magit
;;-----------------------------------------------------------------------------

(use-package magit)

;;-----------------------------------------------------------------------------
;; lsp mode
;;-----------------------------------------------------------------------------

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration)
  )

;; Enable LSP for some languages
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'cuda-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

;;-----------------------------------------------------------------------------
;; company mode
;;-----------------------------------------------------------------------------

(use-package company
  :after lsp-mode
  :config
  (global-company-mode t)
  )

;;-----------------------------------------------------------------------------
;; email (mu4e)
;;-----------------------------------------------------------------------------

;; get mail addresses
(setq email-addresses (split-string (with-temp-buffer (insert-file-contents "~/.email_addresses")
                                                             (buffer-string))))
(defun kalj/get-match (key lst)
  (seq-find (lambda (s) (string-match key s)) lst))
(defun kalj/get-email (key)
  (kalj/get-match key email-addresses))

(setq kalj-mail-address (kalj/get-email "kalj"))
(setq hpc-mail-address (kalj/get-email "hpc"))
(setq ubertek-mail-address (kalj/get-email "ubertek"))
(setq gmail-mail-address (kalj/get-email "k.*gmail"))
(setq qgmail-mail-address (kalj/get-email "q.*gmail"))

;; set main mail address
(setq user-mail-address gmail-mail-address)

(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-debug 't
        mu4e-get-mail-command "offlineimap"
        mu4e-view-show-addresses t
        mu4e-compose-format-flowed t
        mu4e-headers-include-related nil
        mu4e-attachment-dir (expand-file-name "~/Downloads/")
        mu4e-html2text-command "w3m -T text/html" ;; alternatively "textutil -stdin -format html -convert txt -stdout"

        mu4e-maildir-shortcuts
        '((:maildir "/main/INBOX"    :key ?m)
          (:maildir "/hpc/INBOX"     :key ?h)
          (:maildir "/ubertek/INBOX" :key ?u)
          (:maildir "/gmail/INBOX"   :key ?g)
          (:maildir "/gmail2/INBOX"  :key ?q))

        mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:mailing-list . 10)
          (:maildir . 20)
          (:from-or-to . 24)
          (:subject))

        mu4e-bookmarks
        '(( :name  "Unread messages (excl. spam & trash)"
                   :query "flag:unread AND NOT ( maildir:/gmail/[Gmail].Spam OR maildir:/gmail2/[Gmail].Spam OR maildir:/hpc/Spam OR maildir:/gmail/[Gmail].Trash OR maildir:/gmail2/[Gmail].Trash OR maildir:/main/INBOX.Trash OR maildir:/hpc/Trash OR maildir:/ubertek/INBOX.Trash )"
                   :key ?u)
          ( :name "All inboxes"
                  :query "maildir:/main/INBOX OR maildir:/ubertek/INBOX OR maildir:/hpc/INBOX OR maildir:/gmail/INBOX OR maildir:/gmail2/INBOX"
                  :key ?i)
          ( :name "Today's messages"
                  :query "date:today..now"
                  :key ?t)
          ( :name "Last 7 days"
                  :query "date:7d..now"
                  :hide-unread t
                  :key ?w)
          ( :name "Messages with images"
                  :query "mime:image/*"
                  :key ?p))

        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'always-ask
        )
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "main"
          :enter-func (lambda () (mu4e-message "Entering context main"))
          :leave-func (lambda () (mu4e-message "Leaving context main"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,kalj-mail-address)))
          :vars `((user-mail-address . ,kalj-mail-address)
                  (mu4e-sent-folder . "/main/INBOX.Sent")
                  (mu4e-drafts-folder . "/main/INBOX.Drafts")
                  (mu4e-trash-folder . "/main/INBOX.Trash")
                  (smtpmail-smtp-user . ,kalj-mail-address)
                  (smtpmail-smtp-server . "mailcluster.loopia.se")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,kalj-mail-address))
                  ))
         (make-mu4e-context
          :name "ubertek"
          :enter-func (lambda () (mu4e-message "Entering context ubertek"))
          :leave-func (lambda () (mu4e-message "Leaving context ubertek"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,ubertek-mail-address)))
          :vars `((user-mail-address . ,ubertek-mail-address)
                  (mu4e-sent-folder . "/ubertek/INBOX.Sent")
                  (mu4e-drafts-folder . "/ubertek/INBOX.Drafts")
                  (mu4e-trash-folder . "/ubertek/INBOX.Trash")
                  (smtpmail-smtp-user . ,ubertek-mail-address)
                  (smtpmail-smtp-server . "mailcluster.loopia.se")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,ubertek-mail-address))
                  ))
         (make-mu4e-context
          :name "hpc"
          :enter-func (lambda () (mu4e-message "Entering context hpc"))
          :leave-func (lambda () (mu4e-message "Leaving context hpc"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) hpc-mail-address)))
          :vars `((user-mail-address . ,hpc-mail-address)
                  (mu4e-sent-folder . "/hpc/Sent")
                  (mu4e-drafts-folder . "/hpc/Drafts")
                  (mu4e-trash-folder . "/hpc/Trash")
                  (smtpmail-smtp-user . ,hpc-mail-address)
                  (smtpmail-smtp-server . "smtp01.binero.se")
                  (smtpmail-stream-type . ssl)
                  (smtpmail-smtp-service . 465)
                  (mu4e-user-mail-address-list . (,hpc-mail-address))
                  ))
         (make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "Entering context gmail"))
          :leave-func (lambda () (mu4e-message "Leaving context gmail"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,gmail-mail-address)))
          :vars `((user-mail-address . ,gmail-mail-address)
                  (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
                  (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
                  (mu4e-trash-folder . "/gmail/[Gmail].Trash")
                  (smtpmail-smtp-user . ,gmail-mail-address)
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,gmail-mail-address))
                  ))
         (make-mu4e-context
          :name "qgmail"
          :enter-func (lambda () (mu4e-message "Entering context qgmail"))
          :leave-func (lambda () (mu4e-message "Leaving context qgmail"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,qgmail-mail-address)))
          :vars `((user-mail-address . ,qgmail-mail-address)
                  (mu4e-sent-folder . "/gmail2/[Gmail].Sent Mail")
                  (mu4e-drafts-folder . "/gmail2/[Gmail].Drafts")
                  (mu4e-trash-folder . "/gmail2/[Gmail].Trash")
                  (smtpmail-smtp-user . ,qgmail-mail-address)
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,qgmail-mail-address))
                  ))
        ))
  (defun mu4e-message-contact-field-matches-me-current (msg cfield)
    (cl-find-if (lambda (cell) (mu4e-current-personal-address-p (cdr cell)))
                (mu4e-message-field msg cfield)))

  (defun mu4e-current-personal-address-p (addr)
    (when addr
      (if
          (string-match "/\\(.*\\)/" user-mail-address)
          (let ((rx (match-string 1 user-mail-address))
                (case-fold-search t))
            (if (string-match rx addr) t nil))
        (eq t (compare-strings addr nil nil user-mail-address nil nil 'case-insensitive)))
      ))

  (defun mu4e~draft-reply-construct-recipients (origmsg)
    "Determine the to/cc recipients for a reply message."
    (let* ((reply-to-self (mu4e-message-contact-field-matches-me-current origmsg :from))
           ;; reply-to-self implies reply-all
           (reply-all (or reply-to-self
                          (eq mu4e-compose-reply-recipients 'all)
                          (and (not (eq mu4e-compose-reply-recipients 'sender))
                               (mu4e~draft-reply-all-p origmsg)))))
      (progn
        (message (format "reply-to-self: %s" reply-to-self))
        (concat
         (if reply-to-self
             ;; When we're replying to ourselves, simply keep the same headers.
             (concat
              (mu4e~draft-header "To" (mu4e~draft-recipients-list-to-string
                                       (mu4e-message-field origmsg :to)))
              (mu4e~draft-header "Cc" (mu4e~draft-recipients-list-to-string
                                       (mu4e-message-field origmsg :cc))))

           ;; if there's no-one in To, copy the CC-list
           (if (zerop (length (mu4e~draft-create-to-lst origmsg)))
               (mu4e~draft-header "To" (mu4e~draft-recipients-construct
                                        :cc origmsg reply-all))
             ;; otherwise...
             (concat
              (mu4e~draft-header "To" (mu4e~draft-recipients-construct :to origmsg))
              (mu4e~draft-header "Cc" (mu4e~draft-recipients-construct :cc origmsg reply-all)))))))))
  )

(use-package smtpmail
  :config
  (setq smtpmail-debug-info t
        smtpmail-warn-about-unknown-extensions t

        message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t)
  )

;; Global keybinding for mu4e
(global-set-key (kbd "C-c m") 'mu4e)

;;-----------------------------------------------------------------------------
;; vi emulation
;;-----------------------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)

  (setq evil-want-fine-undo t)
  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  (evil-define-key '(insert visual) 'global (kbd "C-å") 'evil-force-normal-state)
  (evil-define-key '(normal visual) 'global "q" 'my-comment-dwim)
  (evil-define-key '(normal) 'global (kbd "M-.") 'xref-find-definitions)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

;;-----------------------------------------------------------------------------
;; Custom
;;-----------------------------------------------------------------------------

;; (message "emacs initialized in %s" (emacs-init-time))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets use-package rust-mode markdown-mode flycheck diminish csv-mode pkgbuild-mode evil dockerfile-mode arduino-mode))
 '(safe-local-variable-values '((dont-delete-trailing-whitespace . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
