;;; kalj-languages.el --- Language-specific modes and settings -*- lexical-binding: t -*-

;; Split verbatim from the old ~/.emacs; loaded from ~/.emacs.d/init.el.

;;-----------------------------------------------------------------------------
;; org-mode
;;-----------------------------------------------------------------------------

;; disabled for now, see git history for setup

;;-----------------------------------------------------------------------------
;; c-mode customizations
;;-----------------------------------------------------------------------------

;; use c++-mode as default for .h header files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'" . c++-mode))

;; make standard indentation offset :=4
(setq-default c-basic-offset my-basic-offset)

;; customizations for all c-related modes
(defun my-c-mode-common-hook ()

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
                                       (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".cu" ".t"))
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
  (setq-default ff-search-directories
                '("." "../src" "../../src" "../source" "../../source" "../include" "../include/*" "../inc" "../inc/*"))
  )

;; realize
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-mode-hook ()
  "Change the indentation of substatement parantheses."
  (c-set-offset 'substatement-open 0))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(use-package cuda-mode
  :mode "\\.cu[th]?$"
  )

;;-----------------------------------------------------------------------------
;; Fortran
;;-----------------------------------------------------------------------------

(add-hook 'f90-mode-hook '(lambda ()
                            (local-unset-key (kbd "C-j"))))

;;-----------------------------------------------------------------------------
;; octave
;;-----------------------------------------------------------------------------

(use-package octave
  :commands octave-mode
  :mode (("\\.m$" . octave-mode))
  :hook (
         (octave-mode . (lambda ()
                          (abbrev-mode 1)
                          (auto-fill-mode 1)
                          (if (display-graphic-p)
                              (font-lock-mode 1))
                          (setq octave-block-offset my-basic-offset
                                indent-tabs-mode nil
                                octave-comment-char ?%)
                          ))
         )
  )

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
;;          (setq matlab-indent-level my-basic-offset)
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
;; shell mode customizations
;;-----------------------------------------------------------------------------

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;;-----------------------------------------------------------------------------
;; css mode customizations
;;-----------------------------------------------------------------------------

;; css mode customizations
(setq-default cssm-indent-function #'cssm-c-style-indenter)

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
;; bb-mode
;;-----------------------------------------------------------------------------

(use-package bb-mode
  :load-path "~/.emacs.d/plugins"
  :mode "\\.\\(bb\\|bbappend\\)$")

;;-----------------------------------------------------------------------------
;; PKGBUILD mode
;;-----------------------------------------------------------------------------

(use-package pkgbuild-mode
  :mode "/PKGBUILD$")

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
  (setq cmake-tab-width my-basic-offset)
  )

;;-----------------------------------------------------------------------------
;; rust-mode
;;-----------------------------------------------------------------------------

(use-package rust-mode)

;;-----------------------------------------------------------------------------
;; haskell
;;-----------------------------------------------------------------------------

(use-package haskell-mode)

(use-package lsp-haskell)

;;-----------------------------------------------------------------------------
;; markdown-mode
;;-----------------------------------------------------------------------------

(use-package markdown-mode)

;;-----------------------------------------------------------------------------
;; csv-mode
;;-----------------------------------------------------------------------------

(use-package csv-mode)

(provide 'kalj-languages)
;;; kalj-languages.el ends here
