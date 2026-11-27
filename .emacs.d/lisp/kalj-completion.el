;;; kalj-completion.el --- which-key, ido, yasnippet, company -*- lexical-binding: t -*-

;; Split verbatim from the old ~/.emacs; loaded from ~/.emacs.d/init.el.

;;-----------------------------------------------------------------------------
;; which-key
;;-----------------------------------------------------------------------------

(use-package which-key
  :demand
  :config
  (add-hook 'after-init-hook 'which-key-mode))

;;-----------------------------------------------------------------------------
;; Use ido to fascilitate buffer switching / file opening
;;-----------------------------------------------------------------------------

(use-package ido
  :diminish ido-mode
  :config
  (setq ido-case-fold t
        ido-use-faces t
        ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-auto-merge-work-directories-length -1)

  ;; (setq ido-use-filename-at-point nil)
  ;; (ido-ubiquitous-mode 1)
  (ido-mode)
  (ido-everywhere)
)

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

(use-package yasnippet-snippets)

;;-----------------------------------------------------------------------------
;; company mode
;;-----------------------------------------------------------------------------

(use-package company
  :after eglot
  :config
  (global-company-mode t)
  )

(provide 'kalj-completion)
;;; kalj-completion.el ends here
