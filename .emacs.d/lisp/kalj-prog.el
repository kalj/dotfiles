;;; kalj-prog.el --- Compile, formatting, ediff, git, LSP, projects

;; Split verbatim from the old ~/.emacs; loaded from ~/.emacs.d/init.el.

;;-------------------------------------------------------------------
;; compile code using f12 and f11
;;-------------------------------------------------------------------

(defun my-compile (dir)
    (interactive "DIn directory ")
  (let ((default-directory dir))
    (call-interactively #'compile)))

(use-package compile
  :bind (( "C-c K" . my-compile)
         ( "C-c k" . recompile))
  :config
  ; skip anything but errors
  (setq compilation-skip-threshold 2)

  (setq compilation-scroll-output t)

  (setq compile-command "ninja -k0 ")

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
;; clang format
;;-----------------------------------------------------------------------------

(use-package clang-format)

;; (add-hook 'c-mode-common-hook
;;           (function (lambda ()
;;                       (add-hook 'before-save-hook
;;                                 'clang-format-buffer))))
(global-set-key (kbd "C-c f") 'clang-format)

;;-----------------------------------------------------------------------------
;; Ediff
;;-----------------------------------------------------------------------------

(use-package ediff
  :config
    ;; Disable the ediff popup and integrate it instead (Xmonad doesn't handle this very well).
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    ;; Split horizontally by default
    (setq ediff-split-window-function 'split-window-horizontally)
    )

;;-----------------------------------------------------------------------------
;; Tramp mode
;;-----------------------------------------------------------------------------

;; (require 'tramp)
;; (add-to-list 'tramp-default-proxies-alist
             ;; '("localhost.localdomain" "\\`root\\'" "/sudo:%h:")
             ;; '(nil "\\`root\\'" "/ssh:%h:"))

;;-----------------------------------------------------------------------------
;; magit
;;-----------------------------------------------------------------------------

(use-package magit
  :bind (:map magit-file-section-map
              ("RET" . magit-diff-visit-file-other-window)
              :map magit-hunk-section-map
              ("RET" . magit-diff-visit-file-other-window))
  )

;;-----------------------------------------------------------------------------
;; flycheck
;;-----------------------------------------------------------------------------

(use-package flycheck
  :init (global-flycheck-mode))

;;-----------------------------------------------------------------------------
;; lsp mode
;;-----------------------------------------------------------------------------

;; (setq-default lsp-rust-server "rust-analyzer")
;; (setq-default lsp-enable-indentation nil)

;; (use-package lsp-mode
;;   :commands (lsp-mode lsp lsp-deferred)
;;   :init
;;   :hook (
;;          (c++-mode . lsp)
;;          (c-mode . lsp)
;;          ;; (cuda-mode . lsp)
;;          (rust-mode . lsp)
;;          (python-mode . lsp)
;;          (haskell-mode . lsp)
;;          (cmake-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          ;; (lsp-mode . (lambda ()
;;          ;;               (let ((lsp-keymap-prefix "C-c l"))
;;          ;;                (lsp-enable-which-key-integration))))
;;          )
;;   :config
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
;;   )

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(cuda-mode "clangd")
               )
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l q" . eglot-code-action-quickfix)
              ("C-c l w" . eglot-code-action-rewrite)
              ("C-c l i" . eglot-code-action-inline)
              ("C-c l x" . eglot-code-action-extract)
              )
  :hook (
         (c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (cuda-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         )
)

;;-----------------------------------------------------------------------------
;; projectile
;;-----------------------------------------------------------------------------

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1)
  )

;;-----------------------------------------------------------------------------
;; python formatting using apheleia + black
;;-----------------------------------------------------------------------------

(use-package apheleia
  )

(add-hook 'python-mode-hook #'apheleia-mode)

;;-----------------------------------------------------------------------------
;; writable grep buffers
;;-----------------------------------------------------------------------------

(use-package wgrep :ensure t)

(provide 'kalj-prog)
;;; kalj-prog.el ends here
