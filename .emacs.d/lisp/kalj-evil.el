;;; kalj-evil.el --- Vi emulation

;; Split verbatim from ~/.emacs; loaded from there.

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

  ;; Make vertical movement work on visual lines
  (evil-define-key '(normal motion) 'global (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (evil-define-key '(normal motion) 'global (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  (evil-define-key '(insert visual) 'global (kbd "C-å") 'evil-force-normal-state)
  (evil-define-key '(normal visual) 'global "q" 'my-comment-dwim)
  (evil-define-key '(normal) 'global (kbd "M-.") 'xref-find-definitions)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode t)
  )

(provide 'kalj-evil)
;;; kalj-evil.el ends here
