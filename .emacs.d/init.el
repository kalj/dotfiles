; init.el --

;; The configuration lives in ~/.emacs.d/lisp/, one file per topic.
;; They are loaded in this order.
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'kalj-core)
(require 'kalj-packages)
(require 'kalj-keys)
(require 'kalj-utils)
(require 'kalj-completion)
(require 'kalj-prog)
(require 'kalj-languages)
(require 'kalj-mail)
(require 'kalj-evil)

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
