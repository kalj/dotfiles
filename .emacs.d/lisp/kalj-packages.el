;;; kalj-packages.el --- package.el and use-package setup -*- lexical-binding: t -*-

;; Split verbatim from the old ~/.emacs; loaded from ~/.emacs.d/init.el.

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

(provide 'kalj-packages)
;;; kalj-packages.el ends here
