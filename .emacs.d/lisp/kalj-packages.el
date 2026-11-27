;;; kalj-packages.el --- package.el and use-package setup

;; Split verbatim from ~/.emacs; loaded from there.

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
