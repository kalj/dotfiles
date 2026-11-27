;;; kalj-utils.el --- Small interactive commands

;; Split verbatim from the old ~/.emacs; loaded from ~/.emacs.d/init.el.

;;-----------------------------------------------------------------------------
;; Command for reloading this file
;;-----------------------------------------------------------------------------

(defun reload-init-file ()
  "Reload Emacs init file (~/.emacs.d/init.el)."
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

(defun edit-init-file ()
  "Open Emacs init file for editing (~/.emacs.d/init.el)."
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

;; bind to keys
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "C-c e") 'edit-init-file)

;;-------------------------------------------------------------------
;; function for converting from camelcase to snake case
;;-------------------------------------------------------------------

(defun to-snake-case (start end)
  "Change selected text (between START and END) to snake case format."
  (interactive "r")
  (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
    (message "No region selected")))

;;-----------------------------------------------------------------------------
;; ediff command-line switch
;;-----------------------------------------------------------------------------

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;;-----------------------------------------------------------------------------
;; ctags
;;-----------------------------------------------------------------------------

(defvar path-to-ctags "ctags")

(defun create-tags (dir-name)
  "Create tags file in directory DIR-NAME."
  (interactive "DDirectory: ")
  (let ((d (directory-file-name dir-name)))
    (shell-command
     (format "%s -f %s/TAGS -e -R %s" path-to-ctags d d))
    )
  )

(global-set-key (kbd "C-c t") 'create-tags)

;;-------------------------------------------------------------------
;; ANSI colors
;;-------------------------------------------------------------------

(use-package ansi-color)

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(provide 'kalj-utils)
;;; kalj-utils.el ends here
