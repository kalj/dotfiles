;;; kalj-keys.el --- Global keybindings and font size

;; Split verbatim from ~/.emacs; loaded from there.

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

(provide 'kalj-keys)
;;; kalj-keys.el ends here
