
(setq-default viper-inhibit-startup-message t
	      viper-expert-level 5
	      viper-ESC-moves-cursor-back nil
	      viper-want-ctl-h-help t
	      viper-ex-style-editing nil
              viper-ex-style-motion  nil)

(when (boundp 'viper-insert-global-user-map)
  (define-key viper-insert-global-user-map (kbd "C-w") 'kill-region)
  (define-key viper-insert-global-user-map (kbd "C-d") 'delete-char)
  (define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify))

(when (boundp 'viper-vi-global-user-map)
  (define-key viper-vi-global-user-map (kbd "u") 'undo)
  (define-key viper-vi-global-user-map (kbd "q") 'fill-paragraph)
  (define-key viper-vi-global-user-map (kbd "C-e") 'move-end-of-line)
  (define-key viper-vi-global-user-map (kbd "C-d") 'delete-char)
  (define-key viper-vi-global-user-map (kbd "C-y") 'yank))
