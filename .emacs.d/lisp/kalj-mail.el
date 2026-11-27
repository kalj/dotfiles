;;; kalj-mail.el --- Email with mu4e

;; Split verbatim from ~/.emacs; loaded from there.

;;-----------------------------------------------------------------------------
;; email (mu4e)
;;-----------------------------------------------------------------------------

;; get mail addresses
(defvar email-addresses (split-string (with-temp-buffer (insert-file-contents "~/.email_addresses")
                                                        (buffer-string))))
(defun kalj/get-match (key lst)
  (seq-find (lambda (s) (string-match key s)) lst))
(defun kalj/get-email (key)
  (kalj/get-match key email-addresses))

(setq kalj-mail-address (kalj/get-email "kalj"))
(setq hpc-mail-address (kalj/get-email "hpc"))
(setq ubertek-mail-address (kalj/get-email "ubertek"))
(setq gmail-mail-address (kalj/get-email "k.*gmail"))
(setq qgmail-mail-address (kalj/get-email "q.*gmail"))

;; set main mail address
(setq user-mail-address gmail-mail-address)

(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-debug 't
        mu4e-get-mail-command "offlineimap"
        mu4e-view-show-addresses t
        mu4e-compose-format-flowed t
        mu4e-headers-include-related nil
        mu4e-attachment-dir (expand-file-name "~/Downloads/")
        mu4e-html2text-command "w3m -T text/html" ;; alternatively "textutil -stdin -format html -convert txt -stdout"

        mu4e-maildir-shortcuts
        '((:maildir "/main/INBOX"    :key ?m)
          (:maildir "/hpc/INBOX"     :key ?h)
          (:maildir "/ubertek/INBOX" :key ?u)
          (:maildir "/gmail/INBOX"   :key ?g)
          (:maildir "/gmail2/INBOX"  :key ?q))

        mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:mailing-list . 10)
          (:maildir . 20)
          (:from-or-to . 24)
          (:subject))

        mu4e-bookmarks
        '(( :name  "Unread messages (excl. spam & trash)"
                   :query "flag:unread AND NOT ( maildir:/gmail/[Gmail].Spam OR maildir:/gmail2/[Gmail].Spam OR maildir:/hpc/Spam OR maildir:/gmail/[Gmail].Trash OR maildir:/gmail2/[Gmail].Trash OR maildir:/main/INBOX.Trash OR maildir:/hpc/Trash OR maildir:/ubertek/INBOX.Trash )"
                   :key ?u)
          ( :name "All inboxes"
                  :query "maildir:/main/INBOX OR maildir:/ubertek/INBOX OR maildir:/hpc/INBOX OR maildir:/gmail/INBOX OR maildir:/gmail2/INBOX"
                  :key ?i)
          ( :name "Today's messages"
                  :query "date:today..now"
                  :key ?t)
          ( :name "Last 7 days"
                  :query "date:7d..now"
                  :hide-unread t
                  :key ?w)
          ( :name "Messages with images"
                  :query "mime:image/*"
                  :key ?p))

        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'always-ask
        )
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "main"
          :enter-func (lambda () (mu4e-message "Entering context main"))
          :leave-func (lambda () (mu4e-message "Leaving context main"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,kalj-mail-address)))
          :vars `((user-mail-address . ,kalj-mail-address)
                  (mu4e-sent-folder . "/main/INBOX.Sent")
                  (mu4e-drafts-folder . "/main/INBOX.Drafts")
                  (mu4e-trash-folder . "/main/INBOX.Trash")
                  (smtpmail-smtp-user . ,kalj-mail-address)
                  (smtpmail-smtp-server . "mailcluster.loopia.se")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,kalj-mail-address))
                  ))
         (make-mu4e-context
          :name "ubertek"
          :enter-func (lambda () (mu4e-message "Entering context ubertek"))
          :leave-func (lambda () (mu4e-message "Leaving context ubertek"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,ubertek-mail-address)))
          :vars `((user-mail-address . ,ubertek-mail-address)
                  (mu4e-sent-folder . "/ubertek/INBOX.Sent")
                  (mu4e-drafts-folder . "/ubertek/INBOX.Drafts")
                  (mu4e-trash-folder . "/ubertek/INBOX.Trash")
                  (smtpmail-smtp-user . ,ubertek-mail-address)
                  (smtpmail-smtp-server . "mailcluster.loopia.se")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,ubertek-mail-address))
                  ))
         (make-mu4e-context
          :name "hpc"
          :enter-func (lambda () (mu4e-message "Entering context hpc"))
          :leave-func (lambda () (mu4e-message "Leaving context hpc"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) hpc-mail-address)))
          :vars `((user-mail-address . ,hpc-mail-address)
                  (mu4e-sent-folder . "/hpc/Sent")
                  (mu4e-drafts-folder . "/hpc/Drafts")
                  (mu4e-trash-folder . "/hpc/Trash")
                  (smtpmail-smtp-user . ,hpc-mail-address)
                  (smtpmail-smtp-server . "smtp01.binero.se")
                  (smtpmail-stream-type . ssl)
                  (smtpmail-smtp-service . 465)
                  (mu4e-user-mail-address-list . (,hpc-mail-address))
                  ))
         (make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "Entering context gmail"))
          :leave-func (lambda () (mu4e-message "Leaving context gmail"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,gmail-mail-address)))
          :vars `((user-mail-address . ,gmail-mail-address)
                  (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
                  (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
                  (mu4e-trash-folder . "/gmail/[Gmail].Trash")
                  (smtpmail-smtp-user . ,gmail-mail-address)
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,gmail-mail-address))
                  ))
         (make-mu4e-context
          :name "qgmail"
          :enter-func (lambda () (mu4e-message "Entering context qgmail"))
          :leave-func (lambda () (mu4e-message "Leaving context qgmail"))
          :match-func `(lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) ,qgmail-mail-address)))
          :vars `((user-mail-address . ,qgmail-mail-address)
                  (mu4e-sent-folder . "/gmail2/[Gmail].Sent Mail")
                  (mu4e-drafts-folder . "/gmail2/[Gmail].Drafts")
                  (mu4e-trash-folder . "/gmail2/[Gmail].Trash")
                  (smtpmail-smtp-user . ,qgmail-mail-address)
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-service . 587)
                  (mu4e-user-mail-address-list . (,qgmail-mail-address))
                  ))
        ))
  (defun mu4e-message-contact-field-matches-me-current (msg cfield)
    (cl-find-if (lambda (cell) (mu4e-current-personal-address-p (cdr cell)))
                (mu4e-message-field msg cfield)))

  (defun mu4e-current-personal-address-p (addr)
    (when addr
      (if
          (string-match "/\\(.*\\)/" user-mail-address)
          (let ((rx (match-string 1 user-mail-address))
                (case-fold-search t))
            (if (string-match rx addr) t nil))
        (eq t (compare-strings addr nil nil user-mail-address nil nil 'case-insensitive)))
      ))

  (defun mu4e~draft-reply-construct-recipients (origmsg)
    "Determine the to/cc recipients for a reply message."
    (let* ((reply-to-self (mu4e-message-contact-field-matches-me-current origmsg :from))
           ;; reply-to-self implies reply-all
           (reply-all (or reply-to-self
                          (eq mu4e-compose-reply-recipients 'all)
                          (and (not (eq mu4e-compose-reply-recipients 'sender))
                               (mu4e~draft-reply-all-p origmsg)))))
      (progn
        (message (format "reply-to-self: %s" reply-to-self))
        (concat
         (if reply-to-self
             ;; When we're replying to ourselves, simply keep the same headers.
             (concat
              (mu4e~draft-header "To" (mu4e~draft-recipients-list-to-string
                                       (mu4e-message-field origmsg :to)))
              (mu4e~draft-header "Cc" (mu4e~draft-recipients-list-to-string
                                       (mu4e-message-field origmsg :cc))))

           ;; if there's no-one in To, copy the CC-list
           (if (zerop (length (mu4e~draft-create-to-lst origmsg)))
               (mu4e~draft-header "To" (mu4e~draft-recipients-construct
                                        :cc origmsg reply-all))
             ;; otherwise...
             (concat
              (mu4e~draft-header "To" (mu4e~draft-recipients-construct :to origmsg))
              (mu4e~draft-header "Cc" (mu4e~draft-recipients-construct :cc origmsg reply-all)))))))))
  )

(use-package smtpmail
  :config
  (setq smtpmail-debug-info t
        smtpmail-warn-about-unknown-extensions t

        message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t)
  )

;; Global keybinding for mu4e
(global-set-key (kbd "C-c m") 'mu4e)

(provide 'kalj-mail)
;;; kalj-mail.el ends here
