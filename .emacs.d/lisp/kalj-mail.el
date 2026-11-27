;;; kalj-mail.el --- Email with mu4e -*- lexical-binding: t -*-

;; Split verbatim from the old ~/.emacs; loaded from ~/.emacs.d/init.el.

;;-----------------------------------------------------------------------------
;; email (mu4e)
;;-----------------------------------------------------------------------------

(require 'cl-lib)

;; get mail addresses
(defvar email-addresses (split-string (with-temp-buffer (insert-file-contents "~/.email_addresses")
                                                        (buffer-string))))
(defun kalj/get-match (key lst)
  (seq-find (lambda (s) (string-match key s)) lst))
(defun kalj/get-email (key)
  (kalj/get-match key email-addresses))

(defvar kalj-mail-address (kalj/get-email "kalj"))
(defvar hpc-mail-address (kalj/get-email "hpc"))
(defvar ubertek-mail-address (kalj/get-email "ubertek"))
(defvar gmail-mail-address (kalj/get-email "k.*gmail"))
(defvar qgmail-mail-address (kalj/get-email "q.*gmail"))

;; set main mail address
(setq user-mail-address gmail-mail-address)

(defvar kalj/mail-inboxes
  '("/main/INBOX" "/ubertek/INBOX" "/hpc/INBOX" "/gmail/INBOX" "/gmail2/INBOX")
  "Inboxes shown by the \"All inboxes\" bookmark.")

(defvar kalj/mail-spam-and-trash
  '("/gmail/[Gmail].Spam" "/gmail2/[Gmail].Spam" "/hpc/Spam"
    "/gmail/[Gmail].Trash" "/gmail2/[Gmail].Trash"
    "/main/INBOX.Trash" "/hpc/Trash" "/ubertek/INBOX.Trash")
  "Folders left out of the \"Unread messages\" bookmark.")

(defun kalj/maildir-query (maildirs)
  "Return a mu query matching any of MAILDIRS."
  (mapconcat (lambda (m) (concat "maildir:" m)) maildirs " OR "))

(cl-defun kalj/mu4e-make-context (name address &key sent drafts trash smtp-server
                                       (smtp-stream 'starttls) (smtp-port 587))
  "Make a mu4e context NAME for ADDRESS.
SENT, DRAFTS and TRASH are maildir paths.  SMTP-SERVER, SMTP-STREAM
and SMTP-PORT describe the server used to send mail."
  (make-mu4e-context
   :name name
   :enter-func (lambda () (mu4e-message (concat "Entering context " name)))
   :leave-func (lambda () (mu4e-message (concat "Leaving context " name)))
   :match-func (lambda (msg)
                 (when msg
                   (mu4e-message-contact-field-matches
                    msg '(:from :to :cc :bcc) address)))
   :vars `((user-mail-address . ,address)
           (mu4e-sent-folder . ,sent)
           (mu4e-drafts-folder . ,drafts)
           (mu4e-trash-folder . ,trash)
           (smtpmail-smtp-user . ,address)
           (smtpmail-smtp-server . ,smtp-server)
           (smtpmail-stream-type . ,smtp-stream)
           (smtpmail-smtp-service . ,smtp-port)
           (mu4e-user-mail-address-list . (,address)))))

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
        `(( :name  "Unread messages (excl. spam & trash)"
                   :query ,(concat "flag:unread AND NOT ( "
                                   (kalj/maildir-query kalj/mail-spam-and-trash)
                                   " )")
                   :key ?u)
          ( :name "All inboxes"
                  :query ,(kalj/maildir-query kalj/mail-inboxes)
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
         (kalj/mu4e-make-context "main" kalj-mail-address
                                 :sent "/main/INBOX.Sent"
                                 :drafts "/main/INBOX.Drafts"
                                 :trash "/main/INBOX.Trash"
                                 :smtp-server "mailcluster.loopia.se")
         (kalj/mu4e-make-context "ubertek" ubertek-mail-address
                                 :sent "/ubertek/INBOX.Sent"
                                 :drafts "/ubertek/INBOX.Drafts"
                                 :trash "/ubertek/INBOX.Trash"
                                 :smtp-server "mailcluster.loopia.se")
         (kalj/mu4e-make-context "hpc" hpc-mail-address
                                 :sent "/hpc/Sent"
                                 :drafts "/hpc/Drafts"
                                 :trash "/hpc/Trash"
                                 :smtp-server "smtp01.binero.se"
                                 :smtp-stream 'ssl
                                 :smtp-port 465)
         (kalj/mu4e-make-context "gmail" gmail-mail-address
                                 :sent "/gmail/[Gmail].Sent Mail"
                                 :drafts "/gmail/[Gmail].Drafts"
                                 :trash "/gmail/[Gmail].Trash"
                                 :smtp-server "smtp.gmail.com")
         (kalj/mu4e-make-context "qgmail" qgmail-mail-address
                                 :sent "/gmail2/[Gmail].Sent Mail"
                                 :drafts "/gmail2/[Gmail].Drafts"
                                 :trash "/gmail2/[Gmail].Trash"
                                 :smtp-server "smtp.gmail.com")))
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
