;;; vm-addons.el --- extra stuff for VM

;; Copyright (C) 1994, 95, 96, 97, 98, 99, 00, 04, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: vm-addons.el,v 1.11 2005/10/29 14:30:44 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(require 'vm)
(require 'fmailutils)
(require 'list-fns)
(require 'string-fns)

(defvar vma-envelope-regexp "^From ")

(defvar vma-forward-types
  '(("mime") ("rfc934") ("rfc1153") ("none"))
  "Forwarding/digest types supported by VM.
This variable is used by `vma-read-forward-type'.")

(defvar vma-vm-meta-state-headers
  '("X-VM-Bookmark"
    "X-VM-IMAP-Retrieved"
    "X-VM-Labels"
    "X-VM-Last-Modified"
    "X-VM-Message-Order"
    "X-VM-POP-Retrieved"
    "X-VM-Summary-Format"
    "X-VM-VHeader"
    "X-VM-v5-Data")
  "Headers created by VM for maintaining VM state.
These headers are purged from messages by `vma-discard-cached-data'.")

(defvar vma-folder-font-lock-keywords
  (eval-when-compile
    (let* ((cite-chars "[>|}]")
	   (cite-prefix "[:alpha:]")
	   (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
      `(("^\\(from\\|resent-from\\):\\s-+\\(.*\\)"
         (2 font-lock-function-name-face))
        ;("^\\(to\\|newsgroups\\):\\s-+\\(.*\\)" (2 font-lock-builtin-face))
        ;("^\\(b?cc\\|reply-to\\):\\s-+\\(.*\\)" (2 font-lock-keyword-face))
        ("^\\(subject:\\)[ \t]*\\(.+\\)?"
         ;(1 font-lock-comment-face)
         (2 font-lock-type-face nil t))
        ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
        (,cite-chars
          (,(concat "\\=[ \t]*"
                    "\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
                    "\\(" cite-chars "[ \t]*\\)\\)+"
                    "\\(.*\\)")
           (beginning-of-line) (end-of-line)
           (2 font-lock-constant-face nil t)
           (4 font-lock-string-face nil t)))
        ;("^\\(x-[a-z0-9-]+\\|in-reply-to\\):.*" . font-lock-string-face)
        ))))

;; Used by vma-do-reply-list.
(defvar vma-do-reply-list-called nil)


;;;###autoload
(defun vma-address-list->regexp (list)
  (mapconcat 'identity (mapcar (lambda (s)
                                 (concat "^" (glob->regexp s) "$"))
                               list)
             "\\|"))

;;;###autoload
(defun vma-afa-match-format (format-str &rest match-values)
  "Creates a string of the format FORMAT-STR by getting
buffer-substrings determined by the &rest values MATCH-VALUES.  This
function is meant to be used when setting vm-auto-folder-alist.  It
allows you to construct filenames from saved regexp patterns.

Example:
   (setq vm-auto-folder-alist
         '(\"To\"
           (\"info-\\\\([^@%]*\\\\)\" . (vma-afa-match-format \"info-%s\" 1))
           (\"foo-\\\\(bar\\\\)@\\\\(baz\\\\)\" . (vma-afa-match-format \"foo%s@%s\" 1 2))))

Thus, a To: line addressed to info-gnu@prep.ai.mit.edu would be saved by
default in a folder called \"info-gnu\", and mail addressed to
foo-bar@baz would go in a folder called \"foobar@baz\"."
  (apply 'format format-str
         (mapcar (lambda (match-value)
                   (buffer-substring (match-beginning match-value)
                                     (match-end match-value)))
                 match-values)))

;;;###autoload
(defun vma-discard-cached-data ()
  "Like `vm-discard-cached-data', but more aggressive.
This function actually removes the cache from the folder so that the effect
is more permanent.  This also affects all messages in the folder."
  (interactive)
  (and (eq major-mode 'vm-mode)
       (let* ((current (memq (car vm-message-pointer) vm-message-list))
              (all (length vm-message-list))
              (pre (- (length current) all))
              (post (length current)))
         ;; Try to flush vm's "in-memory" cache?
         (vm-discard-cached-data pre)
         (vm-discard-cached-data post)))
  (let ((mail-header-separator "")
        (headers nil)
        (case-fold-search t)
        (buffer-read-only nil))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward vma-envelope-regexp nil t)
          (narrow-to-region (point) (point-max))
          (setq headers vma-vm-meta-state-headers)
          (while headers
            (fmailutils-remove-header (car headers) t)
            (setq headers (cdr headers))))))))

;;;###autoload
(defun vma-fixup-html-paragraphs ()
  (interactive)
  (vm-edit-message)
  (goto-char (point-min))
  (re-search-forward "^$")
  (replace-regexp "<br>\n<br>" "<p>\n")
  (goto-char (point-min))
  (re-search-forward "^$")
  (replace-regexp "^<br>" "")
  (vm-edit-message-end))

;;;###autoload
(defun vma-folder-font-lock-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(vma-folder-font-lock-keywords t))
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (font-lock-mode 1))

;;;###autoload
(defun vma-globlist->regexp (list)
  "Convert a list of strings with shell globs into a single regexp."
  (mapconcat 'glob->regexp list "\\|"))


;;; Virtual selectors

(defun vm-vs-sent-between (m dates)
  (if (stringp dates)
      (setq dates (string-split dates nil 2)))
  (not (or (vm-vs-sent-before m (nth 0 dates))
           (vm-vs-sent-after m (nth 1 dates)))))

(set-alist-slot 'vm-virtual-selector-function-alist
                'sent-between 'vm-vs-sent-between)
(set-alist-slot 'vm-supported-interactive-virtual-selectors
                "sent-between" nil)
(put 'sent-between 'vm-virtual-selector-clause
     "between dates YYYYMMDD and YYYYMMDD (exclusive range)")
(put 'sent-between 'vm-virtual-selector-arg-type 'string)



;;;###autoload
(defun vma-kill-duplicate-messages-by-message-id (&optional mp)
  "Mark as deleted any message with a duplicate message ID.
Any undeleted message which has a Message ID indentical to that of another
undeleted message in the same folder, is marked for deletion."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((htbl (make-vector 103 0))
        (n 0)
        (case-fold-search t)
        mid)
    (unless mp
      (setq mp vm-message-list))
    (while mp
      (cond ((vm-deleted-flag (car mp)))
            (t
             (setq mid (vm-su-message-id (car mp)))
             (or mid (debug (car mp)))
             (when (intern-soft mid htbl)
               (vm-set-deleted-flag (car mp) t)
               (setq n (1+ n)))
             (intern mid htbl)))
      (setq mp (cdr mp)))
    (and (interactive-p)
         (message "%d duplicate%s marked deleted" n (if (= n 1) "" "s")))
    (vm-update-summary-and-mode-line)
    (when vm-move-after-killing
      (let ((vm-circular-folders (and vm-circular-folders
                                      (eq vm-move-after-killing t))))
        (vm-next-message 1 t executing-kbd-macro)))
    n))

;;; See also `vma-kill-messages-by-descriptor' below for alternative,
;;; virtual-folder subject-based methods.

;; Adapted from VM 6.72 vm-kill-subject
;;;###autoload
(defun vma-kill-subject-by (type data &optional interactivep)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((mp vm-message-list)
	(n 0)
	(case-fold-search t)
        (matcher (if (eq type 'regexp) 'string-match 'string-equal)))
    (save-match-data
      (while mp
        (and (not (vm-deleted-flag (car mp)))
             (funcall matcher data (vm-so-sortable-subject (car mp)))
             (progn
               (vm-set-deleted-flag (car mp) t)
               (vm-increment n)))
        (setq mp (cdr mp))))
    (and interactivep
	 (if (zerop n)
	     (message "No messages deleted.")
	   (message "%d message%s deleted" n (if (= n 1) "" "s"))))
    (vm-display nil nil '(vm-kill-subject) '(vm-kill-subject))
    (vm-update-summary-and-mode-line)
    (if vm-move-after-killing
        (let ((vm-circular-folders (and vm-circular-folders
                                        (eq vm-move-after-killing t))))
          (vm-next-message 1 t executing-kbd-macro)))
    n))

;;;###autoload
(defun vma-kill-subject-by-regexp (re &optional interactivep)
  (interactive "sKill subject regexp: ")
  (vma-kill-subject-by 'regexp re (interactive-p)))

;;;###autoload
(defun vma-kill-subject-matching-region (beg end &optional interactivep)
  (interactive "r")
  (let ((s (buffer-substring beg end)))
    (vma-kill-subject-by 'literal s (interactive-p))))


;;;###autoload
(defvar vma-kill-message-descriptor-alist nil
  "*Default descriptor alist used by `vma-kill-messages-by-descriptor'.")

;;;###autoload
(defun vma-kill-messages-by-descriptor (&optional name virtual-folder-alist)
  "Mark messages in virtual folder as deleted.
Messages targeted for deletion are those of virtual folder named NAME in
VIRTUAL-FOLDER-ALIST.

If NAME is not specified, it defaults to \"vma-kill-messages-by-descriptor\".
If VIRTUAL-FOLDER-ALIST is not specified, it defaults to the value of
`vma-kill-messages-by-descriptor-alist' if non-nil; otherwise it defaults
to the value of `vm-virtual-folder-alist'.

For example:

    \(setq vma-kill-message-descriptor-alist
          '\(\(\"vma-kill-messages-by-descriptor\"
             \(\(buffer-file-name\)
              \(and \(author  \"spammer\"\)
                   \(subject \"annoying topic\"\)\)
              \(author  \"^\\\(mailer-daemon\\|postmaster\\\)@\"\)
              \(subject \"^Postmaster notify: User unknown$\"\)\)\)\)\)"
  (interactive)
  (or name (setq name "vma-kill-messages-by-descriptor"))
  (or virtual-folder-alist
      (setq virtual-folder-alist
            (or vma-kill-message-descriptor-alist
                vm-virtual-folder-alist)))

  (and name
       virtual-folder-alist
       (assoc name virtual-folder-alist)
       (let ((case-fold-search t)
             (n 0)
             (s nil)
             (last-command)
             (old-marks (vm-marked-messages))
             (vm-virtual-folder-alist virtual-folder-alist))
         (save-match-data
           (unwind-protect
               (progn
                 (vm-clear-all-marks)
                 (setq s (vm-mark-matching-messages-with-virtual-folder name))
                 (and (string-match "^\\([0-9]+\\) messages? marked$" s)
                      (setq n (string-to-int (match-string 1 s))))
                 (setq last-command 'vm-next-command-uses-marks)
                 (vm-delete-message 1))
             (vm-clear-all-marks)
             (vma-restore-marks old-marks)))
         (and (interactive-p)
              (if (zerop n)
                  (message "No messages deleted.")
                (message "%d message%s deleted" n (if (= n 1) "" "s"))))
         n)))

(defun vma-restore-marks (marks)
  "Mark messages indicated in MARKS.
MARKS should be the result from a previous call to `vm-marked-messages'."
  (let ((mp vm-message-list))
    (while mp
      (cond ((memq (car mp) marks)
             (vm-set-mark-of (car mp) t)
             (vm-mark-for-summary-update (car mp) t)))
      (setq mp (cdr mp))))
  (vm-update-summary-and-mode-line))

;; This should not necessarily be enabled by default.
;(add-hook 'vm-arrived-messages-hook 'vma-kill-messages-by-descriptor)


;;;###autoload
(defun vma-kill-vm-summary-buffer ()
  "This is useful to put on `kill-buffer-hook' in vm-mode."
  (and (boundp 'vm-summary-buffer)
       vm-summary-buffer
       (get-buffer vm-summary-buffer)
       (buffer-name vm-summary-buffer)
       (kill-buffer vm-summary-buffer)))

;;;###autoload
(defun vma-mail-yank-default ()
  "Like `vm-mail-yank-default', but compute the `message' arg.
Use `vm-reply-list' or `vm-forward-list', whichever is active for this
buffer.  This function optionally goes on `mail-citation-hook' so
that the default quoted message preparation is done even if other
functions are on that hook.  Ordinarily `vm-mail-yank-default' is only run
by default if `mail-citation-hook' is nil."
  (vm-mail-yank-default (car (or vm-reply-list vm-forward-list))))


;;;###autoload
(defun vma-mime-add-final-boundary ()
  "Edit message to include final mime boundary if missing."
  (interactive)
  (save-excursion
    (save-match-data
      (let* ((case-fold-search t)
             (mail-header-separator "")
             (region-beg (vma-widen-message-headers))
             (hdr (car (fmailutils-get-header-contents "Content-Type")))
             (boundary (and (string-match "boundary=\"\\([^\"]+\\)\"" hdr)
                            (matching-substring 1 hdr)))
             (boundary-re (concat "^--" (regexp-quote boundary) "--")))
        (narrow-to-region region-beg (point-max))
        (or boundary
            (error "Cannot determine mime boundary"))
        (goto-char (point-max))
        (or (re-search-backward boundary-re nil t)
            (let ((vm-edit-message-mode 'fundamental-mode)
                  (mail-header-separator ""))
              ;; Let me know why the message has been labeled as edited
              (fmailutils-put-header "X-vma-mime-add-final-boundary"
                "Added missing final mime boundary separator")
              (vm-edit-message)
              (goto-char (point-max))
              (insert "\n--" boundary "--\n")
              (vm-edit-message-end)))))))

;;;###autoload
(defun vma-mime-send-body-to-file (&optional filename)
  (interactive)
  (let ((layout nil))
    (cond ((or (and (boundp 'vm-fsfemacs-p) vm-fsfemacs-p) ; vm 6.35+
               (and (boundp 'vm-fsfemacs-19-p) vm-fsfemacs-19-p)) ; vm 6.34-
           (let ((ovls (overlays-at (point))))
             (while ovls
               (cond ((vm-extent-property (car ovls) 'vm-mime-layout)
                      (setq layout (car ovls))
                      (setq ovls nil))
                     (t
                      (setq ovls (cdr ovls)))))))
          (vm-xemacs-p
           (setq layout (extent-at (point) nil 'vm-mime-layout))))
    (if layout
        (vm-mime-send-body-to-file layout filename)
      (error "Cannot find any VM MIME layout properties at point."))))

;;;###autoload
(defun vm-mime-display-internal-application/x-pkcs7-signature (layout)
  t)

;;;###autoload
(defun vm-mime-display-button-application/x-pkcs7-signature (layout)
  (vm-mime-display-button-xxxx layout t))

(and (boundp 'vm-auto-displayed-mime-content-types)
     (add-list-members 'vm-auto-displayed-mime-content-types
       "application/x-pkcs7-signature"))


;;;###autoload
(defun vma-nuke-message-body-text-properties ()
  (require 'buffer-fns)
  (nuke-all-text-properies (+ (fmailutils-header-separator-position)
                              (length mail-header-separator))
                           (point-max)))

;;;###autoload
(defun vma-peek ()
  "Peek at readable spool files in VM mode without using inboxes."
  (interactive)
  (let ((spools vm-spool-files)
        (nbuf (generate-new-buffer "virtual inbox"))
        f)
    (pop-to-buffer nbuf)
    (while spools
      (setq f (car spools))
      (setq spools (cdr spools))
      (and (consp f)
           (setq f (nth 1 f)))
      (and (file-readable-p f)
           (insert-file-contents f))))
  (vm-mode)
  (make-local-variable 'vm-confirm-quit)
  (setq vm-confirm-quit nil))


;;;###autoload
(defun vma-reply-list (count)
  "Reply to only the recipient (\"To:\" addresses) of the message.
In all other respects, this command behaves just like \\[vm-reply] \(which
see\)."
  (interactive "p")
  (vma-do-reply-list nil count))

;;;###autoload
(defun vma-reply-list-include-text (count)
  "Reply to only the recipient (\"To:\" addresses) of the message.
In all other respects, this command behaves just like
\\[vm-reply-include-text] (which see)."
  (interactive "p")
  (vma-do-reply-list t count))

;; Putting a wrapper around vm-do-reply is "easier"
;; to install than applying a patch to the source...
(defadvice vm-get-header-contents (before vm-addons:do-reply-list activate)
  (when (and (boundp 'vma-do-reply-list-called)
             (eq vma-do-reply-list-called t)
             (string= (ad-get-arg 1) "Reply-To:"))
    (ad-set-arg 1 "To:")))

(defun vma-do-reply-list (include-text count)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((vm-reply-ignored-reply-tos nil)
        (vma-do-reply-list-called t))
    (vm-do-reply nil include-text count)))

;; This choice of bindings predicated on the theory that including text is
;; more common than not.  Lowercase `l' is already bound to something
;; actually useful.
;(define-key vm-mode-map "L"     'vma-reply-list-include-text)
;(define-key vm-mode-map "\C-cl" 'vma-reply-list)


;;;###autoload
(defun vma-read-forward-type (&optional prompt)
  (or prompt (setq prompt "Forward using digest type"))
  (setq prompt (format "%s (default %s): " prompt vm-forwarding-digest-type))
  (let ((result (completing-read prompt vma-forward-types)))
    (cond ((or (null result) (string= "" result))
           vm-forwarding-digest-type)
          ((string= result "none") nil)
          (t result))))

;;;###autoload
(defun vma-send-digest-all-headers (&optional prefix digest-type)
  "Like `vm-send-digest', but retain all headers.
With prefix arg, query for digest type \(one of `vma-forward-types'\)."
  (interactive (list current-prefix-arg
                     ;; Don't let self-insert commands (via keyboard input)
                     ;; override setting vm-next-command-uses-marks.
                     (let ((last-command last-command))
                       (vma-read-forward-type "Digest format"))))
  (let ((vm-rfc934-digest-headers '(".*"))
        (vm-rfc1153-digest-headers '(".*"))
        (vm-mime-digest-headers '(".*"))
        (vm-digest-send-type digest-type))
    (cond ((interactive-p)
           ;; Allow using marks with this wrapper command and propagate to
           ;; vm-send-digest.
           (and (eq last-command 'vm-next-command-uses-marks)
                (setq this-command last-command))
           (call-interactively 'vm-send-digest))
          (t
           (vm-send-digest prefix)))))

;;;###autoload
(defun vma-set-spool-file (inbox spoolnames &optional crashbox)
  "For INBOX, record associated SPOOLNAMES for VM.
When \\[vm-get-new-mail] is invoked for a specified inbox, VM will
incorporate all the mail in each of the spool files.

The optional third argument CRASHBOX specifies the name of the temporary
two-stage commit folder to which to copy incoming mail; see variable
`vm-spool-files'.

SPOOLNAMES may be a single string if there is only one spool file to
associate with the INBOX.

Previous associations for the specified INBOX which are not listed in
SPOOLNAMES are not deleted, i.e. new spoolnames are appended.  However, if
a specified spool name is already associated with a different inbox, that
previous association is deleted from the other inbox.  It is possible to
remove spoolnames entirely by passing `nil' as the inbox name."
  (and inbox
       (setq inbox (expand-file-name inbox)))
  (and (stringp spoolnames)
       (setq spoolnames (list spoolnames)))
  (if crashbox
      (setq crashbox (expand-file-name crashbox))
    (setq crashbox (concat inbox ".CRASH")))

  (let ((record-alist)
        (new-alist nil))
    (while spoolnames
      (setq record-alist (car (member-by (car spoolnames) vm-spool-files
                                         (lambda (elt obj)
                                           (string= elt (nth 1 obj))))))
      (cond
       ((and record-alist
             (stringp (car record-alist))
             (string= (car record-alist) inbox)))
       (t
        (and record-alist
             (setq vm-spool-files (delq record-alist vm-spool-files)))
        (and inbox
             (setq new-alist
                   (cons (list inbox (car spoolnames) crashbox) new-alist)))))
      (setq spoolnames (cdr spoolnames)))
    (and new-alist
         (setq vm-spool-files (append vm-spool-files (nreverse new-alist)))))
  vm-spool-files)

;;;###autoload
(defun vma-widen-message-headers ()
  "Expose all headers of the current message.
Return a marker pointing to the original start of the narrowed region."
  (let ((beg (point-min))
        (end (point-max))
        (re-header-start "^From "))
    (goto-char (point-min))
    (widen)
    ;; In VM, if the headers are already toggled show-all, then the
    ;; envelope address is also already in view (at point-min), so
    ;; first scan forward to the end of the message headers, then go back.
    (re-search-forward "^$" nil t)
    (cond ((re-search-backward re-header-start nil t)
           (end-of-line)
           (forward-char 1)
           (narrow-to-region (point) end)
           (set-marker (make-marker) beg))
          (t
           ;; Couldn't find start of messages; restore original region and
           ;; return nil to indicate no changes.
           (narrow-to-region beg end)
           nil))))

(provide 'vm-addons)

;;; vm-addons.el ends here.
