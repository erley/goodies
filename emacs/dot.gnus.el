;; -----------------------------------------------------------
;; Customize news server connection
(setq user-mail-address "erleya@gmail.com")
(setq user-full-name "Alex Erley")
(setq smtpmail-default-smtp-server "erley.net")

(setq gnus-select-method '(nntp "news.free.fr"))
;(setq gnus-select-method '(nntp "f75.n5004.z2.fidonet.net"))
;(setq gnus-select-method '(nnimap "erley.homeip.net"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))
;(add-to-list 'gnus-secondary-select-methods '(nnrss "http://sachachua.com/blog/feed"))

;; -----------------------------------------------------------
;; Define newsreader layout:
;;
;;        | topics
;; groups |---------
;;        | article
;;
(gnus-add-configuration
 '(article
   (horizontal 1.0
			   (vertical 40
						 (group 1.0))
			   (vertical 1.0
						 (summary 0.35 point)
						 (article 1.0))))
 )
(gnus-add-configuration
 '(summary
   (horizontal 1.0
			   (vertical 40
						 (group 1.0))
			   (vertical 1.0
						 (summary 1.0 point))))
 )

;; -----------------------------------------------------------
;; Customize topic list - item contents, threading...
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f %* %B%s%)\n"
 gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
;─
 gnus-sum-thread-tree-false-root ""
; gnus-sum-thread-tree-false-root "◯ "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-indent "   "
; gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-root "● "
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│  "
; gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-single-indent "◎ "

 line-spacing 0
)

;(setq-default
; gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f %* %B%s%)\n"
; gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
; gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
; gnus-thread-sort-functions '(gnus-thread-sort-by-date)
; gnus-sum-thread-tree-false-root "◯ "
; gnus-sum-thread-tree-indent "  "
; gnus-sum-thread-tree-leaf-with-other "├─► "
; gnus-sum-thread-tree-root "● "
; gnus-sum-thread-tree-single-leaf "╰─► "
; gnus-sum-thread-tree-vertical "│"
; gnus-sum-thread-tree-single-indent "◎ "
;)
