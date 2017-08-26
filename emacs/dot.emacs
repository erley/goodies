;; =====================================================================
;; ========         Multi-platform Emacs configuration          ========
;; =====================================================================
;; Supported environments:
;;   - MacOS X: Emacs, console Emacs
;;   - Windows: Emacs, console Emacs
;;   - FreeBSD: Emacs, console Emacs
;;   - Linux:   Emacs, console Emacs
;; =====================================================================

;; =====================================================================
;; ======================= BEGIN BASE CONFIGURATION ====================
;; =====================================================================

;; ---------------------------------------------------------------------
;; Global settings
;; ---------------------------------------------------------------------
;; ---- Paths:
(setq
  ;; ... init file to store settings set by customize-emacs
  custom-file "~/.emacs.d/custom.el"
  ;; ... extra lisp modules not included into Emacs distribution
  load-path (cons "~/.emacs.d/extra" load-path)
  ;; ... desktop files
  desktop-path '("~/.emacs.d")
)

;; -------------------------------------------------------------------
;; Rebind C-z to start a shell (use .emacs_shellname for the shells rc file)

;==={{
;; (global-set-key "\C-z" 'ansi-term)

;; ;; term
;; (defface term-color-black   '((t (:foreground "#3f3f3f" :background "#272822"))) "Unhelpful docstring.")
;; (defface term-color-red     '((t (:foreground "#cc9393" :background "#272822"))) "Unhelpful docstring.")
;; (defface term-color-green   '((t (:foreground "#7f9f7f" :background "#272822"))) "Unhelpful docstring.")
;; (defface term-color-yellow  '((t (:foreground "#f0dfaf" :background "#272822"))) "Unhelpful docstring.")
;; (defface term-color-blue    '((t (:foreground "#6d85ba" :background "#272822"))) "Unhelpful docstring.")
;; (defface term-color-magenta '((t (:foreground "#dc8cc3" :background "#272822"))) "Unhelpful docstring.")
;; (defface term-color-cyan    '((t (:foreground "#93e0e3" :background "#272822"))) "Unhelpful docstring.")
;; (defface term-color-white   '((t (:foreground "#dcdccc" :background "#272822"))) "Unhelpful docstring.")
;; '(term-default-fg-color ((t (:inherit term-color-white))))
;; '(term-default-bg-color ((t (:inherit term-color-black))))

;; ;; ansi-term colors
;; (setq ansi-term-color-vector
;;   [unspecified
;;    term-color-black
;;    term-color-red
;;    term-color-green
;;    term-color-yellow
;;    term-color-blue
;;    term-color-magenta
;;    term-color-cyan
;;    term-color-white]
;; )
;===}}

(global-set-key "\C-z" 'shell)
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1" "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"]
)
;(setq ansi-color-map (ansi-color-make-color-map))

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
)
(setenv "PAGER" "cat")

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defvar my-shells
  '("*shell*" "*shell1*"))
(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer))))
            )
        (put-text-property comint-last-output-start output-end 'read-only t)
      )
  )
)
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ---------------------------------------------------------------------
;; Non-MacOSX specific settings
;; ---------------------------------------------------------------------
;(unless (equal system-type 'darwin)
;...
;)

;; ---------------------------------------------------------------------
;; MacOSX specific settings
;; ---------------------------------------------------------------------
(when (equal system-type 'darwin)
  ;; -------------------------------------------------------------------
  ;; ---- Meta key: bind to Apple key
;  (setq ns-command-modifier 'meta)
  ;; -------------------------------------------------------------------
  ;; Unload TeX
;  (unload-feature 'tex-site)
)

;; ---------------------------------------------------------------------
;; Settings for graphic mode only
;; ---------------------------------------------------------------------
(unless (equal window-system 'nil)
  ;; -------------------------------------------------------------------
  ;; Set default font to use everywhere
  (when (equal system-type 'darwin)
    (set-default-font
      "-apple-Liberation_Mono-medium-normal-normal-*-12-*-*-*-m-0-fontset-auto1")
  )
  ;; -------------------------------------------------------------------
  ;; ---- Window :
  ;;c  ... set initial Emacs window geometry
  ;; Monaco => 181 x 63
  ;; Liberation => 209 x 65, 152x46
  (setq-default
    ; fullscreen VBox (vertical)
;	initial-frame-alist '((top . 0) (left . 0) (width . 115) (height . 89))
;	default-frame-alist '((top . 0) (left . 0) (width . 115) (height . 89))
    ; windowed VBox (horizontal)
;	initial-frame-alist '((top . 0) (left . 0) (width . 212) (height . 51))
;	default-frame-alist '((top . 0) (left . 0) (width . 212) (height . 51))
	; fullscreen VBox (horizontal)
;	initial-frame-alist '((top . 0) (left . 0) (width . 185) (height . 54))
;	default-frame-alist '((top . 0) (left . 0) (width . 185) (height . 54))
    ; Prometheus laptop
    initial-frame-alist '((top . 0) (left . 0) (width . 151) (height . 36))
    default-frame-alist '((top . 0) (left . 0) (width . 151) (height . 36))
  ;;   ... prevent resizing frames for "smart" positioning
    smart-frame-positioning-mode 'nil
  )
  ;; -------------------------------------------------------------------
  ;; Change automatic window split behavior - we want it to be vertical, ie side by side
  ;; Second statement makes it fallback to the default horizontal split when
  ;; buffer windows become too narrow.
  (setq split-height-threshold nil)
  (setq split-width-threshold 80)
  ;; -------------------------------------------------------------------
  ;; ---- Mouse :
  ;;   ... enable mouse wheel
  (mouse-wheel-mode 't)
  ;;   ... don't let mouse pointer under cursor, set avoidance to one of:
  ;;       {animate,banish,exile,jump,cat-and-mouse,proteus}
  (require 'avoid)
  (if (display-mouse-p) (mouse-avoidance-mode 'proteus))
  ;; -------------------------------------------------------------------
  ;; ---- Toolbar : remove
  (tool-bar-mode 0)
  ;; -------------------------------------------------------------------
  ;; ---- Cursor : specify look
  (setq-default cursor-type 'box)
  ;; -------------------------------------------------------------------
  ;; ---- Fringe :
  ;;   ... activate it (left - right widths in pixels)
  (fringe-mode '(5 . 0))
; (fringe-mode (quote (5 . 5)) nil (fringe))
  ;;   ... show buffer boundaries and arrows (on the left side only)
  (setq-default
    indicate-buffer-boundaries
        '((t) (top . left) (bottom . left) (up . left) (down . left))
    indicate-empty-lines 't
  )
  ;; -------------------------------------------------------------------
  ;; ---- Scrollbars: remove
  (set-scroll-bar-mode 'nil)
  ;; -------------------------------------------------------------------
  ;; ---- Tab bar: remove
  (setq-default tabbar-mode 'nil)
  ;; -------------------------------------------------------------------
  ;; ---- Show 80th column vertical line:
  (require 'fill-column-indicator)
  (setq fci-rule-color "grey12")
  (setq fci-rule-column 80)
;  (setq fci-rule-use-dashes t)
;  (setq fci-dash-pattern 0.1)
  (add-hook 'prog-mode-hook 'fci-mode)
  ;(add-hook 'after-change-major-mode-hook 'fci-mode)
;  (define-globalized-minor-mode global-fci-mode fci-mode (
;                     lambda () (fci-mode 1)))
;  (global-fci-mode 1)
  ;; ---- Text coloring (must be before faces):
  ;;   ... show leading tabs (use 'C-x h M-x tabify' to tabify buffer)
  (require 'show-wspace)
  (add-hook 'font-lock-mode-hook 'highlight-tabs)
;  (add-hook 'font-lock-mode-hook 'highlight-hard-spaces)
  ;; -------------------------------------------------------------------
  ;; ---- Trailing whitespaces:
  ;;   ... show with different (red) color
  (setq-default show-trailing-whitespace 't)
  ;;   ... delete them when file is saved
  (add-hook 'write-file-functions 'delete-trailing-whitespace)
;  ;; ---- Color space after 80th character with different color:
;  (require 'whitespace)
;  (setq whitespace-line-column 80) ;; limit line length to 80 characters
;  (setq whitespace-style '(face lines-tail))
;  ;;   ... activate for prog mode only:
;  ;(add-hook 'prog-mode-hook 'whitespace-mode)
;  ;;   ... activate globally
;  (global-whitespace-mode +1)
  ;; -------------------------------------------------------------------
  ;;   ... put as much syntax highlighting as possible
  (setq font-lock-maximum-decoration t)
  ;;c  ... highlight current line
  (global-hl-line-mode 't)
  ;; -------------------------------------------------------------------
  ;; ---- Faces:
  (set-face-attribute 'default nil
                      :foreground "aquamarine"
                      :background "black")
  (set-face-attribute 'fringe nil
                      :background "grey20"
                      :foreground "cyan")
  (set-face-attribute 'match nil
                      :background "RoyalBlue3"
                      :foreground "black")
  (set-face-attribute 'hl-line nil
                      :background "grey12")
  (set-face-attribute 'pesche-hardspace nil
                      :background "sienna4")
  (set-face-attribute 'pesche-space nil
                      :background "brown4")
  (set-face-attribute 'pesche-tab nil
                      :background "grey8")
  (set-face-attribute 'trailing-whitespace nil
                      :background "red4")
;  (set-face-attribute 'whitespace-line nil
;                     :background "gray8"
;                     :foreground 'unspecified)
;  (set-face-attribute 'whitespace-highlight nil
;                      :background "DeepPink4")
  (set-face-attribute 'mode-line nil
                      :background "grey12"
                      :foreground "lightgreen")
  (set-face-attribute 'mode-line-inactive nil
                      :background "grey24"
                      :foreground "black")
  (set-face-attribute 'header-line nil
                      :background "grey12"
                      :foreground "lightgreen")
; this is set later after which-function-mode is activated
;  (set-face-attribute 'which-func nil
;                     :foreground "PaleVioletRed1")
  (set-face-attribute 'region nil
                      :background "SteelBlue4")
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "gray32")
  (set-face-attribute 'font-lock-function-name-face nil
                      :foreground "DodgerBlue")
  (set-face-attribute 'font-lock-keyword-face nil
                      :bold t :foreground "CornflowerBlue")
  (set-face-attribute 'font-lock-preprocessor-face nil
                      :foreground "CornFlowerBlue")
  (set-face-attribute 'font-lock-string-face nil
                      :foreground "LimeGreen")
  (set-face-attribute 'font-lock-constant-face nil
                      :bold t :foreground "DarkOrchid")
  (set-face-attribute 'font-lock-type-face nil
                      :foreground "thistle")
)

;; ---------------------------------------------------------------------
;; Settings for console mode only
;; ---------------------------------------------------------------------
(when (eq window-system 'nil)
  ;; -------------------------------------------------------------------
  ;; ---- Faces:
  (set-face-attribute 'default nil
                      :foreground "cyan"
                      :background "black")
  (set-face-attribute 'trailing-whitespace nil
                      :background "red")
)

;; ---------------------------------------------------------------------
;; Activate additional features here to be able to tune them later
;; ---------------------------------------------------------------------

;; -------------------------------------------------------------------
;; ---- Put in scratch a list of mostly used keybindings currently set
(setq initial-scratch-message "
=============================== Useful keybindings =============================
  F2          - Toggle bookmark
  F12         - Next bookmark
  Shift-F12   - Prev bookmark
  Alt-Up/Down - Search word under cursor Up/Down
  Ctrl-x/     - Toggle comment on selected region
  Ctrl-_      - Undo
  Ctrl-xrd    - Delete rectangle
  Ctrl-xrri   - Copy rectangle
  Ctrl-xrii   - Paste rectangle
================================ SCRATCH BUFFER ================================
")

;; -------------------------------------------------------------------
;; ---- Google client
;(add-to-list 'load-path "~/.emacs.d/extra/g-client")
;(defun google ()
;  (interactive)
;  (load-library "g")
;)

;; -------------------------------------------------------------------
;; ---- Jabber client
;(add-to-list 'load-path "~/.emacs.d/extra/jabber")
;(add-to-list 'load-path "~/.emacs.d/extra/jabber/compat")
;;(require 'jabber-autoloads)
;(defun my-jabber-chat-delete-or-bury ()
;  (interactive)
;  (if (eq 'jabber-chat-mode major-mode)
;      (condition-case e
;          (delete-frame)
;        (error
;         (if (string = "Attempt to delete the sole visible or iconified frame"
;                      (cadr e))
;            (bury-buffer)
;        )
;       )
;     )
;  )
;)
;(defun jabber ()
;  (interactive)
;  (require 'jabber)
;;  (require 'jabber-autoloads)
;  (define-key jabber-chat-mode-map [escape]
;    'my-jabber-chat-delete-or-bury
;  )
;  (define-key mode-specific-map "jr"
;    '(lambda ()
;      (interactive)
;      (switch-to-buffer "*-jabber-*")
;    )
;  )
;  (define-key mode-specific-map "jc"
;    '(lambda ()
;       (interactive)
;       (call-interactively 'jabber-connect)
;    )
;  )
;  (define-key mode-specific-map "jd"
;    '(lambda ()
;       (interactive)
;       (call-interactively 'jabber-disconnect)
;    )
;  )
;  (define-key mode-specific-map "jj"
;    '(lambda ()
;       (interactive)
;       (call-interactively 'jabber-chat-with)
;    )
;  )
;  (define-key mode-specific-map "ja"
;    '(lambda ()
;       (interactive)
;       (jabber-send-presence "away" "" 10)
;    )
;  )
;  (define-key mode-specific-map "jo"
;    '(lambda ()
;       (interactive)
;       (jabber-send-presence "" "" 10)
;    )
;  )
;  (define-key mode-specific-map "jx"
;    '(lambda ()
;       (interactive)
;       (jabber-send-presence "xa" "" 10)
;    )
;  )
;
;  ;; ---- Send message on Ctrl-Enter
;  (define-key jabber-chat-mode-map (kbd "RET") 'newline)
;  (define-key jabber-chat-mode-map [C-return] 'jabber-chat-buffer-send)
;
;  ;; ---- Change chat prompt
;  (setq my-chat-prompt "[%t] %n>\n")
;  (when (featurep 'jabber)
;   (setq
;    jabber-chat-foreign-prompt-format my-chat-prompt
;    jabber-chat-local-prompt-format my-chat-prompt
;    jabber-groupchat-prompt-format my-chat-prompt
;    jabber-muc-private-foreign-prompt-format "[%t] %g/%n>\n"
;   )
;  )
;  ;; ---- Save chat log in ~/.emacs-jabber
;  (setq
;   jabber-history-enabled t
;   jabber-use-global-history nil
;   jabber-backlog-number 40
;   jabber-backlog-days 30
;  )
;  ;; ---- Enable clicking on URL in chat log
;  (add-hook 'jabber-chat-mode-hook 'goto-address)
;
;  ;; ---- Activate smileys automatically when chatting
;  (require 'autosmiley)
;  (add-hook 'jabber-chat-mode-hook 'autosmiley-mode)
;
;  ;; ---- Add accounts
;  (setq
;   jabber-account-list
;   '(("erleya@gmail.com"
;;    (:password . "952xmrhr") ;or (:password . nil)
;     (:network-server . "talk.google.com")
;;    (:port . 443)
;     (:connection-type . ssl)
;    )
;;   ("alexei.troussov@chat.facebook.com"
;;    (:network-server . "chat.facebook.com")
;;    (:connection-type . network)
;;   )
;   )
;  )
;  ;; ---- And finally connect
;;  (jabber-connect-all)
;)

;; ---- Dockerfile mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ---- Activate SEMANTIC and CEDET
(semantic-mode 1)
(require 'cedet-files)
(require 'srecode)
(global-srecode-minor-mode 1)
(global-ede-mode 1)
(ede-enable-generic-projects)

;; ---- Load CEDET
;(require 'cedet)
;; ---- Enable EDE project mode support
;(require 'ede)
;(global-ede-mode 't)
;; ---- Enable template insertion menu with SRECODE
;(require 'srecode)
;(global-srecode-minor-mode 1)
;; ---- Activate SEMANTIC
;(unless (equal system-type 'darwin)
;; (semantic-load-enable-minimum-features)
;; (semantic-load-enable-code-helpers)
;  (semantic-load-enable-gaudy-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;)

; (frame-background-mode (quote dark))
; (global-font-lock-mode t nil (font-lock))
; (global-semantic-highlight-edits-mode t nil (semantic-util-modes))
; (global-semantic-show-unmatched-syntax-mode t nil (semantic-util-modes))
;; (global-semantic-tag-folding-mode t nil (semantic-util-modes))
;(gnus-secondary-servers (quote ("news.gmane.org" "news.free.fr")))
; (save-place t nil (saveplace))
; (semantic-c-dependency-system-include-path (quote ("d:/msys/include")))
; (semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-ghost))
; (semantic-idle-scheduler-idle-time 3)
; (semantic-idle-scheduler-mode t t)
;(visible-bell t)

;; -------------------------------------------------------------------
;; ---- Delete seleted text when typing
(delete-selection-mode 1)

;; -------------------------------------------------------------------
;; ---- Startup message: remove
(setq inhibit-startup-message 't)

;; ---------------------------------------------------------------------
;; Mode line:
;; - show current function name when possible
(which-function-mode 't)
(unless (eq window-system 'nil)
  (set-face-attribute 'which-func nil
                      :foreground "PaleVioletRed1")
)
;;c- show percentage of battery left
(display-battery-mode 't)
;;c- show file size (i.e. "36% of 22k")
(size-indication-mode 't)
;; - show line and column numbers
(column-number-mode 't)
;; - show time/date
(setq display-time-24hr-format 't)
(setq display-time-day-and-date 't)
(display-time-mode 't)

;; ---------------------------------------------------------------------
;; Highlight matching parentheses
(show-paren-mode 't)

;; ---------------------------------------------------------------------
;; Turn on automatic buffer compression/decompression
(auto-compression-mode 't)

;; ---------------------------------------------------------------------
;; Activate autosaving buffer contents without backup files
(auto-save-mode 't)
(setq make-backup-files 'nil)

;; ---------------------------------------------------------------------
;; CUA settings
(setq cua-enable-cua-keys 'nil)
(setq cua-highlight-region-shift-only 't)

;; ---------------------------------------------------------------------
;; Setup language environment
(set-language-environment "UTF-8")
;(setq-default default-input-method "cyrillic-jcuken")

;; ---------------------------------------------------------------------
;; Adjust DELETE key
(setq delete-key-deletes-forward 't)
;; Delete with Del ;)
(global-set-key [del] 'delete-char)

;; ---------------------------------------------------------------------
;; Cursor position:
;; - save position in all opened files
(setq-default save-place 't)
;; - keep same position when scrolling buffer
(setq scroll-preserve-screen-position 't)

;; ---------------------------------------------------------------------
;; ???
(setq get-frame-for-buffer-default-instance-limit 0)
;; ???
(setq load-home-init-file 't)

;; ---------------------------------------------------------------------
;; Make searching case-sensitive
(setq-default case-fold-search 'nil)

;; ---------------------------------------------------------------------
;; Use tabulation of 4 space characters where possible
(setq indent-tabs-mode 't)
(setq-default tab-width 4)

;; ---------------------------------------------------------------------
;; Ensure buffer always end up with an empty line
(setq require-final-newline 't)

;; ---------------------------------------------------------------------
;; Truncate long lines
(setq-default truncate-lines 't)
;; Wrap long lines and consider them separate for cursor navigation
;(visual-line-mode 't)

;; ---------------------------------------------------------------------
;; Scrolling:
;; - enable conservative scrolling (doesn't satisfy me => TODO)
(setq scroll-conservatively 50)
(setq scroll-margin 4)
(setq scroll-step 1)

;; ---------------------------------------------------------------------
;; Do not ask me type "yes" or "no", but "y" or "n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; ---------------------------------------------------------------------
;; Always ask before quit, stay with Emacs as long as possible :)
;(setq confirm-kill-emacs 'yes-or-no-p)

;; ---------------------------------------------------------------------
;; iGrep (TODO: set default search path to the root of current project)
(defvar grep-null-device null-device)
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." 't)
;(autoload 'igrep-find "igrep"
;   "*Run `grep` via `find`..." 't)
;(autoload 'igrep-visited-files "igrep"
;   "*Run `grep` ... on all visited files." 't)
(setq igrep-find 't)

;; ---------------------------------------------------------------------
;; Overlay-based bookmarks a-la Visual Studio
(require 'bm)
(global-set-key (kbd "<f2>") 'bm-toggle)
(global-set-key (kbd "<f12>") 'bm-next)
(global-set-key (kbd "<S-f12>") 'bm-previous)

;; ---------------------------------------------------------------------
;; GNU-Emacs customization stuff
;(defun erley/customize-emacs ()
;   ;; Get rid of the *Messages* buffer
;   (setq message-log-max nil)
;   (if (get-buffer "*Messages*")
;       (kill-buffer "*Messages*")
;   )
;   ;; Get rid of *scratch* buffer
;   (if (get-buffer "*scratch*")
;       (kill-buffer "*scratch*")
;   )
;)
;(erley/customize-emacs)

;; ---------------------------------------------------------------------
;; Eat space at point up to non-space (TODO: leaves 1 space always)
(global-set-key "\C-ce" 'fixup-whitespace)

;; ---------------------------------------------------------------------
;; Kill current buffer without confirmation
(defun erley/kill-current-buffer()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer))
)
(global-set-key "\C-xk" 'erley/kill-current-buffer)

;; ---------------------------------------------------------------------
;; Cyclic buffer switch with Ctrl-Tab
(global-set-key '[C-tab] 'bury-buffer)

;; ---------------------------------------------------------------------
;; Cyclic window frame switch with Shift-Tab
;(global-set-key '[\S-tab] 'other-window)
(global-set-key '[backtab] 'other-window)

;; ---------------------------------------------------------------------
;; Saving Emacs Sessions - Useful when you have a bunch of source
;; files open and you don't want to go and manually open each one,
;; especially when they are in various directories. Page 377 of the
;; GNU Emacs Manual says: "The first time you save the state of the
;; Emacs session, you must do it manually, with the command M-x
;; desktop-save. Once you have done that, exiting Emacs will save the
;; state again -- not only the present Emacs session, but also
;; subsequent sessions. You can also save the state at any time,
;; without exiting Emacs, by typing M-x desktop-save again."

;(setq desktop-save 'ask-if-new)
;; look for a desktop in directories from 'desktop-path' global variable
;(desktop-read)
(desktop-save-mode 't)

;; ---------------------------------------------------------------------
;; Search word under cursor with F2 and Shift-F2 (TODO: broken, upgrade?)
(require 'tinysearch)
;; Install default search keybindings:
;;   M-s (forward), M-r (backward),
;;   M-Mouse-1 (forward), C-M-Mouse-1 (backward)
;(add-hook 'tinysearch--load-hook 'tinysearch-install)
;(global-set-key "\M-s" 'tinysearch-search-word-forward)
;(global-set-key "\M-r" 'tinysearch-search-word-backward)
(global-set-key (kbd "<M-down>") 'tinysearch-search-word-forward)
(global-set-key (kbd "<M-up>") 'tinysearch-search-word-backward)

;; =====================================================================
;; ======================= END OF BASE CONFIGURATION ===================
;; =====================================================================

;; =====================================================================
;; Sets "Ctrl-x /" for commenting selected lines
(global-set-key "\C-x/" 'comment-or-uncomment-region)

;; =====================================================================
;; SVN integration
(require 'psvn)

;; =====================================================================
;; CVS integration
;(require 'pcvs)
;; Changelog autoupdate doesn't work currently
;(require 'add-log)

;; =====================================================================
;; Where to look for TAGS file
;; As far as this variable is stored in desktop file, you should probably
;;  make sure it's not overriden when you change it here
;(setq-default tags-table-list '("~/dev/work/DB"))

;; =====================================================================
;; nXml Mode
;(when (equal system-type 'darwin)
;  (load "~/Library/Preferences/Aquamacs Emacs/extra/nxml-mode/rng-auto.el")
;  (load "~/.emacs.rc/extra/nxml-mode/rng-auto.el")
;)
(add-to-list 'auto-mode-alist
  (cons (concat "\\." (regexp-opt
    '("xml" "xsl" "html" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
    'nxml-mode
  )
)
(unify-8859-on-decoding-mode)
(setq
 nxml-child-indent 4
 nxml-outline-child-indent 4
 nxml-slash-auto-complete-flag t
)
(mapc ;; use nxml-mode instead of sgml, xml or html mode
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode)
           (eq (cdr pair) 'html-mode)
           )
       (setcdr pair 'nxml-mode)
     )
   )
 magic-mode-alist
)
(setq magic-mode-alist (append '(
    ("\0357\0273\0277<\\?xml "  . nxml-mode)
    ("<\\!DOCTYPE HTML"         . nxml-mode)
    ("<\\?xml "                 . nxml-mode)
    )  magic-mode-alist)
)
;; =====================================================================
;; Reformat one-line XML to pretty format
(defun my-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region by inserting linebreaks to separate
tags that have nothing but whitespace between them. It then indents
the markup by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "XML reformatting is completed")
  )

;; =====================================================================
;; CC-Mode comments tuning
(defun erley/c-mode-common-hook()
  (c-setup-filladapt)
  (filladapt-mode 1)
)
(add-hook 'c-mode-common-hook 'erley/c-mode-common-hook)
;; =====================================================================
;; Define default code styles
(setq c-default-style '((c-mode     . "stroustrup")
                        (c++-mode   . "stroustrup")
                        (java-mode  . "java")
                        (idl-mode   . "stroustrup")
                        (other      . "gnu"))
)

;; =====================================================================
;; Go into proper mode according to file extension
(setq auto-mode-alist (append '(
    ("\\.cc$"                                   . c++-mode)
    ("\\.cpp$"                                  . c++-mode)
    ("\\.cxx$"                                  . c++-mode)
    ("\\.hxx$"                                  . c++-mode)
    ("\\.\\(cmd\\|bat\\)$"                      . cmd-mode)
    ("\\.h$"                                    . c-mode)
    ("\\.hh$"                                   . c++-mode)
    ("\\.idl$"                                  . c++-mode)
    ("\\.c$"                                    . c-mode)
    ("\\.mk\\'"                                 . makefile-mode)
    ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?"    . makefile-mode)
    ("\\.pl$"                                   . perl-mode)
    ("\\.pm$"                                   . perl-mode)
    ("\\.java$"                                 . java-mode)
    ("\\.js$"                                   . java-mode)
    ("\\.txt$"                                  . text-mode)
    ("\\.py$"                                   . python-mode)
    ("ChangeLog\\'"                             . change-log-mode)
    ("change\\.log\\'"                          . change-log-mode)
    ("changelo\\'"                              . change-log-mode)
    ("ChangeLog\\.[0-9]+\\'"                    . change-log-mode)
    ("changelog\\'"                             . change-log-mode)
    ("changelog\\.[0-9]+\\'"                    . change-log-mode)
    ("\\$CHANGE_LOG\\$\\.TXT"                   . change-log-mode)
    ("\\.tar\\'"                                . tar-mode)
    ("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\)\\'"  . archive-mode)
    ("\\.\\(ARC\\|ZIP\\|LZH\\|ZOO\\|JAR\\)\\'"  . archive-mode)
    ("\\.el$"                                   . emacs-lisp-mode)
    ("\\.emacs\\'"                              . emacs-lisp-mode)
    ("[]>:/\\]\\..*emacs\\'"                    . emacs-lisp-mode)
    ("\\`\\..*emacs\\'"                         . emacs-lisp-mode)
    ("[:/]_emacs\\'"                            . emacs-lisp-mode)
    ) auto-mode-alist)
)

;; =====================================================================
;; When I yank a piece of code ( known as paste in Windows lingo )
;; into an existing function, I like to have it indent itself to the
;; proper level automatically. This simple macro runs yank ( C-y )
;; followed by an indent current function. ( C-c C-q )
(fset 'erley/do-smart-yank "\C-y\C-c\C-q")
(global-set-key "\C-cy" 'erley/do-smart-yank)

;; =====================================================================
;; Compile command behavior:
;; ---- customize compilation buffer
(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

;; ---- set default command
(setq compile-command "gmake -f Makefile all")

;; ---- or, alternatively, bind utility function to do everything
(defun erley/compile()
  "Saves all unsaved buffers, and runs 'compile' in the current project's root directory"
  (interactive)
  (save-some-buffers t)
  (let* (
         (current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer))
                           default-directory))
         )
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
        ))
;;    (message "AA: %s" root-dir)
;  (concat "cd " root-dir "; make -j2 -f Makefile all")
  (compile '(concat "cd " root-dir "; gmake -j2 -f Makefile all"))
)
(global-set-key "\C-cb" 'erley/compile)

;; =====================================================================
;; Prints the ascii table
(setq ascii-unprint-chars-low ["NUL " "SOH " "STX " "ETX " "EOT "
                               "ENQ " "ACK " "BEL " "BS  " "HT  "
                               "LF  " "VT  " "FF  " "CR  " "SO  "
                               "SI  " "DLE " "DC1 " "DC2 " "DC3 "
                               "DC4 " "NAK " "SYN " "ETB " "CAN "
                               "EM  " "SUB " "ESC " "FS  " "GS  "
                               "RS  " "US  "])
(defun ascii-table()
    "Prints a formatted ASCII table.    With control characters symbolically shown"
    (interactive)
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (insert "ASCII Table:\n\n")
    (let ((i 0))
    (let ((j 0))    ; Start table - print its header:
    (insert "    0    1    2    3    4    5    6    7    8    9    A    B    C    D    E   F")
    (while (< i 16)
        (setq j 0)
        ; Add in "Not Ascii after this point seperator" if i = 8
        (if (= i 8)
        (insert "\n\nCharacters after 127 aren't defined in the ASCII spec\nbut are defined on this computer's locale as\n"))
        ; start of new line, insert table index
        (insert (format "\n %X  " i))
        (while (< j 16)
        (let ((char-num (+ (* i 16) j)))
        (if (or (< char-num 32))
            (insert (aref ascii-unprint-chars-low char-num))
            (if (= char-num 127)
                (insert "DEL ")
            (if (or (< char-num 127) (> char-num 159))
                (insert (format "%c   " char-num))
                (insert "    ")))))
        (setq j (+ j 1)))
        (setq i (+ i 1)))))
    (beginning-of-buffer)
)

;; =====================================================================
;; Load host-specific config file
(defun erley/get-short-hostname ()
  (let* ((sys-name (system-name))
         (idx (string-match "\\." sys-name)))
    (if idx
        (substring sys-name 0 idx)
      sys-name))
)
(let* (
       (fname (concat "~/.emacs.d/emacs-rc-local-" (erley/get-short-hostname) ".el"))
       )
  (when (file-exists-p fname)
    (load fname)
    )
)

;; =====================================================================
;; Load customization file
;; Argument "noerror" tells Emacs to ignore errors if that file doesn't exist
(load custom-file 'noerror)
