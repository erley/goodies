;; =====================================================================
;; Here we set path to extra lisp modules not included
;; into standard Emacs distribution
(add-to-list 'load-path (expand-file-name "D:/emacs/lisp/extra"))
(add-to-list 'load-path (expand-file-name "D:/emacs/lisp/extra/cedet/common"))

;; =====================================================================

;; Load CEDET
(require 'cedet)

;; Enable EDE project mode support
(require 'ede)
(global-ede-mode t)

;; Enable template insertion menu with SRECODE
(global-srecode-minor-mode 1)

;; Activate SEMANTIC
;(semantic-load-enable-minimum-features)
;(semantic-load-enable-code-helpers)
(semantic-load-enable-gaudy-code-helpers)
;(semantic-load-enable-excessive-code-helpers)

;; =====================================================================

;; which-func-mode ( From gnu emacs FAQ ) If you set
;; which-func-mode-global via customize, which-func-mode will not turn
;; on automatically. You need to add the following to your startup
;; file BEFORE the call to custom-set-variables:
(which-func-mode 1)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(allow-remote-paths (quote t))
 '(auto-compression-mode t nil (jka-compr))
 '(auto-save-mode t)
 '(bar-cursor 0)
 '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup") (java-mode . "java") (idl-mode . "stroustrup") (other . "gnu"))))
 '(case-fold-search t)
 '(column-number-mode t)
 '(compile-command "make -f Makefile all")
 '(delete-key-deletes-forward t)
 '(desktop-path (quote ("~/.emacs.d")))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(frame-background-mode (quote dark))
 '(fringe-mode (quote (5 . 5)) nil (fringe))
 '(get-frame-for-buffer-default-instance-limit 0)
 '(global-font-lock-mode t nil (font-lock))
 '(global-semantic-highlight-edits-mode t nil (semantic-util-modes))
 '(global-semantic-show-unmatched-syntax-mode t nil (semantic-util-modes))
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
 '(gnus-secondary-servers (quote ("news.gmane.org" "news.free.fr")))
 '(indent-tabs-mode t)
 '(indicate-buffer-boundaries (quote ((t . right) (top . left))))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(require-final-newline t)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(semantic-c-dependency-system-include-path (quote ("d:/mingw/msys/1.0/include")))
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-ghost))
 '(semantic-idle-scheduler-idle-time 3)
 '(semantic-idle-scheduler-mode t t)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(truncate-lines t)
 '(visible-bell t)
 '(which-function-mode t nil (which-func)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "cyan" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "DejaVu LGC Sans Mono"))))
 '(cursor ((t (:background "light green" :foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "gray32"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 '(font-lock-function-name-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
 '(font-lock-preprocessor-face ((t (:foreground "CornFlowerBlue"))))
 '(font-lock-string-face ((t (:foreground "LimeGreen"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "thistle"))))
 '(fringe ((((class color) (background dark)) (:background "gray24"))))
 '(header-line ((default (:inherit mode-line)) (((class color grayscale) (background dark)) (:background "grey24" :foreground "white"))))
 '(menu ((((type x-toolkit)) (:background "dim gray" :foreground "light gray"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "dim gray" :foreground "light gray" :box (:line-width -1 :style released-button) :height 0.8))))
 '(pesche-hardspace ((t (:background "gray8"))))
 '(pesche-space ((t (:background "gray8"))))
 '(pesche-tab ((t (:background "gray8"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "SteelBlue4"))))
 '(scroll-bar ((t (:background "gray24" :foreground "burlywood4"))))
 '(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "aquamarine")))))

;; ===== CEDET { =====

;; =====================================================================
;; Enable name completion and class member info feature
(require 'semantic-ia)

;; =====================================================================
;; Highlight #include directives to make its state visible
;; (this is not needed in most cases as semantic auto-activates it)
;(require 'semantic-decorate-include)

;; =====================================================================
;; imenu integration with Semantic
(add-hook 'semantic-init-hooks '(lambda()
  (imenu-add-to-menubar "TAGS")))

;; =====================================================================
;; Keyboard bindings for CEDET
(defun my-cedet-hook ()
  ;; complete identifier with Ctrl-Enter
  (local-set-key (quote [C-return]) 'semantic-ia-complete-symbol)
  ;; show drop-down list of possible completions
  ;; we don't use 'semantic-ia-complete-tip' as it shows tooltip at the mouse pointer
  (local-set-key (quote [apps]) 'semantic-ia-complete-symbol-menu)
  ;; show drop-down list of possible completions automatically after "." and "->"
  (local-set-key (kbd ".") 'semantic-complete-self-insert)
  (local-set-key (kbd ">") 'semantic-complete-self-insert)
  ;; Fold/unfold semantic tag
  (local-set-key (kbd "<C-f11>") 'senator-unfold-tag)
  (local-set-key (kbd "<C-f12>") 'senator-fold-tag)
  ;; Jump to the definition of the symbol under cursor in the whole project
  (local-set-key (kbd "<M-f3>") 'semantic-ia-fast-jump)
  ;; Find all the references to the symbol in the whole project (my favorite!)
  (local-set-key (kbd "<C-f3>") 'semantic-symref-symbol)
;;TODO: enable smart comments (doesn't work?)
;  (c-setup-filladapt)
;  (filladapt-mode 1)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'c++-mode-common-hook 'my-cedet-hook)

;; ===== CEDET } =====

;; =====================================================================
;; To allow to type y instead of "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; =====================================================================
;;; The "Windows" keys on newer keyboards bring up the Start menu
;;; whether you want it or not - make Emacs ignore these keystrokes
;(global-set-key [lwindow] 'ignore) ;115 or 125
;(global-set-key [rwindow] 'ignore) ;116 or 126

;; =====================================================================
;; Make ediff always split horizontally
;(setq ediff-split-window-function 'split-window-horizontally)

;; Make ediff split horizontally only if the frame is larger than 140 chars in width
(setq ediff-split-window-function
	  (lambda (&optional arg)
		(if (> (frame-width) 140)
			(split-window-horizontally arg)
		  (split-window-vertically arg))))

;; =====================================================================
;; Use emacs to compare two files from command-line
;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
		(file2 (pop command-line-args-left)))
	(ediff file1 file2)))
(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; =====================================================================
;; Some usefull keyboard shortcuts for ediff-mode
;; TODO: (it doesn't work and btw standard key bindings aren't bad: n, p, a, b, !, q)
;(defun my-ediff-hook ()
;  ;; next difference
;  (local-set-key (kbd "<M-down>") 'ediff-next-difference)
;  ;; previous difference
;  (local-set-key (kbd "<M-up>") 'ediff-previous-difference)
;  ;; copy delta from left to right
;  (local-set-key (kbd "<M-right>") 'ediff-copy-A-to-B)
;  ;; copy delta from right to left
;  (local-set-key (kbd "<M-left>") 'ediff-copy-B-to-A)
;  )
;(add-hook 'ediff-mode-common-hook 'my-ediff-hook)

;; =====================================================================
;; Highlite current line
(require 'hl-line)
(setq
 global-hl-line-mode t
 hl-line-face '((t (:background "grey10" )))
;hl-line-face '((t (:inherit highlight :background "black" :underline "LightYellow4")))
 )

;; =====================================================================
;; Change automatic window split behavior - we want it to be vertical, ie side by side
;; Second statement makes it fallback to the default horizontal split when
;; buffer windows become too narrow.

;; Split a window vertically only if it has at least this many lines.
;; If this is nil, `split-window-sensibly' is not allowed to split a window vertically.
;; If, however, a window is the only window on its frame, `split-window-sensibly' may
;; split it vertically disregarding the value of this variable.
(setq split-height-threshold nil)
;; Split a window horizontally only if it has at least this many columns.
;; If this is nil, `split-window-sensibly' is not allowed to split a window horizontally.
(setq split-width-threshold 140)

;; =====================================================================
;; Set shell command interpreter to MSYS bash shell.
;; Startup script has to be put in ~/.emacs_bash
(require 'shell)
(setq shell-file-name "bash" ;; don't add .exe part here
	  explicit-shell-file-name shell-file-name
	  msys-root "D:/mingw/msys/1.0"
	  msys-bin (concat msys-root "/bin")
	  exec-path (cons msys-bin exec-path))
(setenv "PATH" (concat msys-bin ";" (getenv "PATH")))
(setenv "SHELL" shell-file-name)
;(setenv "MANPATH" (concat
;		(concat (concat msys-root "/man:") (concat msys-root "/share/man:"))
;		(concat msys-root "/llvm-gcc/man")))
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "Add command history navigation in shell mode"
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
;  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (setq comint-input-sender 'n-shell-simple-send)
  ; wrap long lines by word
  (visual-line-mode t)
  )
(defun n-shell-simple-send (proc command)
  "Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer)
    )
   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command)
    )
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))
   )
  )
(setq comint-prompt-read-only t)
(setq explicit-bash-args '("--login" "-i"))
;; add colors to a shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;(setq ansi-color-names-vector ; better contrast colors
;      ["black" "red4" "green4" "yellow4" "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;(custom-set-faces  '(comint-highlight-prompt ((t (:foreground "blue15")))) )
(global-set-key "\C-z" 'shell)

;; =====================================================================
;; Rebind C-z to start a shell (use .emacs_shellname for the shells rc file)
;(setenv "ESHELL" shell-file-name)
;(global-set-key "\C-z" 'eshell)

;; =====================================================================
; Cmd & Bat files mode
(autoload 'cmd-mode "cmd-mode" "CMD mode." t)

;; =====================================================================
;; Where to look for TAGS file
(setq tags-table-list '("D:/dev/SGBD"))

;; =====================================================================
;; Make typing override text selection
(delete-selection-mode 1)

;; =====================================================================
;; Conservative scrolling (i.e. avoid scrolling jumps)
(require 'smooth-scrolling)
;(setq scroll-conservatively 10000)
;(setq scroll-preserve-screen-position (quote t))
;(setq scroll-step 1)
;(setq scroll-margin 4)

;; =====================================================================
;; iGrep
(defvar grep-null-device null-device)
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
;(autoload 'igrep-find "igrep"
;	"*Run `grep` via `find`..." t)
;(autoload 'igrep-visited-files "igrep"
;	"*Run `grep` ... on all visited files." t)
(setq igrep-find t)

;; =====================================================================
;; CVS integration (deactivated because isn't needed)
;(require 'pcvs)
;; Changelog autoupdate doesn't work currently
;(require 'add-log)

;; =====================================================================
;; SVN integration
(require 'psvn)

;; =====================================================================
;; Overlay-based bookmarks a-la Visual Studio
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<M-f2>") 'bm-next)
;(global-set-key (kbd "<S-f12>") 'bm-previous)

;; =====================================================================
;; Show leading tabs
;; Remark: use 'Ctrl-x h Alt-x tabify' to tabify the whole buffer
(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'highlight-tabs)
;(add-hook 'font-lock-mode-hook 'highlight-hard-spaces)

;; =====================================================================
;; Whitespace mode to show spaces, tabs, eol (less beautiful than show-wspace)
;(require 'whitespace-mode)
;(autoload 'global-whitespace-toggle-options "whitespace"
;  "Toggle global `whitespace-mode' options." t)
;; Face used to visualize TAB
;(add-hook 'whitespace-tab 'pesche-tab)
;; Face used to visualize SPACEs before TAB
;(add-hook 'whitespace-space-before-tab 'pesche-tab)
;; Face used to visualize empty lines at beginning and/or end of buffer
;(add-hook 'whitespace-empty 'pesche-tab)
;; Specify trailing characters regexp
;(setq 'whitespace-trailing-regexp "[ \t]+")
;; Specify column beyond which the line is highlighted
;(setq whitespace-line-column 140)

;; =====================================================================
;; Delete trailing whitespace when file is saved
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;; =====================================================================
;; Time/Calendar stuff
(setq display-time-24hr-format t)
;(setq european-calendar-style 't) ; doesn't work?
(display-time)

;; =====================================================================
;; Enable mouse wheel
(mouse-wheel-mode 1)

;; =====================================================================
;; Remove toolbar
(tool-bar-mode 0)

;; =====================================================================
;; Initial emacs window size
(setq
   initial-frame-alist '((top . 0) (left . 0) (width . 271) (height . 82))
   default-frame-alist '((top . 0) (left . 0) (width . 271) (height . 82))
)

;; =====================================================================
;; Delete with Del ;)
(global-set-key [del] 'delete-char)

;;=========================================================================
;; Put as much syntax highlighting into documents as possible
(setq font-lock-maximum-decoration t)

;; =====================================================================
;; Use nXml mode for the following file extentions
(add-to-list 'auto-mode-alist
	(cons (concat "\\." (regexp-opt
		'("xml" "xsl" "html" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		'nxml-mode)
)
(setq
	nxml-child-indent 4
	nxml-outline-child-indent 4
	nxml-slash-auto-complete-flag t
)
;; =====================================================================
;; Use nXml mode instead of sgml, xml or html mode
(mapc
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
;; =====================================================================
;; Use nXml mode when file has specific pattern in its header
(setq magic-mode-alist (append '(
	("<\\?xml " . nxml-mode)
	("<\\!DOCTYPE HTML" . nxml-mode)
	("\0357\0273\0277<\\?xml " . nxml-mode)
	) magic-mode-alist)
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
;; Go into proper mode according to file extension
(setq auto-mode-alist (append '(
	("\\.cc$"	. c++-mode)
	("\\.cpp$"	. c++-mode)
	("\\.cxx$"	. c++-mode)
	("\\.hxx$"	. c++-mode)
	("\\.\\(cmd\\|bat\\)$" . cmd-mode)
	("\\.h$"	. c-mode)
	("\\.hh$"	. c++-mode)
	("\\.idl$"	. c++-mode)
	("\\.c$"	. c-mode)
	("\\.mk$"	. makefile-mode)
	("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?" . makefile-mode)
	("\\.pl$"	. perl-mode)
	("\\.pm$"	. perl-mode)
	("\\.java$"	. java-mode)
	("\\.js$"	. java-mode)
	("\\.txt$"	. text-mode)
	("\\.el\\'"	. emacs-lisp-mode)
	("\\.emacs\\'"	. emacs-lisp-mode)
	("\\.texinfo\\'" . texinfo-mode)
	("ChangeLog\\'" . change-log-mode)
	("change\\.log\\'" . change-log-mode)
	("changelo\\'" . change-log-mode)
	("ChangeLog\\.[0-9]+\\'" . change-log-mode)
	("changelog\\'" . change-log-mode)
	("changelog\\.[0-9]+\\'" . change-log-mode)
	("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
	("\\.tar\\'" . tar-mode)
	("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\)\\'" . archive-mode)
	("\\.\\(ARC\\|ZIP\\|LZH\\|ZOO\\|JAR\\)\\'" . archive-mode)
	("[]>:/\\]\\..*emacs\\'" . emacs-lisp-mode)
	("\\`\\..*emacs\\'" . emacs-lisp-mode)
	("[:/]_emacs\\'" . emacs-lisp-mode)
	) auto-mode-alist)
)

;; =====================================================================
;; When I yank a piece of code ( known as paste in Windows lingo )
;; into an existing function, I like to have it indent itself to the
;; proper level automatically. This simple macro runs yank ( C-y )
;; followed by an indent current function. ( C-c C-q )
(global-set-key "\C-cy" 'do-smart-yank)
(fset 'do-smart-yank
	"\C-y\C-c\C-q")

;; =====================================================================
;; Eat space at point up to non-space
(global-set-key "\C-ce" 'fixup-whitespace)

;; =====================================================================
;; Kill current buffer without confirmation
(global-set-key "\C-xk" 'kill-current-buffer)
(defun kill-current-buffer ()
	"Kill the current buffer, without confirmation."
	(interactive)
	(kill-buffer (current-buffer)))

;; =====================================================================
;; Use lazy lock for syntax coloring, otherwise large files will not
;; be colored. Also allows large files to load faster.
;(setq font-lock-support-mode 'lazy-lock-mode)
;(setq lazy-lock-defer-on-scrolling nil)
;(setq lazy-lock-defer-time 1)
;(setq lazy-lock-defer-on-the-fly nil)
;(setq lazy-lock-stealth-time 20)
;(setq lazy-lock-stealth-lines 25)
;(setq lazy-lock-stealth-verbose nil)
;(require 'font-lock)
;(require 'lazy-lock)

;; =====================================================================
;; Cyclic buffer switch
(defun my-previous-buffer ()
  "Cycle to the previous buffer with keyboard."
  (interactive)
  (bury-buffer)
  )
(global-set-key '[C-tab] 'my-previous-buffer)		; switch to the next buffer with Ctrl-Tab
;(defun my-next-buffer ()
;	"Cycle to the next buffer with keyboard."
;	(interactive)
;	(let* ((bufs (buffer-list))
;	(entry (1- (length bufs))) val)
;	(while (not (setq val (nth entry bufs)
;			  val (and (/= (aref (buffer-name val) 0) ? ) val)))
;	(setq entry (1- entry)))
;	(switch-to-buffer val)))
;(global-set-key '[C-S-tab] 'my-next-buffer) ; switch to the previous with Ctrl-Shift-Tab

;; =====================================================================
;; Cyclic window switch on Shift-Tab
(global-set-key '[backtab] 'other-window)

;; =====================================================================
;; Prints the ascii table
(setq ascii-unprint-chars-low ["NUL " "SOH " "STX " "ETX " "EOT "
							   "ENQ " "ACK " "BEL " "BS	 " "HT	"
							   "LF	" "VT  " "FF  " "CR	 " "SO	"
							   "SI	" "DLE " "DC1 " "DC2 " "DC3 "
							   "DC4 " "NAK " "SYN " "ETB " "CAN "
							   "EM	" "SUB " "ESC " "FS	 " "GS	"
							   "RS	" "US  "])
(defun ascii-table()
	"Prints a formatted ASCII table.	With control characters symbolically shown"
	(interactive)
	(switch-to-buffer "*ASCII*")
	(erase-buffer)
	(insert "ASCII Table:\n\n")
	(let ((i 0))
	(let ((j 0))	  ; Start of table. Print header.
	(insert "    0    1    2    3    4    5    6    7    8    9    A    B    C    D    E   F")
	(while (< i 16)
		(setq j 0)
		; Add in "Not Ascii after this point seperator" if i = 8
		(if (= i 8)
		(insert "\n\nCharacters after 127 aren't defined in the ASCII spec\nbut are defined on this computer's locale as\n"))
		; start of new line, insert table index
		(insert (format "\n %X	" i))
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
;; Prevent resizing frames for "smart" positioning
;(smart-frame-positioning-mode nil)

;; =====================================================================
;; Search word under cursor with F2 and Shift-F2
(require 'tinysearch)
;(defun erley/isearch-word-at-point ()
;  (interactive)
;  (call-interactively 'isearch-forward-regexp)
;  )
;(defun erley/isearch-yank-word-hook ()
;  (when (equal this-command 'erley/isearch-word-at-point)
;    (let ((string (concat "\\_<"
;                          (buffer-substring-no-properties
;                           (progn (skip-syntax-backward "w_") (point))
;                           (progn (skip-syntax-forward "w_") (point)))
;                          "\\_>")))
;      (if (and isearch-case-fold-search
;               (eq 'not-yanks search-upper-case))
;          (setq string (downcase string))
;		)
;      (setq isearch-string string
;            isearch-message
;            (concat isearch-message
;                    (mapconcat 'isearch-text-char-description
;                               string "")
;					)
;            isearch-yank-flag t)
;      (isearch-search-and-update)
;	  )
;	)
;  )
;(add-hook 'isearch-mode-hook 'erley/isearch-yank-word-hook)
;(global-set-key [f2] 'erley/isearch-word-at-point)

;; =====================================================================
;; For those who are annoyed by the mouse pointer obscuring text,
;; this mode moves the mouse pointer - either just a little out of
;; the way, or all the way to the corner of the frame.
;;
;; Legitimate alternatives include
;; `animate', `banish', `exile', `jump', `cat-and-mouse', and `proteus'.
(require 'avoid)
(if (display-mouse-p) (mouse-avoidance-mode 'proteus))

;; =====================================================================
;; Scroll down compilation window
(setq compilation-scroll-output t)

;; =====================================================================
;; Printing through Ghostscript
;(setenv "GS_LIB" "C:/Program Files/gs/gs8.60/lib;C:/Program Files/gs/fonts")
;(setq ps-lpr-command "C:/Program Files/gs/gs8.60/bin/gswin32c.exe")
;(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
;(setq ps-printer-name t)

;; =====================================================================
;; Indent when inserting new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; =====================================================================
;; Saving Emacs Sessions - Useful when you have a bunch of source
;; files open and you don't want to go and manually open each one,
;; especially when they are in various directories. Page 377 of the
;; GNU Emacs Manual says: "The first time you save the state of the
;; Emacs session, you must do it manually, with the command M-x
;; desktop-save. Once you have done that, exiting Emacs will save the
;; state again -- not only the present Emacs session, but also
;; subsequent sessions. You can also save the state at any time,
;; without exiting Emacs, by typing M-x desktop-save again.
(desktop-save-mode 1)
;; Save it only if it already exists in the directory specified by 'desktop-path'
(setq desktop-save 'if-exists)

;; =====================================================================
;; GNU-Emacs customization stuff
(defun customize-gnuemacs ()
  ;; Get rid of the *Messages* buffer (useful for debugging with --debug-init switch)
  (setq message-log-max nil)
  (if (get-buffer "*Messages*")
      (kill-buffer "*Messages*")
	)
  ;; Get rid of *scratch* buffer
  (if (get-buffer "*scratch*")
	  (kill-buffer "*scratch*")
	)
)
(customize-gnuemacs)

;; =====================================================================
;; Describe project to work with
(ede-cpp-root-project "DMSP"
                :name "DMSP Project"
                :file "D:/dev/SGBD/Makefile"
                :include-path '("/include"
                                "/include/Extra"
                               )
                :system-include-path '("D:/mingw/msys/1.0/include")
                :spp-table '(("SGBD_DEBUG" . "1")
                             ("WITH_FAILUREPOINTS" . "1")
                             ("WITH_TEARINGPOINTS" . "1")
                             ("WITH_DBIO" . "1")
                             ("WITH_DBQ" . "1")
							 ("WIN32" . "1"))
)

;; =====================================================================
;; Use UTF language environment
;; Needed to display various non-ASCI characters properly
(set-language-environment "UTF-8")

;; =====================================================================
;; Activate and configure snippets
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory (expand-file-name "D:/emacs/lisp/extra/snippets"))

;; =====================================================================
;; These setting are written in the order as described in the Emacs
;; info pages. ( Hit C-hi, then go to Emacs | Programs | Program
;; Indent | Custom C Indent | Syntactic Symbols for a description of
;; each. I found it easier to open one of my own source files, and hit
;; tab on a particular line to find the name of the syntactic
;; symbol. This assumes that the setting for
;; c-echo-syntactic-information-p is not nil. )
(defconst my-c-style
  '(
    (c-echo-syntactic-information-p . t)
    (c-basic-offset                 . 4)
    (c-toggle-auto-state            . t)
    (c-offsets-alist .
		(
		 (string                . +)
		 (c                     . +)
		 (defun-open            . 0)
		 (defun-close           . 0)
		 (defun-block-intro     . +)
		 (class-open            . 0)
		 (class-close           . 0)
		 (inline-open           . 0)
		 (inline-close          . 0)
		 (extern-lang-open      . 0)
		 (extern-lang-close     . 0)
		 (func-decl-cont        . +)
		 (knr-argdecl-intro     . +)
		 (knr-argdecl           . +)
		 (topmost-intro         . 0)
		 (topmost-intro-cont    . +)
		 (member-init-intro     . +)
		 (member-init-cont      . +)
		 (inher-intro           . +)
		 (inher-cont            . +)
		 (block-open            . 0)
		 (block-close           . 0)
		 (brace-list-open       . 0)
		 (brace-list-close      . 0)
		 (brace-list-intro      . +)
		 (brace-list-entry      . 0)
		 (statement             . 0)
		 (statement-cont        . +)
		 (statement-block-intro . +)
		 (statement-case-intro  . +)
		 (statement-case-open   . 0)
		 (substatement          . +)
		 (substatement-open     . 0)
		 (case-label            . +)
		 (access-label          . -)
		 (label                 . 0)
		 (do-while-closure      . 0)
		 (else-clause           . 0)
		 (catch-clause          . 0)
		 (comment-intro         . 0)
		 (arglist-intro         . c-lineup-arglist-intro-after-paren)
		 (arglist-cont          . c-lineup-arglist)
;        (arglist-cont-nonempty . +)
		 (arglist-cont-nonempty . c-lineup-arglist-intro-after-paren)
		 (arglist-close         . c-lineup-arglist)
		 (stream-op             . +)
		 (inclass               . +)
		 (inextern-lang         . +)
		 (cpp-macro             . 0)
		 (friend                . 0)
		 (objc-method-intro     . +)
		 (objc-method-args-cont . +)
		 (objc-method-call-cont . +)
		 ))

    (c-comment-only-line-offset . (0 . -1000))
    (c-hanging-braces-alist     . ((substatement-open after)
								   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
								   (inher-intro)
								   (case-label after)
								   (label after)
								   (access-label after)))
    (c-cleanup-list             . ((scope-operator
									empty-defun-braces
									defun-close-semi))))
  "Erley C++ Programming Style")

(defun my-c-mode-common-hook ()
  (c-add-style "erley" my-c-style t)
  (c-set-offset 'member-init-intro '+)
  (setq tab-width 4
        indent-tabs-mode nil)
  (c-toggle-auto-hungry-state t)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; =====================================================================
;; CScope integration
;(require 'ascope)
;(define-key global-map "\C-csi" 'ascope-init) ;; run this first before using cscope
;(define-key global-map "\C-css" 'ascope-find-this-symbol)
;(define-key global-map "\C-csg" 'ascope-find-global-definition)
;(define-key global-map "\C-cst" 'ascope-find-this-text-string)
;(define-key global-map "\C-csc" 'ascope-find-functions-calling-this-function)
;(define-key global-map "\C-csf" 'ascope-find-called-functions)
;(define-key global-map "\C-csu" 'ascope-find-files-including-file)
;(define-key global-map "\C-csm" 'ascope-pop-mark)
;(define-key global-map "\C-csa" 'ascope-all-symbol-assignments)
;(define-key global-map "\C-cso" 'ascope-clear-overlay-arrow)

(require 'xcscope)
(define-key global-map "\C-csi" 'cscope-set-initial-directory)
(define-key global-map "\C-csu" 'cscope-unset-initial-directory)
(define-key global-map "\C-css" 'cscope-find-this-symbol)
(define-key global-map "\C-csG" 'cscope-find-global-definition)
(define-key global-map "\C-csg" 'cscope-find-global-definition-no-prompting)
(define-key global-map "\C-csm" 'cscope-pop-mark)
(define-key global-map "\C-csn" 'cscope-next-symbol)
(define-key global-map "\C-csN" 'cscope-next-file)
(define-key global-map "\C-csp" 'cscope-prev-symbol)
(define-key global-map "\C-csP" 'cscope-prev-file)
(define-key global-map "\C-csb" 'cscope-display-buffer)
(define-key global-map "\C-cst" 'cscope-display-buffer-toggle)

;; =====================================================================
;; Automatically turn on auto-fill-mode when editing text files
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; =====================================================================
;; If you want to change the word wrap column, change this number
; (setq-default fill-column 65)

;; =====================================================================
;; Good for opening header file under cursor
(global-set-key "\C-cf" 'open-file-under-cursor)
(fset 'open-file-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-\M-f ?\M-w ?\C-x ?\C-f ?\C-y return])

;; =====================================================================
;; Sets "Ctrl-x /" for commenting selected lines
(global-set-key "\C-x/" 'comment-or-uncomment-region)

;; =====================================================================
;; Insert header file template from ~/templates directory
;(defun insert-header-skeleton ()
;  "Insert skeleton header file"
;  (interactive)
;  (insert-file-contents "~/templates/header.h"))
;(global-set-key [f5] 'insert-header-skeleton)

;; =====================================================================
;; Insert implementation file template from ~/templates directory
;(defun insert-cpp-skeleton ()
;  "Insert skeleton cpp file"
;  (interactive)
;  (insert-file-contents "~/templates/cpp.cpp"))
;(global-set-key [f6] 'insert-cpp-skeleton)

;; =====================================================================
;; USEFUL NOTES AND OTHER STUFF
;; =====================================================================

;; How to record and display a keyboard macro

;; Just open a buffer and type C-x ( Then start typing in your macro.
;; Once you are finished defining your macro type C-x ) Then type M-x
;; name-last-kbd-macro. This will allow you to call your macro
;; whatever you want. Next open up your .emacs file and position your
;; cursor where you want the code for the macro to appear.  Type M-x
;; insert-kbd-macro and type in the name.  The code will automatically
;; be generated.

;; =====================================================================
;; =====================================================================

;; Use shell-command-on-region M-| to send region to external
;; process. If you use a prefix argument , C-u M-| this will replace
;; the region with the output of the external process. Good for
;; sending something to stdin and reading from stdout.

;; =====================================================================
;; =====================================================================

;; To copy to named register: C-x r s a - Where a is the name of the
;; register ( a - z ) to save the text to.

;; To paste from named register: C-x r i a - Where a is the name of
;; the register ( a - z ) to paste the saved text from.

;; To remember current point: C-x r spc a - Where a is the name of the
;; register to save point to.

;; To jump to named point: C-x r j a - Where a is the name of the
;; register holding desired point to jump to

;; =====================================================================
;; SOME GOOD URL's FOR EMACS SOURCES
;; =====================================================================

;; http://www.splode.com/users/friedman/software/emacs-lisp/
;; http://www.anc.ed.ac.uk/~stephen/emacs/ell.html

;; =====================================================================

(defun my-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
	(nxml-mode)
	(goto-char begin)
	(while (search-forward-regexp "\>[ \\t]*\<" nil t)
	  (backward-char) (insert "\n"))
	(indent-region begin end)
	)
)
