;; This is an example of how to deal with terminal emulator
;; weirdness. Find out what the emulator sends to emacs by
;; pressing C-q and the key.
;;
;; (defvar real-keyboard-keys
;;   '(("M-<up>"        . "\M-[1;3A")
;;     ("M-<down>"      . "\M-[1;3B")
;;     ("M-<right>"     . "\M-[1;3C")
;;     ("M-<left>"      . "\M-[1;3D")
;;     ("C-<return>"    . "\C-j")
;;     ("C-<delete>"    . "\M-[3;5~")
;;     ("C-<up>"        . "\M-[1;5A")
;;     ("C-<down>"      . "\M-[1;5B")
;;     ("C-<right>"     . "\M-[1;5C")
;;     ("C-<left>"      . "\M-[1;5D"))
;;   "An assoc list of pretty key strings
;; and their terminal equivalents.")
;;
;; (defun key (desc)
;;   (or (and window-system (read-kbd-macro desc))
;;       (or (cdr (assoc desc real-keyboard-keys))
;;           (read-kbd-macro desc))))
;;
;; Ctrl+right   => forward word
(global-set-key "\M-[1;5C"    'forward-word)
;; Ctrl+left    => backward word
(global-set-key "\M-[1;5D"    'backward-word)

;; Do not create backup files.
(setq make-backup-files nil)

;; Disable startup message.
(setq inhibit-startup-message t)

;; Wind move.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; =============================
;; Load path. Require. Autoload.
;; =============================
(setq load-path (cons "~/.elisp" load-path))
(require 'redo+)            ;; in ~/.elisp
(require 'less-mode)        ;; in ~/.elisp
(require 'linum)
(require 'uniquify)
(require 'nav)              ;; in ~/.elisp
(require 'multi-term)       ;; in ~/.elisp
(require 'transpose-frame)  ;; in ~/.elisp
(require 'dired+)           ;; in ~/.elisp
(require 'dired-x)
(require 'protobuf-mode)    ;; in ~/.elisp
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)  ;; in ~/.elisp
(autoload 'rust-mode "rust-mode" "Rust editing mode" t) ;; in ~/.elisp
(add-to-list 'load-path "~/.elisp/emacs-eclim")
(add-to-list 'load-path "~/.elisp/emacs-eclim-dependencies")
(add-to-list 'load-path "~/.elisp/company-mode")
(add-to-list 'load-path "~/.elisp/yasnippet")
(require 'eclim)
(require 'eclimd)
(add-to-list 'load-path "~/.elisp/flycheck")
(require 'flycheck)
(add-to-list 'load-path "~/.elisp/py-autopep8")
(add-to-list 'load-path "~/.elisp/ein")
(add-to-list 'load-path "~/.elisp/websocket")
(add-to-list 'load-path "~/.elisp/request")
(add-to-list 'load-path "~/.elisp/cl-generic")
(require 'ein-loaddefs)

;; ============================
;; Key mappings
;; ============================

;; use F1 key to go to a man page
(global-set-key [f1] 'apropos)
;; use F2 key to kill current buffer
(global-set-key [f2] 'kill-this-buffer)
;; use F3 to get help (apropos)
(global-set-key [f3] 'revert-buffer)
;; use F5 to compile
(global-set-key [f5] 'compile)
;; use F6 to toggle a dedicated terminal window
(global-set-key [f6] 'multi-term-dedicated-toggle)
;; use F7 to select the dedicated terminal window
(global-set-key [f7] 'multi-term-dedicated-select)
;; use F8 to view the buffer in less-mode
(global-set-key [f8] 'less-mode)
;; use F9 to toggle emacs-nav
(global-set-key [f9] 'nav-toggle)
;; use F10 to switch to dired buffer
(global-set-key [f10] 'switch-to-dired-buffer)

;; goto line function C-c C-g. (kbd "C-c C-g") also works.
(global-set-key [ (control c) (control g) ] 'goto-line)
;; redo
(global-set-key (kbd "C-\\") 'redo)

;; C-<up> and C-<down> scroll one line.
(global-set-key [(control up)] 'scroll-down-one-line)
(global-set-key [(control down)] 'scroll-up-one-line)

;; Jump to a matching parenthesis.
(global-set-key "%" 'match-paren)

;; Add keybindings for next and previous compilation errors
;; (global-set-key [M-down]    'next-error)
;; (global-set-key [M-up]      '(lambda () (interactive) (next-error -1)))

;; C-c r f renames buffer, and file it's visiting
(global-set-key "\C-crf" 'rename-file-and-buffer)
;; C-c r d moves buffer, and file it's visiting, to a new directory
(global-set-key "\C-crd" 'move-buffer-file)

;; Prevent elpy from overwriting essential key bindings.
(eval-after-load "elpy"
  '(cl-dolist (key '("C-<up>" "C-<down>"))
     (define-key elpy-mode-map (kbd key) nil)))

;; ============================
;; Buffers/files.
;; ============================

;; Nicer buffer switching.
(ido-mode t)
;; Style to name buffers when visiting files with the same name.
(setq uniquify-buffer-name-style 'forward)
;; Kill other buffers.
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; ----------------------------------------------------------------
;; function to rename current buffer and the file it's visiting
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and the file it's visiting, to NEW-NAME."
 (interactive "sNew name (file+buffer): ")
 (let ((name (buffer-name))
 (filename (buffer-file-name)))
 (if (not filename)
 (message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
 (message "A buffer named '%s' already exists!" new-name)
 (rename-file name new-name 1)
 (rename-buffer new-name)
 (set-visited-file-name new-name)
 (set-buffer-modified-p nil)))))
;; ----------------------------------------------------------------
;; function to move buffer and file it's visiting, to a new directory
(defun move-buffer-file (dir)
 "Move both current buffer and the file it's visiting to DIR."
 (interactive "DNew directory (file+buffer): ")
 (let *((name (buffer-name))
 (filename (buffer-file-name))
 (dir (if (string-match-dir "\\(?:/\\|\\\\)$")
 (substring dir 0 -1) dir))
 (newname (concat dir "/" name)))
 (if (not filename)
 (message "Buffer '%s' is not visiting a file!" name)
 (copy-file filename newname 1)
 (delete-file filename)
 (set-visited-file-name newname)
 (set-buffer-modified-p nil)
 t)))

;; ============================
;; Dired.
;; ============================

;; Use a single buffer for dired.
(diredp-toggle-find-file-reuse-dir 1)

;; Can set a hot key to switch to dired buffer.
(defun switch-to-dired-buffer ()
  "Switch to dired buffer."
  (interactive)
  (let ((dbufs  (cl-remove-if-not
		 (lambda (bf)
		   (with-current-buffer bf
		     (derived-mode-p 'dired-mode)))
		 (buffer-list))))
    (switch-to-buffer (car dbufs))))

;; Set directory of other dired buffer as copy/rename target.
;; From dired-x.
(setq dired-dwim-target t)

;; Open file in external app.
(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (
	 (-file-list
	  (if (string-equal major-mode "dired-mode")
	      (dired-get-marked-files)
	    (list (buffer-file-name))))
	 (-do-it-p (if (<= (length -file-list) 5)
		       t
		     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
	(mapc
	 (lambda (-fpath)
	   (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" -fpath t t))) -file-list))
       ((string-equal system-type "darwin")
	(mapc
	 (lambda (-fpath)
	   (shell-command
	    (concat "open " (shell-quote-argument -fpath))))  -file-list))
       ((string-equal system-type "gnu/linux")
	(mapc
	 (lambda (-fpath) (let ((process-connection-type nil))
			    (start-process "" nil "xdg-open" -fpath))) -file-list))))))

;; Open file in file manager.
(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
	  (process-connection-type nil)
	  (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
			       "/usr/bin/gvfs-open"
			     "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. For example: with nautilus
    )))

;; =================================================
;; Setup syntax, background, and foreground coloring
;; =================================================

;;(set-background-color "Black")
(add-to-list 'default-frame-alist '(background-color . "Black"))
;;(set-foreground-color "White")
(add-to-list 'default-frame-alist '(foreground-color . "White"))
;;(set-cursor-color "LightSkyBlue")
(add-to-list 'default-frame-alist '(cursor-color . "LightSkyBlue"))
;;(set-mouse-color "LightSkyBlue")
(add-to-list 'default-frame-alist '(mouse-color . "LightSkyBlue"))
(global-font-lock-mode t)  ;; the default
(setq font-lock-maximum-decoration t)  ;; the default
;; Set font size.
;;(set-face-attribute 'default (selected-frame) :height 100)
(set-face-attribute 'default nil :height 100)

;; ============================
;; Mouse Settings
;; ============================

;; mouse button one drags the scroll bar
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

;; setup scroll mouse settings
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
;; (global-set-key [C-mouse-4] 'down-a-lot)
;; (global-set-key [C-mouse-5] 'up-a-lot)
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; ============================
;; Scrolling
;; ============================

;; Hide scrollbar.
(scroll-bar-mode -1)

;; C-n/<down> doesn't add new lines at the bottom of a buffer
(setq next-line-add-newlines nil)

;; scroll just one line when cursor moves off window
(setq scroll-step 1)

;; PgUp then PgDn will return exactly to the starting point.
;; Same for PgDn then PgUp.
(setq scroll-preserve-screen-position t)

;; Scroll one line in either direction.
(defun scroll-up-one-line ()
 (interactive) (scroll-up 1))
(defun scroll-down-one-line ()
 (interactive) (scroll-down 1))

;; ============================
;; Display
;; ============================

;; display the current time
(display-time)

;; Show column number at bottom of screen
(column-number-mode 1)

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable blink-cursor-mode
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; format the title-bar to always include the buffer name
(setq frame-title-format "%b")

;; show a menu only when running within X (save real estate when in console)
(menu-bar-mode (if window-system 1 -1))

;; turn off the toolbar
(if (>= emacs-major-version 21)
   (tool-bar-mode -1))

;; ============================
;; Edit
;; ============================

;; Turn on linum-mode
(global-linum-mode 1)

;; I use a single space between sentences.
(setq sentence-end-double-space nil)

;; turn on word wrapping in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Break lines after the 80th column.
(setq-default fill-column 80)

;; replace highlighted text with what I type rather than just
;; inserting at a point
(delete-selection-mode t)

;; resize the mini-buffer when necessary
(setq resize-minibuffer-mode t)

;; highlight during searching
(setq query-replace-highlight t)
(setq isearch-highlight t)
(setq search-highlight t)
(setq-default transient-mark-mode t)

;; kill trailing white space on save
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Jump to a matching paren
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; =================================================
;; Highlight characters ouside of the 80-char limit.
;; =================================================

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
(font-lock-add-keywords 'protobuf-mode (font-lock-width-keyword 80))

;; ============================
;; Emacs-nav.
;; ============================
(nav-disable-overeager-window-splitting)

;; ==============================================
;; Multi term.
;; ==============================================

(setq multi-term-program "/bin/bash")

;; ==============================================
;; Dedicated windows.
;; ==============================================

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
	(dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
	    (if dedicated "no longer " "")
	    (buffer-name))))

(global-set-key [pause] 'toggle-current-window-dedication)

;; ============================
;; Set up compile options.
;; ============================

;; M-x compile.
(setq compile-command "g++ -Wall -Wno-sign-compare -std=c++0x"
      compilation-ask-about-save nil)
;; Make lines in compilation mode in vertically split (split-window-right)
;; buffers wrap.
(add-hook 'compilation-mode-hook
	  (lambda () (setq truncate-partial-width-windows nil)))

;; ============================
;; Packages.
;; ============================
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; =======================================================================
;; Use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
;; =======================================================================
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; ============================
;; Rust mode.
;; ============================
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; ============================
;; Semantic.
;; ============================
;; (semantic-mode t)

;; ============================
;; Python.
;; ============================
(setq python-guess-indent nil)
(setq-default python-indent-offset 4)

;; Set this or you will get gibberish in the ipython prompt.
(setq python-shell-interpreter "ipython3" python-shell-interpreter-args
      "--simple-prompt --pprint")

(package-initialize)
(elpy-enable)
(elpy-use-ipython)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Fix elpy <-> autocomplete race condition.
(defvar elpy--ac-init-lock nil)
(defun elpy--ac-init ()
  "Initialize a completion.

This will call Python in the background and initialize
`elpy--ac-cache' when it returns."
  (when (and (not elpy--ac-init-lock)
	     (not (eq (python-syntax-context-type)
		      'comment)))
    (let ((elpy--ac-init-lock t))
      (elpy-rpc-get-completions
       (lambda (result)
	 (setq elpy--ac-cache nil)
	 (dolist (completion result)
	   (let ((name (car completion))
		 (doc (cadr completion)))
	     (when (not (string-prefix-p "_" name))
	       (push (cons (concat ac-prefix name)
			   doc)
		     elpy--ac-cache))))
	 (ac-start))
       (lambda (err)
	 (message "Can't get completions: %s" err))))))

;; ============================
;; Java.
;; ============================
(add-hook 'java-mode-hook 'eclim-mode)

;; ============================
;; Yasnippet.
;; ============================
(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
	     (split-string x "[[:blank:]]*=[[:blank:]]*" t))
	  (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
	 (args (python-split-args text))
     (nr 0)
	 (formatted-args
      (mapconcat
       (lambda (x)
	 (concat "  " (nth 0 x)
	     (if make-fields (format " ${%d:arg%d}" (incf nr) nr))
	     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
       args
       indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
	  (list "" "Args:" formatted-args)
	  indent)
       "\n"))))

(define-key yas-keymap (kbd "C-p") 'yas-prev-field)

;; ============================
;; Ediff.
;; ============================
(setq-default ediff-forward-word-function 'forward-char)

;; ============================
;; Proxy.
;; ============================
(setq url-proxy-services
      (quote (("http" . "127.0.0.1:9093")
	      ("https" . "127.0.0.1:9093")
	      ("no_proxy" . "\\(localhost\\|127.0.0.1\\|.*\\.twosigma\\.com\\)"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
