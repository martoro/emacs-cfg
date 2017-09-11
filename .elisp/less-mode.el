;;; LESS-MODE.EL - minor mode replacement for view-mode
;;; Copyright (C) 2000-2003 Robert Wyrick (rob@wyrick.org)
;;
;; SUMMARY: a replacement for view-mode that works better
;;
;; INSTALLATION INSTRUCTIONS:
;;
;; Install this file somewhere in your load path, byte-compile it and
;; add the following to your .emacs file (remove the comment
;; delimiters ;-)
;;
;;     (require 'less-mode)
;;
;; USAGE INSTRUCTIONS:
;;
;; Enter less-mode by typing M-x less-mode RET

;; I, of course, made some keybindings:

;;   (global-set-key [(meta up)] 'less-mode-on)
;;   (global-set-key [(meta down)] 'less-mode-on)
;;   (global-set-key (quote [27 up]) 'less-mode-on)
;;   (global-set-key (quote [27 down]) 'less-mode-on)

;; Once in less mode, the file is toggled to read-only and the keys
;; are remapped to the same keybindings you get when you use the 'less'
;; command line text viewing program.  See the variable 'less-mode-map'
;; for more details.
;; Press ^G, q, Q, x, X, v, V or spacebar to exit this mode
;;
;; Known bugs:
;;   none
;;
;; COMMENTARY:
;;
;; This minor mode is just a nifty replacement to view-mode which
;; works the way *I* expect it to. ;)  

;; This file is _NOT_ part of GNU Emacs, but should be.
;; I hereby release this code under the GPL.
;;
;;; LICENSE:

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; A copy of the GNU General Public License can be obtained from 
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

(defconst less-mode-version "1.0"
  "Report bugs to: Robert Wyrick <rob@wyrick.org>")

;;; CHANGE HISTORY
;;;
;;; 1.0 - Jun 20, 2003
;;; First public release - I've been using it for over 3 years now without problems.

(defvar less-mode nil
  "Whether less mode is on or not")
(make-variable-buffer-local 'less-mode)

(defvar less-overlay nil
  "Overlay for highlight of search string")
(make-variable-buffer-local 'less-overlay)

(defvar less-read-only-save nil
  "Value of buffer-read-only before entering less-mode")
(make-variable-buffer-local 'less-read-only-save)

(defvar less-search-dir 'fwd
  "The direction we're searching")
(make-variable-buffer-local 'less-search-dir)

(defvar less-half-page-size 0
  "The size of half a page")
(make-variable-buffer-local 'less-half-page-size)

(defvar less-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map [up]    'less-down)
    (define-key map [down]  'less-up)
    (define-key map "\r"    'less-up)
    (define-key map "j"     'next-line)
    (define-key map "k"     'previous-line)
    (define-key map "l"     'forward-char)
    (define-key map "h"     'backward-char)
    (define-key map ""    'less-mode-off)
    (define-key map "q"     'less-mode-off)
    (define-key map "Q"     'less-mode-off)
    (define-key map "x"     'less-mode-off)
    (define-key map "X"     'less-mode-off)
    (define-key map "v"     'less-mode-off)
    (define-key map "V"     'less-mode-off)
    (define-key map " "     'scroll-up)
    (define-key map "f"     'scroll-up)
    (define-key map "b"     'scroll-down)
    (define-key map "d"     (lambda () (interactive) (scroll-up less-half-page-size)))
    (define-key map "u"     (lambda () (interactive) (scroll-down less-half-page-size)))
    (define-key map [?\C- ] 'less-mode-off)
    (define-key map "g"     'beginning-of-buffer)
    (define-key map "G"     'end-of-buffer)
    (define-key map "<"     'beginning-of-buffer)
    (define-key map ">"     'end-of-buffer)
    (define-key map "="     'what-line)
    (define-key map "/"     'less-search-fwd)
    (define-key map "?"     'less-search-bwd)
    (define-key map "n"     (lambda () (interactive) (less-search-fwd)))
    (define-key map "N"     (lambda () (interactive) (less-search-bwd)))
    (define-key map "p"     (lambda () (interactive) (less-search-bwd)))
    (define-key map "r"     'isearch-backward)
    (define-key map "s"     'isearch-forward)
    (define-key map "m"     'point-to-register)
    (define-key map "'"     'register-to-point)
    (define-key map "-"     'negative-argument)
    (define-key map "0"     'digit-argument)
    (define-key map "1"     'digit-argument)
    (define-key map "2"     'digit-argument)
    (define-key map "3"     'digit-argument)
    (define-key map "4"     'digit-argument)
    (define-key map "5"     'digit-argument)
    (define-key map "6"     'digit-argument)
    (define-key map "7"     'digit-argument)
    (define-key map "8"     'digit-argument)
    (define-key map "9"     'digit-argument)
    map)
)

(add-minor-mode 'less-mode " Less" less-mode-map) ;; depends on add-minor-mode

(defun less-down (&optional arg)
  (interactive)
  (if (null arg)
      (setq arg (if (null current-prefix-arg)
                    1
                  current-prefix-arg)))
  (scroll-down arg)
  ;; The following line is needed (at least on XEMacs) to ensure that pressing down and
  ;; then pressing page-down will work.  Without the following line, page-up and page-down
  ;; will only scroll one-line after pressing down
  (setq scroll-previous-lines (window-height))
)
(defun less-up (&optional arg)
  (interactive)
  (if (null arg)
      (setq arg (if (null current-prefix-arg)
                    1
                  current-prefix-arg)))
  (scroll-up arg)
  ;; The following line is needed (at least on XEMacs) to ensure that pressing down and
  ;; then pressing page-down will work.  Without the following line, page-up and page-down
  ;; will only scroll one-line after pressing down
  (setq scroll-previous-lines (window-height))
)
(defun less-mode-on ()
  "Turn less-mode on"
  (interactive)
  (if (not less-mode)
      (less-mode 1))
)
(defun less-mode-off (&optional one two)
  "Turn less-mode off"
  (interactive)
  (if less-mode
      (less-mode 0))
)

(defvar less-last-search nil)
(defun less-search-fwd (&optional arg)
  "less-mode search forward"
  (interactive (list (read-string "Regex search fwd: ")))
  (setq less-search-dir 'fwd)
  (if (and (or (null arg)
               (= (length arg) 0))
           (not (null less-last-search)))
      (search-forward-regexp less-last-search)
    (search-forward-regexp arg)
    (setq less-last-search arg))
  (if less-overlay
      (cond ((fboundp 'move-overlay)
             (move-overlay less-overlay (match-beginning 0) (match-end 0)))
            ((fboundp 'detach-extent)
             (set-extent-endpoints less-overlay (match-beginning 0) (match-end 0))))
    (setq less-overlay
          (cond ((fboundp 'make-overlay)
                 (make-overlay (match-beginning 0) (match-end 0)))
                ((fboundp 'make-extent)
                 (make-extent (match-beginning 0) (match-end 0)))))
  )
  (cond ((fboundp 'overlay-put)
         (overlay-put less-overlay 'face 'highlight))
        ((fboundp 'set-extent-face)
         (set-extent-face less-overlay 'highlight)))
)
(defun less-search-bwd (&optional arg)
  "less-mode search backward"
  (interactive (list (read-string "Regex search bwd: ")))
  (setq less-search-dir 'bwd)
  (if (and (or (null arg)
               (= (length arg) 0))
           (not (null less-last-search)))
      (search-backward-regexp less-last-search)
    (search-backward-regexp arg)
    (setq less-last-search arg))
  (if less-overlay
      (cond ((fboundp 'move-overlay)
             (move-overlay less-overlay (match-beginning 0) (match-end 0)))
            ((fboundp 'detach-extent)
             (set-extent-endpoints less-overlay (match-beginning 0) (match-end 0))))
    (setq less-overlay
          (cond ((fboundp 'make-overlay)
                 (make-overlay (match-beginning 0) (match-end 0)))
                ((fboundp 'make-extent)
                 (make-extent (match-beginning 0) (match-end 0)))))
  )
  (cond ((fboundp 'overlay-put)
         (overlay-put less-overlay 'face 'highlight))
        ((fboundp 'set-extent-face)
         (set-extent-face less-overlay 'highlight)))
)

(defun less-search-again (arg)
  "Search again."
  (interactive "p")
  (if (not (null less-last-search))
      (cond ((equal less-search-dir 'fwd)
             (less-search-fwd less-last-search))
            ((equal less-search-dir 'bwd)
             (less-search-bwd less-last-search))
      )
    (message "No previous regular expression."))
)

(defun less-mode (arg)
  "Toggle less-mode (like the GNU program 'less' for Unix).
With arg, turn less-mode on iff arg is positive."
  (interactive "P")
  (setq less-mode
        (if (null arg)
            (not less-mode)
          (> (prefix-numeric-value arg) 0)))
  (if less-mode
      (progn
        ;; Always leave less-mode before changing major mode.
        (make-local-hook 'change-major-mode-hook)
        (add-hook 'change-major-mode-hook 'less-mode-off nil t)
        (setq less-read-only-save buffer-read-only
              buffer-read-only t
              less-half-page-size (/ (1- (window-height)) 2))
        (message "Less Mode On")
      )
    ;; ELSE
    (if less-overlay 
        (cond ((fboundp 'delete-overlay)
               (delete-overlay less-overlay))
              ((fboundp 'delete-extent)
               (delete-extent less-overlay))))
    (remove-hook 'change-major-mode-hook 'less-mode-off t)
    (setq buffer-read-only less-read-only-save)
    (message "Less Mode Off")
  )
  (cond ((fboundp 'force-mode-line-update)
         (force-mode-line-update))
        ((fboundp 'redraw-modeline)
         (redraw-modeline)))
)

(provide 'less-mode)
