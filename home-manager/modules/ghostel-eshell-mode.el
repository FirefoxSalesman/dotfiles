;;; ghostel-eshell-mode.el --- Turn Eshell into a ghostel terminal emulator -*- lexical-binding: t; -*-

;; Author: HK-47
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ghostel-eshell-mode integrates ghostel's PTY-based terminal emulation with
;; Eshell.  When enabled:
;;
;;   * Visual commands (vim, htop, less, ...) run in ghostel buffers via
;;     `eshell-exec-visual` override.
;;   * Eshell output is filtered through ghostel's VT parser for proper
;;     ANSI/SGR color and OSC 8 hyperlink handling.
;;   * Working directory is synced to ghostel via OSC 7 handling.
;;
;; This is inspired by eat-eshell-mode but built on ghostel's primitives
;; (ghostel-exec, ghostel--comint-filter, ghostel--update-directory).
;;
;; NOTE: ghostel-eshell-mode and ghostel-eshell-visual-command-mode both
;; override `eshell-exec-visual`.  Enable only one of them at a time.

;;; Code:

(require 'ghostel)
(require 'esh-ext)
(require 'esh-util)

(eval-when-compile
  (require 'cl-lib))

;;; Variables declared (Eshell may not be loaded yet)

(defvar eshell-exec-visual)
(defvar eshell-mode-hook)
(defvar eshell-directory-change-hook)
(defvar eshell-process-list)
(defvar eshell-preoutput-filter-functions)
(defvar eshell-visual-command-p)
(defvar eshell-visual-commands)
(defvar eshell-interactive-output-p)
(defvar eshell-destroy-buffer-when-process-dies)
(defvar eshell-last-async-proc)
(defvar eshell-last-async-procs)

(declare-function eshell-exec-visual "em-term" (&rest args))
(declare-function eshell-find-interpreter "esh-ext" (file args &optional no-examine-p))
(declare-function eshell-stringify-list "esh-util" (args))

(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ghostel--comint-make-state "ghostel-module" ())
(declare-function ghostel--comint-filter "ghostel-module" (state string))
(declare-function ghostel--comint-set-palette "ghostel-module" (state))
(declare-function ghostel--update-directory "ghostel" (uri))

;;; User options

(defcustom ghostel-eshell-mode-handle-visual-commands t
  "Whether ghostel-eshell-mode should handle visual commands via ghostel PTY.
When non-nil, `eshell-exec-visual` is overridden to dispatch visual
commands (vim, htop, less, ...) through `ghostel-exec`."
  :type 'boolean
  :group 'ghostel)

(defcustom ghostel-eshell-mode-handle-ansi-colors t
  "Whether to route Eshell output through ghostel's VT parser.
When non-nil, Eshell output is passed through `ghostel--comint-filter`
for proper ANSI/SGR color and OSC 8 hyperlink handling."
  :type 'boolean
  :group 'ghostel)

(defcustom ghostel-eshell-mode-track-cwd t
  "Whether to sync Eshell's working directory to ghostel via OSC 7."
  :type 'boolean
  :group 'ghostel)

;;; Buffer-local state

(defvar ghostel--eshell-local-mode nil
  "Non-nil if ghostel-eshell integration is active in the current buffer.")
(make-variable-buffer-local 'ghostel--eshell-local-mode)

(defvar ghostel--eshell-vt-state nil
  "Opaque VT parser state for ghostel's ANSI/SGR output filtering.")
(make-variable-buffer-local 'ghostel--eshell-vt-state)

;;; Global minor mode

;;;###autoload
(define-minor-mode ghostel-eshell-mode
  "Turn Eshell into a ghostel terminal emulator.

When enabled, Eshell's external process execution is integrated with ghostel's
PTY-based terminal emulation:

  * Visual commands run in ghostel buffers (full terminal emulation).
  * Output is filtered through ghostel's VT parser (ANSI/SGR colors,
    OSC 8 hyperlinks).
  * Working directory is synced via OSC 7.

Enable globally with `(ghostel-eshell-mode 1)` or add to your Eshell setup:

    (add-hook 'eshell-load-hook #'ghostel-eshell-mode)"
  :global t
  :group 'ghostel
  (if ghostel-eshell-mode
      (progn
        (add-hook 'eshell-mode-hook #'ghostel--eshell-local-mode-enable)
        (when ghostel-eshell-mode-track-cwd
          (add-hook 'eshell-directory-change-hook #'ghostel--eshell-update-cwd))
        (when ghostel-eshell-mode-handle-visual-commands
          (advice-add #'eshell-exec-visual :override
                      #'ghostel--eshell-exec-visual))
        (when ghostel-eshell-mode-handle-ansi-colors
          (add-hook 'eshell-preoutput-filter-functions #'ghostel--eshell-filter-output nil t))
        ;; Enable in existing Eshell buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (eq major-mode 'eshell-mode)
              (ghostel--eshell-local-mode-enable)))))
    (remove-hook 'eshell-mode-hook #'ghostel--eshell-local-mode-enable)
    (when ghostel-eshell-mode-track-cwd
      (remove-hook 'eshell-directory-change-hook #'ghostel--eshell-update-cwd))
    (when ghostel-eshell-mode-handle-visual-commands
      (advice-remove #'eshell-exec-visual #'ghostel--eshell-exec-visual))
    (when ghostel-eshell-mode-handle-ansi-colors
      (remove-hook 'eshell-preoutput-filter-functions #'ghostel--eshell-filter-output t))
    ;; Disable in existing Eshell buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'eshell-mode)
          (ghostel--eshell-local-mode-disable))))))

(defun ghostel--eshell-local-mode-enable ()
  "Enable ghostel-eshell integration in the current Eshell buffer."
  (setq ghostel--eshell-local-mode t)
  (when ghostel-eshell-mode-track-cwd
    (ghostel--eshell-update-cwd)))

(defun ghostel--eshell-local-mode-disable ()
  "Disable ghostel-eshell integration in the current Eshell buffer."
  (setq ghostel--eshell-local-mode nil)
  (setq ghostel--eshell-vt-state nil))

;;; Output filtering

(defun ghostel--eshell-filter-output (string)
  "Filter Eshell output STRING through ghostel's VT parser.
Returns the filtered string with text properties for colors/links."
  (when (and ghostel--eshell-local-mode
             ghostel-eshell-mode-handle-ansi-colors
             (featurep 'ghostel-module)
             (fboundp 'ghostel--comint-make-state)
             (fboundp 'ghostel--comint-filter))
    (let ((state (or ghostel--eshell-vt-state
                     (setq ghostel--eshell-vt-state
                           (ghostel--comint-make-state)))))
      (when state
        (ghostel--comint-set-palette state)
        (setq string (ghostel--comint-filter state string)))))
  string)

;;; Working directory sync

(defun ghostel--eshell-update-cwd ()
  "Sync the current Eshell buffer's working directory to ghostel via OSC 7."
  (when (and ghostel--eshell-local-mode
             ghostel-eshell-mode-track-cwd
             (boundp 'default-directory))
    (ghostel--update-directory (expand-file-name default-directory))))

;;; Visual command override

(defun ghostel--eshell-exec-visual (&rest args)
  "Replacement for `eshell-exec-visual' that dispatches through ghostel.

ARGS are the program name followed by its arguments, as passed by eshell.
Runs the program in a dedicated ghostel terminal buffer."
  (require 'esh-ext)
  (require 'esh-util)
  (save-current-buffer
    (let* ((eshell-interpreter-alist nil)
           (interp (eshell-find-interpreter (car args) (cdr args)))
           (program (car interp))
           (prog-args (flatten-tree
                       (eshell-stringify-list
                        (append (cdr interp) (cdr args)))))
           (buf (generate-new-buffer
                 (concat "*" (file-name-nondirectory program) "*")))
           (kill-on-exit (bound-and-true-p eshell-destroy-buffer-when-process-dies)))
      (switch-to-buffer buf)
      (let ((ghostel-kill-buffer-on-exit kill-on-exit))
        (ghostel-exec buf program prog-args
                      `((kind . eshell-visual)
                        (command . (,program . ,prog-args)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local ghostel-kill-buffer-on-exit kill-on-exit)
          (add-hook 'ghostel-exit-functions
                    #'ghostel--eshell-visual-exit nil t)
          (unless (process-live-p ghostel--process)
            (ghostel--eshell-visual-exit buf nil))))
      nil)))

(defun ghostel--eshell-visual-exit (buffer _event)
  "Post-exit cleanup for a ghostel visual-command BUFFER.

Snaps point to point-max, binds `q' to kill the buffer, and moves
all windows showing BUFFER to point-max."
  (run-at-time 0 nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (goto-char (point-max))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map (current-local-map))
                       (keymap-set map "q" #'kill-current-buffer)
                       (use-local-map map))
                     (dolist (win (get-buffer-window-list buffer nil t))
                       (set-window-point win (point-max))))))))

(provide 'ghostel-eshell-mode)

;;; ghostel-eshell-mode.el ends here
