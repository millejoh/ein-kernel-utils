;;; ein-buffer-connect.el --- Connect external buffers to IPython   -*- lexical-binding: t -*-

;; Copyright (C) 2020 - John Miller

;; Author: John M. Miller

;; This file is NOT part of GNU Emacs.

;; ein-buffer-connect.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-buffer-connect.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-buffer-connect.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: There is a problem when connected notebook is closed.
;;        This can be fixed in some ways:
;; * Turn off ein:connect when the command that uses kernel is invoked
;;   but corresponding notebook was closed already.
;; * Connect directly to ein:kernel and make its destructor to care
;;   about connecting buffers.

;;; Code:

(require 'eieio)
(require 'anaphora)
(require 'ein-kernel-utils)
(require 'ein-events)
(require 'ein-traceback)

(defun ein:maybe-save-buffer (option)
  "Conditionally save current buffer.
Return `t' if the buffer is unmodified or `nil' otherwise.
If the buffer is modified, buffer is saved depending on the value
of OPTION:
  ask  : Ask whether the buffer should be saved.
  yes  : Save buffer always.
  no   : Do not save buffer."
  (if (not (buffer-modified-p))
      t
    (cl-case option
      (ask (when (y-or-n-p "Save buffer? ")
             (save-buffer)
             t))
      (yes (save-buffer)
           t)
      (t nil))))


;;; Configuration

(defcustom ein:connect-save-before-run 'yes
  "Whether the buffer should be saved before `ein:connect-run-buffer'."
  :type '(choice (const :tag "Always save buffer" yes)
                 (const :tag "Always do not save buffer" no)
                 (const :tag "Ask" ask))
  :group 'ein)

(defcustom ein:connect-default-notebook nil
  "Notebook to be connect when `ein:connect-to-default-notebook' is called.

Example setting to connect to \"My_Notebook\" in the server at
port 8888 when opening any buffer in `python-mode'::

  (setq ein:connect-default-notebook \"8888/My_Notebook\")
  (add-hook 'python-mode-hook 'ein:connect-to-default-notebook)

`ein:connect-default-notebook' can also be a function without any
argument.  This function must return a string (notebook path of
the form \"URL-OR-PORT/NOTEBOOK-NAME\").

As `ein:connect-to-default-notebook' requires notebook list to be
loaded, consider using `ein:notebooklist-load' to load notebook
list if you want to connect to notebook without manually opening
notebook list."
  :type '(choice (string :tag "URL-OR-PORT/NOTEBOOK-NAME")
                 (function :tag "Notebook path getter"))
  :group 'ein)

(defcustom ein:on-buffer-connect (list 'ein:enable-company-kernel-completion)
  "Normal hook for functions to execute after connecting a buffer to a running notebook."
  :type 'list
  :group 'ein)


;;; Class

(ein:deflocal ein:%buffer-kernel% nil
  "Buffer local variable to store an instance of `ein:buffer-connection'")


(defclass ein:buffer-connection ()
  ((notebook :initarg :notebook :accessor ein:buffer-notebook :type ein:$notebook)
   (nb-buffer :initarg :nb-buffer :type buffer)
   (buffer :initarg :buffer :type buffer)))

(defun ein:new-buffer-kernel (notebook buffer)
  (let ((kernel (ein:$notebook-kernel notebook)))
    (cl-assert (ein:kernel-live-p kernel) nil
               "Warning: Trying to connect to a disconnected kernel. Please verify the status of %s"
               (ein:notebook-buffer notebook))
    (ein:events-on (ein:$kernel-events kernel) 'status_disconnected.Kernel
                   #'(lambda (buffer _ignore)
                       (message "Disconnecting ein kernel connection for buffer %s." buffer)
                       (with-current-buffer buffer
                         (setq ein:%buffer-kernel% nil))))
    (ein:buffer-connection :notebook notebook
                           :nb-buffer (ein:notebook-buffer notebook)
                           :buffer buffer)))


;;; Methods

;;;###autoload
(defun ein:connect-to-notebook (&optional not-yet-opened)
  "Connect to notebook.  When the prefix argument is given,
you can choose any notebook on your server including the ones
not yet opened.  Otherwise, already chose from already opened
notebooks."
  (interactive "P")
  (call-interactively (if not-yet-opened
                          #'ein:connect-to-any-notebook
                        #'ein:connect-to-open-notebook)))

;;;###autoload
(defun ein:connect-to-any-notebook (nbpath &optional buffer no-reconnection)
  "Connect current buffer to any notebook available on the
jupyter server. If notebook is not already opened, ein will open
the notebook and create a session for the associated kernel."
  (interactive (list (ein:notebooklist-ask-path "notebook")))
  (cl-multiple-value-bind (url-or-port path) (ein:notebooklist-parse-nbpath nbpath)
    (ein:notebook-open url-or-port path nil
                       (apply-partially
                        (lambda (buffer* no-reconnection* notebook _created)
                          (ein:connect-buffer-to-notebook notebook buffer* no-reconnection*))
                        (or buffer (current-buffer)) no-reconnection))))

;;;###autoload
(defun ein:connect-to-open-notebook (buffer-or-name)
  "Connect buffer to a notebook and its associated kernel. If not
supplied, command will prompt user to select from already opened
notebooks."
  (interactive (list (ein:completing-read "Notebook buffer to connect: "
                                      (ein:notebook-opened-buffer-names))))
  (aif (get-buffer buffer-or-name)
      (let ((notebook (buffer-local-value 'ein:%notebook% it)))
        (ein:connect-buffer-to-notebook notebook))
    (error "No buffer %s" buffer-or-name)))

;;;###autoload
(defun ein:connect-buffer-to-notebook (notebook &optional buffer
                                                no-reconnection)
  "Connect BUFFER to NOTEBOOK."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (if (or (not no-reconnection)
            (not ein:%buffer-kernel%))
        (let ((connection (ein:new-buffer-kernel notebook buffer)))
          (setq ein:%buffer-kernel% connection)
          (run-hooks 'ein:on-buffer-connect)
          (ein:log 'info "Connected to %s"
                   (ein:$notebook-notebook-name notebook))
          connection)
      (ein:log 'info "Buffer is already connected to notebook."))))

(defun ein:connect-get-notebook ()
  (ein:buffer-notebook ein:%buffer-kernel%))

(defun ein:connect-get-kernel ()
  (ein:$notebook-kernel (ein:connect-get-notebook)))


(defun ein:connect-eval-buffer ()
  "Evaluate the whole buffer.  Note that this will run the code
inside the ``if __name__ == \"__main__\":`` block."
  (interactive)
  (let ((b (current-buffer)))
    (deferred:$
      (deferred:next
        (lambda ()
          (with-current-buffer b
            (ein:shared-output-eval-string (ein:connect-get-kernel) (buffer-string) :silent t))))))
  (ein:log 'info "Whole buffer is sent to the kernel."))

(defun ein:connect-run-buffer (&optional ask-command)
  "Run buffer using ``%run``.  Ask for command if the prefix ``C-u`` is given.
Variable `ein:connect-run-command' sets the default command."
  (interactive "P")
  (aif (buffer-file-name)
      (let* ((default-command "%run")
             (command (if ask-command
                          (read-from-minibuffer "Command: " default-command)
                        default-command))
             (cmd (format "%s \"%s\"" command it)))
        (ein:connect-execute-command cmd))
    (error (concat "This buffer has no associated file.  "
                   "Use `ein:connect-eval-buffer' instead."))))

(defun ein:connect-execute-command (command)
  (if (ein:maybe-save-buffer ein:connect-save-before-run)
      (progn
        (ein:shared-output-eval-string (ein:connect-get-kernel) command :silent t)
        (ein:log 'info "Command sent to the kernel: %s" command)
        (aand (ein:shared-output-get-cell)
              (ein:cell-get-tb-data it)
              (ein:tb-popup (ein:tb-new (format "*ein:tb %s" (buffer-name)) (ein:connect-get-notebook))
                            it)))
    (ein:log 'info "Buffer must be saved before %%run.")))

(defun ein:connect-run-or-eval-buffer (&optional eval)
  "Run buffer using the ``%run`` magic command or eval whole
buffer if the
Variable `ein:connect-run-command' sets the command to run.
You can change the command and/or set the options.
See also: `ein:connect-run-buffer', `ein:connect-eval-buffer'."
  (interactive "P")
  (if eval
      (ein:connect-eval-buffer)
    (ein:connect-run-buffer)))

(defun ein:connect-reload-buffer ()
  "Reload buffer using the command set by `ein:connect-reload-command'."
  (interactive)
  (aif (buffer-file-name)
      (let ((command (format "%s \"%s\"" "%run -n" it)))
        (ein:connect-execute-command command))))

(defun ein:connect-eval-region (start end)
  (interactive "r")
  (ein:shared-output-eval-string (ein:connect-get-kernel) (buffer-substring start end) nil)
  (ein:log 'info "Selected region is sent to the kernel."))

(defun ein:connect-pop-to-notebook ()
  (interactive)
  (ein:connect-assert-connected)
  (pop-to-buffer (slot-value ein:%buffer-kernel% 'nb-buffer)))



(defun ein:connect-assert-connected ()
  (cl-assert (not (null ein:%buffer-kernel%)) nil
             "Current buffer (%s) is not connected to IPython notebook."
             (buffer-name))
  (cl-assert (ein:kernel-live-p (slot-value ein:%buffer-kernel% 'kernel)) nil
             "Connected notebook kernel is not live."))


;;; Auto-connect

;;;###autoload
(defun ein:connect-to-default-notebook ()
  "Connect to the default notebook specified by
`ein:connect-default-notebook'.  Set this to `python-mode-hook'
to automatically connect any python-mode buffer to the
notebook."
  (ein:log 'verbose "CONNECT-TO-DEFAULT-NOTEBOOK")
  (ein:and-let* ((nbpath ein:connect-default-notebook)
                 ((not (ein:worksheet-buffer-p))))
    (when (functionp nbpath)
      (setq nbpath (funcall nbpath)))
    (ein:connect-to-any-notebook nbpath nil t)))



;;; ein:connect-mode

;; (defvar ein:connect-mode-map (make-sparse-keymap))

;; (let ((map ein:connect-mode-map))
;;   (define-key map "\C-c\C-c" 'ein:connect-run-or-eval-buffer)
;;   (define-key map "\C-c\C-l" 'ein:connect-reload-buffer)
;;   (define-key map "\C-c\C-r" 'ein:connect-eval-region)
;;   (define-key map (kbd "C-:") 'ein:shared-output-eval-string)
;;   (define-key map "\C-c\C-z" 'ein:connect-pop-to-notebook)
;;   (define-key map "\C-c\C-x" 'ein:tb-show)
;;   (define-key map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)

;;   map)

;; (defun ein:connect-mode-get-lighter ()
;;   " ein:c")

;; (define-minor-mode ein:connect-mode
;;   "Minor mode for communicating with IPython notebook.

;; \\{ein:connect-mode-map}"
;;   :lighter (:eval (ein:connect-mode-get-lighter))
;;   :keymap ein:connect-mode-map
;;   :group 'ein)

;; (put 'ein:connect-mode 'permanent-local t)


(provide 'ein-buffer-connect)

;;; ein-buffer-connect.el ends here
