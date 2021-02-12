;;; ein-kernel-utils.el --- Kernel-based utilities for the Emacs IPython Notebook -*- lexical-binding: t; -*-

;; Copyright (C) 2020 John Miller

;; Author: John Miller <millejoh at mac dot com>
;; Maintainer: John Miller <millejoh at mac dot com>
;; Version: "0.1"
;; Keywords: ein, python
;; URL: https://github.com/millejoh/ein-kernel-utils
;; Package-Requires: ((ein company))

;; This file is NOT part of GNU Emacs.

;; ein-kernel-utils.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-multilang.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel-utilss.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'anaphora)
(require 'ses)
(require 'pos-tip)
(require 'ein-kernel)
(require 'ein-shared-output)

(defconst ein:kernel-utils-dir (file-name-directory load-file-name)
  "Directory in which `ein*.el` files are located.")

(defun ein:kernel-language (kernel)
  (ein:$kernelspec-language (ein:$kernel-kernelspec kernel)))

(defadvice ein:kernel--handle-payload (around ein:kernel-utils--handle-payload-patch last (kernel callbacks payload))
  (cl-loop with events = (ein:$kernel-events kernel)
        for p across payload
        for text = (or (plist-get p :text) (plist-get (plist-get p :data) :text/plain))
        for source = (plist-get p :source)
        if (member source '("IPython.kernel.zmq.page.page"
                            "IPython.zmq.page.page"
                            "page"))
        do (when (not (equal (ein:trim text) ""))
             (ein:events-trigger
              events 'open_with_text.Pager (list :text text)))
        else if
        (member
         source
         '("IPython.kernel.zmq.zmqshell.ZMQInteractiveShell.set_next_input"
           "IPython.zmq.zmqshell.ZMQInteractiveShell.set_next_input"
           "set_next_input"))
        do (let ((cb (plist-get callbacks :set_next_input)))
             (when cb (ein:funcall-packed cb text)))))


;;; Support for kernel "complete_request" and "history_request" messages.

(defun ein:kernel-request-stream (kernel code func &optional args)
  "Run lisp callback FUNC with the output stream returned by Python CODE.

The first argument to the lisp function FUNC is the stream output
as a string and the rest of the argument is the optional ARGS."
  (ein:kernel-execute
   kernel
   code
   (list :output (cons (lambda (packed msg-type content _metadata)
                         (let ((func (car packed))
                               (args (cdr packed)))
                           (when (equal msg-type "stream")
                             (aif (plist-get content :text)
                                 (apply func it args)))))
                       (cons func args)))))

(defun ein:kernel-complete (kernel line cursor-pos callbacks errback)
  "Complete code at CURSOR-POS in a string LINE on KERNEL.

CURSOR-POS is the position in the string LINE, not in the buffer.

ERRBACK takes a string (error message).

When calling this method pass a CALLBACKS structure of the form:

    (:complete_reply (FUNCTION . ARGUMENT))

Call signature::

  (funcall FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `complete_reply' message.

`complete_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#complete
"
  (condition-case err
      (let* ((content (if (< (ein:$kernel-api-version kernel) 4)
                          (list
                           ;; :text ""
                           :line line
                           :cursor_pos cursor-pos)
                        (list
                         :code line
                         :cursor_pos cursor-pos)))
             (msg (ein:kernel--get-msg kernel "complete_request" content))
             (msg-id (plist-get (plist-get msg :header) :msg_id)))
        (cl-assert (ein:kernel-live-p kernel) nil "kernel not live")
        (ein:websocket-send-shell-channel kernel msg)
        (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
        msg-id)
    (error (if errback (funcall errback (error-message-string err))
             (ein:display-warning (error-message-string err) :error)))))


(cl-defun ein:kernel-history-request (kernel callbacks
                                      &key
                                      (output nil)
                                      (raw t)
                                      (hist-access-type "tail")
                                      session
                                      start
                                      stop
                                      (n 10)
                                      pattern
                                      unique)
  "Request execution history to KERNEL.

When calling this method pass a CALLBACKS structure of the form:

    (:history_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `history_reply' message.

`history_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#history

Relevant Python code:

* :py:method:`IPython.zmq.ipkernel.Kernel.history_request`
* :py:class:`IPython.core.history.HistoryAccessor`
"
  (cl-assert (ein:kernel-live-p kernel) nil "history_reply: Kernel is not active.")
  (let* ((content (list
                   :output (ein:json-any-to-bool output)
                   :raw (ein:json-any-to-bool raw)
                   :hist_access_type hist-access-type
                   :session session
                   :start start
                   :stop stop
                   :n n
                   :pattern pattern
                   :unique unique))
         (msg (ein:kernel--get-msg kernel "history_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


;;; Kernel support inside a buffer

(defvar *ein:kernel-utils-buffer-registry* (make-hash-table))

(defun ein:kernel-utils-register-buffer (buf kernel)
  (with-current-buffer buf
    (setf (gethash (buffer-name buf) *ein:kernel-utils-buffer-registry*) kernel)
    (ein:log 'debug "Registering buffer %s with kernel %s."
             (buffer-name buf)
             (ein:$kernel-kernel-id kernel))
    (add-hook 'kill-bufferhook #'(lambda ()
                                   (ein:kernel-utils-deregister-buffer buf)))))

(defun ein:kernel-utils-deregister-buffer (buf)
  (ein:log 'debug "Removing jupyter support for buffer %s with %s."
           (buffer-name buf)
           (ein:$kernel-kernel-id (gethash (buffer-name buf) *ein:kernel-utils-buffer-registry*)))
  (remhash (buffer-name buf) *ein:kernel-utils-buffer-registry*))

(defun ein:kernel-utils--find-kernel ()
  (or (ein:get-kernel) (gethash (buffer-name (current-buffer))
                                *ein:kernel-utils-buffer-registry*)))


;;; Support tooling in languages other than Python

(defvar *ein:kernel-utils-db* (make-hash-table)
  "Lookup table tool support functions for a given language. Keys
are symbols representing the language of a running
kernel (i.e. (make-symbol (ein:kernelinfo-language <kinfo>)).
Values are an plist of (TOOL-COMMAND-NAME
LANG-CODE). TOOL-COMMAND-NAME is a symbol, LANG-CODE is a string
suitable for passing to format.")


(defun ein:define-kernel-utils-command (language tool-command code)
  (aif (gethash language *ein:kernel-utils-db*)
      (plist-put it tool-command code)
    (setf (gethash language *ein:kernel-utils-db*)
          (list tool-command code))))

(defun ein:get-kernel-utils-command (language tool-command)
  (plist-get (gethash language *ein:kernel-utils-db*) tool-command))

(eval-when-compile
  (cl-defmacro ein:make-kernel-utils (language defs)
    (let ((expr (cl-loop for d in defs
                         collecting `(ein:define-kernel-utils-command ',language ',(car d) ,(cdr d)))))
      `(progn
         ,@expr)))
  (cl-defmacro ein:kernel-utils-execute-command (kernel command &key args output execute-reply (silent t))
    (let ((lang (cl-gensym))
          (cmd (cl-gensym)))
      `(let* ((,lang (intern (ein:$kernelspec-language (ein:$kernel-kernelspec ,kernel))))
              (,cmd (ein:get-kernel-utils-command ,lang ,command)))
         (if ,cmd
             (ein:kernel-execute
              ,kernel
              ,(if (listp args)
                   `(format ,cmd ,@args)
                 `(format ,cmd ,args))
              ,(cond ((and (null execute-reply) (not (null output)))
                      `(list :output (cons ,@output)
                             :execute_reply (cons #'(lambda (&rest _args)) nil)))
                     ((and (null output) (not (null execute-reply)))
                      `(list :execute_reply (cons ,@execute-reply)))
                     ((and (not (null output)) (not (null execute-reply)))
                      `(list :output (cons ,@output)
                             :execute_reply (cons ,@execute-reply)))
                     (t `(list :execute_reply (cons #'(lambda (&rest _args)) nil))))
              :silent ,silent)
           (error "ein-kernel-utils: Kernel command not defined for %s in language %s" ,command ,lang))))))


(ein:make-kernel-utils python
                   ((get-notebook-dir . "print(__import__('os').getcwd(),end='')")
                    (add-sys-path . "__import__('sys').path.append('%s')")
                    (request-tooltip . "__ein_print_object_info_for('%s')")
                    (request-help . "%s?")
                    (object-info-request . "__ein_print_object_info_for('%s')")
                    (find-source . "__ein_find_source('%s')")
                    (run-doctest . "__ein_run_docstring_examples(%s)")
                    (set-figure-size . "__ein_set_figure_size('[%s, %s]')")
                    (set-figure-dpi . "__ein_set_figure_dpi('%s')")
                    (set-figure-param . "__ein_set_matplotlib_param('%s', '%s', '%s')")
                    (get-figure-param . "__ein_get_matplotlib_params()")
                    (export . "__ein_export_nb(r'%s', '%s')")
                    (tools-file . "ein_remote_safe.py")))

(ein:make-kernel-utils hy
                   ((find-source . "(ein-find-source \"%s\")")
                    (request-tooltip . "(ein-print-object-info-for \"%s\")")
                    (object-info-request . "(ein-print-object-info-for \"%s\")")
                    (tools-file . "ein_hytools.hy")))


;;;; Core functions forgotten by time

;;; File name translation (tramp support)

;; Probably it's better to define `ein:filename-translations-get' as
;; an EIEIO method so that I don't have to re-define functions such as
;; `ein:kernel-filename-to-python' and `ein:kernel-filename-from-python'.

(defun ein:filename-translations-get (url-or-port)
  (ein:choose-setting 'ein:filename-translations url-or-port))

(defun ein:filename-to-python (url-or-port filename)
  (aif (car (ein:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ein:filename-from-python (url-or-port filename)
  (aif (cadr (ein:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ein:make-tramp-file-name (username remote-host python-filename)
  "Old (with multi-hops) tramp compatibility function.
Adapted from `slime-make-tramp-file-name'."
  (if (boundp 'tramp-multi-methods)
      (tramp-make-tramp-file-name nil nil
                                  username
                                  remote-host
                                  python-filename)
    (tramp-make-tramp-file-name nil
                                username
                                remote-host
                                python-filename)))

(defun ein:tramp-create-filename-translator (remote-host &optional username)
  "Generate a pair of TO-PYTHON and FROM-PYTHON for
`ein:filename-translations'.

Usage::

    (setq ein:filename-translations
          `((8888
             . ,(ein:tramp-create-filename-translator \"MY-HOSTNAME\"))))
    ;; Equivalently:
    (setq ein:filename-translations
          (lambda (url-or-port)
            (when (equal url-or-port 8888)
              (ein:tramp-create-filename-translator \"MY-HOSTNAME\"))))

This setting assumes that the IPython server which can be
connected using the port 8888 in localhost is actually running in
the host named MY-HOSTNAME.

Adapted from `slime-create-filename-translator'."
  (require 'tramp)
  (let ((remote-host remote-host)
        (username (or username (user-login-name))))
    (list (lambda (emacs-filename)
            (tramp-file-name-localname
             (tramp-dissect-file-name emacs-filename)))
          (lambda (python-filename)
            (ein:make-tramp-file-name username remote-host python-filename)))))


;;;; Kernel Functions Forgotten by Time

(defun ein:kernel-filename-to-python (kernel filename)
  "See: `ein:filename-to-python'."
  (ein:filename-to-python (ein:$kernel-url-or-port kernel) filename))

(defun ein:kernel-filename-from-python (kernel filename)
  "See: `ein:filename-from-python'."
  (ein:filename-from-python (ein:$kernel-url-or-port kernel) filename))



(defun ein:goto-file (filename lineno &optional other-window)
  "Jump to file FILEAME at line LINENO.
If OTHER-WINDOW is non-`nil', open the file in the other window."
  (funcall (if other-window #'find-file-other-window #'find-file) filename)
  (goto-char (point-min))
  (forward-line (1- lineno)))

(defun ein:goto-marker (marker &optional other-window)
  (funcall (if other-window #'pop-to-buffer #'switch-to-buffer)
           (marker-buffer marker))
  (goto-char marker))

(defcustom ein:propagate-connect t
  "Set to `t' to connect to the notebook after jumping to a buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

;;;###autoload
(defun ein:kernel-utils-setup-hooks (kernel)
  (add-to-list (cons #'ein:kernel-utils-load-safely kernel)
               (ein:$kernel-after-start-hook kernel)))

(defun ein:kernel-utils-wrap-hy-code (code)
  (format "__import__('hy').eval(__import__('hy').read_str('''%s'''))" code))

;;;###autoload
(defun ein:kernel-utils-load-safely (kernel)
  (with-temp-buffer
    (let* ((fname (ein:get-kernel-utils-command (intern (ein:$kernelspec-language
                                                     (ein:$kernel-kernelspec kernel)))
                                                'tools-file))
           (kernel-utils-file (format "%s/%s" ein:kernel-utils-dir fname)))
      (cl-assert (file-exists-p kernel-utils-file) nil "Cannot find kernel utils language file %s." kernel-utils-file)
      (insert-file-contents kernel-utils-file)
      (ein:kernel-execute
       kernel
       (buffer-string)
       (list
        :execute_reply (cons #'(lambda (file &rest _args)
                                 (message "ein-kernel-utils: Kernel utils loaded from %s." file))
                             kernel-utils-file))))))

;;;###autoload
(defun ein:kernel-utils-reinject ()
  "Re-send ein's kernel-utils code to the current kernel.

If the kernel is reset by the notebook server then it may become
necessary to call this command to ensure kernel-utils continue
working."
  (interactive)
  (ein:kernel-utils-load-safely (ein:get-kernel-or-error)))

(defun ein:kernel-utils-add-sys-path (kernel)
  (ein:kernel-utils-execute-command kernel 'add-sys-path :args ein:source-dir))

(defun ein:set-buffer-file-name (nb msg-type content _not-used-)
  (let ((buf (ein:notebook-buffer nb)))
    (ein:case-equal msg-type
      (("stream" "output")
       (with-current-buffer buf
         (setq buffer-file-name
               (expand-file-name
                (format "%s" (ein:$notebook-notebook-name nb))
                (plist-get content :text))))))))

(defun ein:kernel-utils-get-notebook-dir (packed)
  (cl-multiple-value-bind (kernel notebook) packed
    (ein:kernel-utils-execute-command kernel 'get-notebook-dir
                                  :output (#'ein:set-buffer-file-name notebook))))


;;; Tooltip and help

;; We can probably be more sophisticated than this, but as a hack it will do.

(defun ein:kernel-utils-magic-func-p (fstr)
  (string-prefix-p "%" fstr))

(defun ein:kernel-utils--construct-defstring (content)
  "Construct call signature from CONTENT of ``:object_info_reply``.
Used in `ein:kernel-utils-finish-tooltip', etc."
  (plist-get content :call_signature))

(defun ein:kernel-utils--construct-help-string (content)
  "Construct help string from CONTENT of ``:object_info_reply``.
Used in `ein:pytools-finish-tooltip', etc."
  (let* ((defstring (ein:aand
                     (ein:kernel-utils--construct-defstring content)
                     (ansi-color-apply it)
                     (ein:string-fill-paragraph it)))
         (docstring (ein:aand
                     (plist-get content :docstring)
                     (ansi-color-apply it)))
         (help (ein:aand
                (delete nil (list defstring docstring))
                (ein:join-str "\n" it))))
    help))

(defun ein:kernel-utils-request-tooltip (kernel func)
  (interactive (list (ein:get-kernel-or-error)
                     (ein:object-at-point-or-error)))
  (unless (ein:kernel-utils-magic-func-p func)
    (if (>= (ein:$kernel-api-version kernel) 3)
        (ein:kernel-utils-execute-command kernel 'request-tooltip
                                          :args func
                                          :output
                                          (#'(lambda (name msg-type content metadata)
                                               (ein:case-equal msg-type
                                                 (("stream" "display_data")
                                                  (ein:kernel-utils-finish-tooltip name
                                                                                   (ein:json-read-from-string
                                                                                    (plist-get content :text))
                                                                                   metadata))))
                                           func))
      (ein:kernel-object-info-request
       kernel func (list :object_info_reply
                         (cons #'ein:kernel-utils-finish-tooltip nil)
                         :execute_reply
                         (cons #'(lambda (&rest _args)) nil))))))


(defun ein:kernel-utils-finish-tooltip (_ignore content _metadata-not-used-)
  ;; See: Tooltip.prototype._show (tooltip.js)
  (let ((tooltip (ein:kernel-utils--construct-help-string content))
        (defstring (ein:kernel-utils--construct-defstring content))
        (name (plist-get content :name)))
    (if tooltip
        (pos-tip-show defstring)
      (ein:log 'info "no info for %s" name))))

(defun ein:kernel-utils-request-help (kernel func)
  (interactive (list (ein:get-kernel-or-error)
                     (ein:object-at-point-or-error)))
  (ein:kernel-utils-execute-command kernel 'request-help
                                    :args func
                                    :silent nil))

(defun ein:kernel-utils-request-tooltip-or-help (&optional pager)
  "Show the help for the object at point using tooltip.
When the prefix argument ``C-u`` is given, open the help in the
pager buffer.  You can explicitly specify the object by selecting it."
  (interactive "P")
  (call-interactively (if pager
                          #'ein:kernel-utils-request-help
                        #'ein:kernel-utils-request-tooltip)))


;;; Source jump

(defvar ein:kernel-utils-jump-stack nil)

(defvar ein:kernel-utils-jump-to-source-not-found-regexp
  (ein:join-str "\\|"
                (list "^WARNING: .*"
                      "^Traceback (most recent call last):\n"
                      "^.*<ipython-input-[^>\n]+>\n"
                      "^\n")))

(defun ein:kernel-utils-jump-to-source-1 (packed msg-type content _metadata)
  (ein:log 'debug "msg-type[[%s]] content[[%s]]" msg-type content)
  (cl-destructuring-bind (kernel object other-window notebook) packed
    (ein:log 'debug "object[[%s]] other-window[[%s]]" object other-window)
    (ein:case-equal msg-type
      (("stream" "display_data")
       (aif (or (plist-get content :text) (plist-get content :data))
           (if (string-match ein:kernel-utils-jump-to-source-not-found-regexp it)
               (ein:log 'info
                 "Jumping to the source of %s...Not found" object)
             (cl-destructuring-bind (&key filename lineno) (ein:json-read-from-string it)
               (setq filename (ein:kernel-filename-from-python kernel filename))
               (ein:log 'debug "filename[[%s]] lineno[[%s]]"
                        filename lineno)
               (if (not (file-exists-p filename))
                   (ein:log 'info
                     "Jumping to the source of %s...Not found" object)
                 (let ((ein:connect-default-notebook nil))
                   ;; Avoid auto connection to connect to the
                   ;; NOTEBOOK instead of the default one.
                   (ein:goto-file filename lineno other-window))
                 ;; Connect current buffer to NOTEBOOK. No reconnection.
                 (when (fboundp 'ein:connect-buffer-to-notebook)
                   (ein:connect-buffer-to-notebook notebook nil t))
                 (push (point-marker) ein:kernel-utils-jump-stack)
                 (ein:log 'info "Jumping to the source of %s...Done" object))))))
      (("pyerr" "error")
       (ein:log 'info "Jumping to the source of %s...Not found" object)))))

(defun ein:kernel-utils-jump-to-source (kernel object &optional
                                          other-window notebook)
  (ein:log 'info "Jumping to the source of %s..." object)
  (let ((last (car ein:kernel-utils-jump-stack)))
    (if (ein:aand last (eql (current-buffer) (marker-buffer it)))
        (unless (equal (point) (marker-position last))
          (push (point-marker) ein:kernel-utils-jump-stack))
      (setq ein:kernel-utils-jump-stack (list (point-marker)))))
  (ein:kernel-utils-execute-command kernel 'find-source :args object
                                    :output (#'ein:kernel-utils-jump-to-source-1
                                             (list kernel object other-window notebook))))


(defun ein:kernel-utils-find-source (kernel object &optional callback)
  "Find the file and line where object is defined.
This function mostly exists to support company-mode, but might be
useful for other purposes. If the definition for object can be
found and when callback isort specified, the callback will be
called with a cons of the filename and line number where object
is defined."
  (ein:kernel-utils-execute-command kernel 'find-source :args object
                                    :output (#'ein:kernel-utils-finish-find-source
                                             (list kernel object callback))))


(defun ein:kernel-utils-finish-find-source (packed msg-type content _ignored)
  (cl-destructuring-bind (kernel object callback) packed
    (if (or (string= msg-type "stream")
            (string= msg-type "display_data"))
        (aif (or (plist-get content :text) (plist-get content :data))
            (if (string-match ein:kernel-utils-jump-to-source-not-found-regexp it)
                (ein:log 'info
                  "Source of %s not found" object)
              (cl-destructuring-bind (&key filename lineno) (ein:json-read-from-string it)
                (if callback
                    (funcall callback
                             (cons (ein:kernel-filename-from-python kernel filename)
                                   (string-to-number lineno)))
                  (cons (ein:kernel-filename-from-python kernel filename)
                        lineno))))) ;; FIXME Generator?
      (ein:log 'info "Source of %s notebook found" object))))

;;;###autoload
(defun ein:kernel-utils-jump-to-source-command (&optional other-window)
  "Jump to the source code of the object at point.
When the prefix argument ``C-u`` is given, open the source code
in the other window.  You can explicitly specify the object by
selecting it."
  (interactive "P")
  (let ((kernel (ein:kernel-utils--find-kernel))
        (object (ein:object-at-point)))
    (cl-assert (ein:kernel-live-p kernel) nil "Kernel is not ready.")
    (cl-assert object nil "Object at point not found.")
    (ein:kernel-utils-jump-to-source kernel object other-window
                                     (when ein:propagate-connect
                                       (ein:get-notebook)))))

(defun ein:kernel-utils-jump-back-command (&optional other-window)
  "Go back to the point where `ein:kernel-utils-jump-to-source-command'
is executed last time.  When the prefix argument ``C-u`` is
given, open the last point in the other window."
  (interactive "P")
  (if poly-ein-mode
      (call-interactively #'xref-pop-marker-stack)
    (when (ein:aand (car ein:kernel-utils-jump-stack)
                    (equal (point) (marker-position it)))
      (setq ein:kernel-utils-jump-stack (cdr ein:kernel-utils-jump-stack)))
    (aif (car ein:kernel-utils-jump-stack)
        (ein:goto-marker it other-window)
      (ein:log 'info "Nothing on stack."))))

(define-obsolete-function-alias
  'ein:kernel-utils-eval-string-internal
  'ein:shared-output-eval-string "0.1.2")

(defun ein:kernel-utils-doctest ()
  "Do the doctest of the object at point."
  (interactive)
  (let* ((object (ein:object-at-point))
         (kernel (ein:kernel-utils--find-kernel))
         (cmd (ein:get-kernel-utils-command kernel 'run-docstring)))
    (ein:shared-output-eval-string (ein:kernel-utils--find-kernel)
                                   (format cmd object)
                                   t)))

(defun ein:kernel-utils-whos ()
  "Execute ``%whos`` magic command and popup the result."
  (interactive)
  (ein:shared-output-eval-string (ein:kernel-utils--find-kernel) "%whos" t))

(defun ein:kernel-utils-hierarchy (&optional ask)
  "Draw inheritance graph of the class at point.
hierarchymagic_ extension is needed to be installed.
You can explicitly specify the object by selecting it.

.. _hierarchymagic: https://github.com/tkf/ipython-hierarchymagic"
  (interactive "P")
  (let ((object (ein:object-at-point)))
    (when ask
      (setq object (read-from-minibuffer "class or object: " object)))
    (cl-assert (and object (not (equal object "")))
               nil "Object at point not found.")
    (ein:shared-output-eval-string (ein:kernel-utils--find-kernel) (format "%%hierarchy %s" object) t)))

(defun ein:kernel-utils-pandas-to-ses (dataframe)
  "View pandas_ DataFrame in SES_ (Simple Emacs Spreadsheet).
Open a `ses-mode' buffer and import DataFrame object into it.

SES_ is distributed with Emacs since Emacs 22, so you don't need
to install it if you are using newer Emacs.

.. _pandas: http://pandas.pydata.org
.. _SES: http://www.gnu.org/software/emacs/manual/html_node/ses/index.html"
  (interactive (list (read-from-minibuffer "pandas DataFrame "
                                           (ein:object-at-point))))
  (let ((buffer (get-buffer-create
                 (generate-new-buffer-name "*ein:ses pandas*"))))
    ;; fetch TSV (tab separated values) via stdout
    (ein:kernel-request-stream
     (ein:kernel-utils--find-kernel)
     (concat dataframe ".to_csv(__import__('sys').stdout, sep='\\t')")
     (lambda (tsv buffer)
       (with-current-buffer buffer
         (cl-flet ((y-or-n-p
                    (prompt)
                    (if (string-prefix-p "Yank will insert " prompt)
                        t
                      (error "Unexpected prompt: %s" prompt))))
           ;; Import DataFrame as TSV
           (ses-yank-tsf tsv nil))
         ;; Force SES to update (equivalent to run `post-command-hook').
         (ses-command-hook)))
     (list buffer))
    ;; Open `ses-mode' buffer
    (with-current-buffer buffer
      (ses-mode))
    (pop-to-buffer buffer)))

(defun ein:kernel-utils-export-buffer (buffer format)
  "Export contents of notebook using nbconvert_ to user-specified format
\(options will depend on the version of nbconvert available\) to a new buffer.

Currently EIN/IPython supports exporting to the following formats:

 - HTML
 - JSON (this is basically the same as opening the ipynb file in a buffer).
 - Latex
 - Markdown
 - Python
 - RST
 - Slides

.. _nbconvert: http://ipython.org/ipython-doc/stable/notebook/nbconvert.html"
  (interactive (list (read-buffer "Buffer: " (current-buffer) t)
                     (ein:completing-read "Export format: "
                                      (list "html"
                                            "json"
                                            "latex"
                                            "markdown"
                                            "python"
                                            "rst"
                                            "slides"))))
  (let* ((nb (car (ein:notebook-opened-notebooks
                   #'(lambda (nb)
                       (equal (buffer-name (ein:notebook-buffer nb))
                              buffer)))))
         (json (json-encode (ein:notebook-to-json nb)))
         (name (format "*ein %s export: %s*" format (ein:$notebook-notebook-name nb)))
         (buffer (get-buffer-create name)))
    (if (equal format "json")
        (with-current-buffer buffer
          (erase-buffer)
          (insert json)
          (json-pretty-print (point-min) (point-max)))
      (ein:kernel-request-stream
       (ein:kernel-utils--find-kernel)
       (format "__ein_export_nb(r'%s', '%s')"
               json
               format)
       (lambda (export buffer)
         (with-current-buffer buffer
           (erase-buffer)
           (insert export)))
       (list buffer)))
    (switch-to-buffer buffer)))



;;;; Helper functions for working with matplotlib

(defun ein:kernel-utils-set-figure-size (width height)
  "Set the default figure size for matplotlib figures. Works by setting `rcParams['figure.figsize']`."
  (interactive "nWidth: \nnHeight: ")
  (let ((kernel (ein:kernel-utils--find-kernel)))
    (ein:kernel-utils-execute-command kernel 'set-figure-size :args (width height)))
  )
;; (ein:shared-output-eval-string (ein:kernel-utils--find-kernel)
;;                                (format "__ein_set_figure_size('[%s, %s]')" width height)
;;                                nil)

(defun ein:kernel-utils-set-figure-dpi (dpi)
  "Set the default figure dpi for matplotlib figures. Works by setting `rcParams['figure.figsize']`."
  (interactive "nFigure DPI: ")
  (let ((kernel (ein:kernel-utils--find-kernel)))
    (ein:kernel-utils-execute-command kernel 'set-figure-dpi :args dpi)))

(defun ein:kernel-utils-set-matplotlib-parameter (param value)
  "Generically set any matplotlib parameter exposed in the matplotlib.pyplot.rcParams variable. Value is evaluated as a Python expression, so be careful of side effects."
  (interactive
   (list (completing-read "Parameter: " (ein:kernel-utils--get-matplotlib-params) nil t)
         (read-string "Value: " nil)))
  (let* ((kernel (ein:kernel-utils--find-kernel))
         (split (cl-position ?. param))
         (family (cl-subseq param 0 split))
         (setting (cl-subseq param (1+ split))))
    (ein:kernel-utils-execute-command kernel 'set-figure-param :args (family setting value))))

(defun ein:kernel-utils--get-matplotlib-params ()
  (let* ((kernel (ein:kernel-utils--find-kernel))
         (cmd (ein:get-kernel-utils-command kernel 'get-figure-param)))
    (ein:shared-output-eval-string (ein:kernel-utils--find-kernel)
                                   (format cmd)
                                   nil)
    (with-current-buffer (ein:shared-output-create-buffer)
      (ein:wait-until #'(lambda ()
                          (slot-value (slot-value *ein:shared-output* 'cell) 'outputs))
                      nil
                      5.0)
      (let ((outputs (first (slot-value (slot-value *ein:shared-output* 'cell) 'outputs))))
        (ein:json-read-from-string (plist-get outputs :text))))))

(defun ein:kernel-utils--estimate-screen-dpi ()
  (let* ((pixel-width (display-pixel-width))
         (pixel-height (display-pixel-height))
         (in-width (/ (display-mm-width) 25.4))
         (in-height (/ (display-mm-height) 25.4)))
    (values (/ pixel-width in-width) (/ pixel-height in-height))))

(defun ein:kernel-utils-matplotlib-dpi-correction ()
  "Estimate the screen dpi and set the matplotlib rc parameter 'figure.dpi' to that value. Call this command *after* importing matplotlib into your notebook, else this setting will be overwritten after the first call to `import matplotlib' Further testing is needed to see how well this works on high resolution displays."
  (interactive)
  (cl-multiple-value-bind (dpi-w dpi-h) (ein:kernel-utils--estimate-screen-dpi)
    (let ((dpi (floor (/ (+ dpi-w dpi-h) 2.0))))
      (ein:log 'info "Setting matplotlib scaling to: %s dpi" dpi)
      (ein:kernel-utils-set-figure-dpi dpi))))

(provide 'ein-kernel-utils)

;;; ein-kernel-utils.el ends here
