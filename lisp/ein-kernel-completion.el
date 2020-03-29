;;; ein-kernel-completion.el --- Kernel-based utilities for the Emacs IPython Notebook -*- lexical-binding: t; -*-

;; Copyright (C) 2020 John Miller

;; This file is NOT part of GNU Emacs.

;; ein-kernel-completion.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-kernel-completion.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel-completion.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-kernel-utils)
(require 'deferred)
(require 'company)
(require 'anaphora)

(autoload 'company-begin-backend "company")
(autoload 'company-doc-buffer "company")



(defun ein:completer-beginning (matched-text)
  (save-excursion
    (re-search-backward (concat matched-text "\\="))))

(defun ein:completer-finish-completing (args content _metadata)
  (ein:log 'debug "COMPLETER-FINISH-COMPLETING: content=%S" content)
  (let* ((beg (point))
         (delta (- (plist-get content :cursor_end)
                   (plist-get content :cursor_start)))
         (matched-text (buffer-substring beg (- beg delta)))
         (matches (plist-get content :matches))
         (completer #'ein:completer-finish-completing-default))
    (ein:log 'debug "COMPLETER-FINISH-COMPLETING: completer=%s" completer)
    (apply completer matched-text matches args)))

(defun ein:completer-finish-completing-default (matched-text matches
                                                &rest _ignore)
  (let* ((end (point))
         (beg (ein:completer-beginning matched-text))
         (word (if (and beg matches)
                   (ein:completing-read "Complete: " matches
                                    nil nil matched-text))))
    (when word
      (delete-region beg end)
      (insert word))))

(defun ein:completer-complete (kernel callbacks errback)
  "Start completion for the code at point.

   EXPAND keyword argument is supported by
   `ein:completer-finish-completing-ac'.  When it is specified,
   it overrides `ac-expand-on-auto-complete' when calling
   `auto-complete'."
  (interactive (list (ein:kernel-utils--find-kernel)
                     (list :complete_reply
                           (cons #'ein:completer-finish-completing '(:expand nil)))
                     #'ignore))
  (multiple-value-bind (code pos) (ein:get-completion-context (ein:$kernel-api-version kernel))
    (ein:log 'debug (format "EIN:COMPLETER-COMPLETE Code block: %s at position :%s" code pos))
    (ein:kernel-complete kernel
                         code ;; (thing-at-point 'line)
                         pos ;; (current-column)
                         callbacks errback)))

(defun ein:get-completion-context (api-version)
  (cond ((< api-version 5)
         (values (thing-at-point 'line) (current-column)))
        ((and (ein:kernel-utils--find-kernel) (ein:get-cell-at-point))
         (let* ((cell (ein:get-cell-at-point))
                (code (ein:cell-get-text cell))
                (beg (ein:cell-input-pos-min cell)))
           (values code (- (point) beg))))
        ((ein:kernel-utils--find-kernel)
         (values (buffer-string) (1- (point))))))

;;; Retrieving Python Object Info
(defun ein:completions--reset-oinfo-cache (kernel)
  (setf (ein:$kernel-oinfo-cache kernel) (make-hash-table :test #'equal)))

(defun ein:dev-clear-oinfo-cache (kernel)
  (interactive (list (ein:kernel-utils--find-kernel)))
  (ein:completions--reset-oinfo-cache kernel))

(defun ein:completions-get-cached (partial oinfo-cache)
  (cl-loop for candidate being the hash-keys of oinfo-cache
           when (string-prefix-p partial candidate)
           collect candidate))

(defun ein:completions--get-oinfo (objs)
  (let ((d (deferred:new #'identity))
        (kernel (ein:kernel-utils--find-kernel)))
    (ein:case-equal (ein:$kernelspec-language (ein:$kernel-kernelspec kernel))
      (("python")
       (if (ein:kernel-live-p kernel)
           (ein:kernel-execute
            kernel
            (format "__ein_generate_oinfo_data(%s, locals())" objs)
            (list
             :execute_reply (cons #'(lambda (&rest _args))
                                  nil)
             :output `(,(lambda (d* &rest args) (deferred:callback-post d* args)) . ,d)))
         (deferred:callback-post d "kernel not live"))))
    d))

(defvar ein:oinfo-chunk-size 50)

(defun ein:completions--build-oinfo-cache (objects)
  (cl-labels ((object-string (o)
                             (format "'%s'" (ein:trim o "\\s-\\|\n\\|\\.")))
              (to-ostrings (objs)
                           (s-join ", " (-map #'(lambda (x) (object-string x))
                                              objs)))
              (do-completions (ostrings kernel)
                              (deferred:$
                                (deferred:next
                                  (lambda ()
                                    (ein:completions--get-oinfo ostrings)))
                                (deferred:nextc it
                                  (lambda (output)
                                    (if (stringp output)
                                        (ein:display-warning output :error)
                                      (ein:completions--prepare-oinfo output objects kernel)))))))
    (if (< (length objects) ein:oinfo-chunk-size)
        (do-completions (format "[%s]" (to-ostrings (-non-nil objects))) (ein:kernel-utils--find-kernel))
      (dolist (chunk (-partition-all ein:oinfo-chunk-size (-non-nil objects)))
        (do-completions (format "[%s]" (to-ostrings chunk)) (ein:kernel-utils--find-kernel))))))


(defun ein:completions--prepare-oinfo (output objs kernel)
  (condition-case err
      (cl-destructuring-bind (msg-type content _) output
        (ein:case-equal msg-type
          (("stream" "display_data" "pyout" "execute_result")
           (aif (plist-get content :text)
               (let ((all-oinfo (ein:json-read-from-string it)))
                 (cl-loop for oinfo in all-oinfo
                          for obj in objs
                          doing (unless (string= (plist-get oinfo :string_form) "None")
                                  (setf (gethash obj (ein:$kernel-oinfo-cache kernel))
                                        oinfo))))))
          (("error" "pyerr")
           (ein:log 'verbose "ein:completions--prepare-oinfo: %s"
                    (plist-get content :traceback)))))
    (error
     (ein:log 'verbose "ein:completions--prepare-oinfo: [%s]"
              (error-message-string err))
     (let (eval-expression-print-length eval-expression-print-level)
       (prin1 output #'external-debugging-output)))))

;;; Support for Eldoc

(defun ein:completer--get-eldoc-signature ()
  (ein:and-let* ((func (ein:function-at-point))
                 (kernel (ein:kernel-utils--find-kernel)))
    (aif (gethash func (ein:$kernel-oinfo-cache kernel))
        (ein:kernel-construct-defstring it)
      (ein:completions--build-oinfo-cache (list func))
      nil)))



(defun ein:company--deferred-complete ()
  (let ((d (deferred:new #'identity)))
    (ein:completer-complete
     (ein:kernel-utils--find-kernel)
     (list :complete_reply
           (cons (lambda (d* &rest args) (deferred:callback-post d* args))
                 d))
     (apply-partially (lambda (d* err) (deferred:callback-post d* err)) d))
    d))

(defun ein:company--complete (prefix fetcher)
  (deferred:$
    (deferred:next
      (lambda ()
        (ein:company--deferred-complete)))
    (deferred:nextc it
      (lambda (replies)
        (unless (stringp replies) ;; if not an error
          (ein:completions--prepare-matches prefix fetcher replies))))))

(defun ein:completions--prepare-matches (prefix fetcher replies)
  (cl-destructuring-bind
      ((&key matches cursor_start cursor_end &allow-other-keys) ; :complete_reply
       _metadata)
      replies
    (let ((nix (- cursor_end cursor_start))
          prefixed-matches)
      (dolist (match matches)
        (setq prefixed-matches
              (nconc prefixed-matches (list (concat prefix (substring match nix))))))
      (ein:completions--build-oinfo-cache prefixed-matches)
      (funcall fetcher prefixed-matches))))



;;;###autoload
(defun ein:company-backend (command &optional arg &rest _)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ein:company-backend))
    (prefix (and (ein:kernel-utils--find-kernel)
                 (ein:object-prefix-at-point)))
    (annotation (let ((kernel (ein:kernel-utils--find-kernel)))
                  (aif (gethash arg (ein:$kernel-oinfo-cache kernel))
                      (plist-get it :definition))))
    (doc-buffer (cons :async
                      (lambda (cb)
                        (ein:company-handle-doc-buffer arg cb))))
    (location (cons :async
                    (lambda (cb)
                      (ein:kernel-utils-find-source (ein:kernel-utils--find-kernel)
                                                    arg
                                                    cb))))
    (candidates
     (let* ((kernel (ein:kernel-utils--find-kernel))
            (cached (ein:completions-get-cached arg (ein:$kernel-oinfo-cache kernel))))
       (aif cached it
         (unless (ein:company--punctuation-check (thing-at-point 'line)
                                                 (current-column))
           (cons :async
                 (lambda (cb)
                   (ein:company--complete arg cb)))))))))

(defun ein:company--punctuation-check (thing col)
  (or (string-match "[[:nonascii:]]" thing)
      (let ((query (ein:trim-right (cl-subseq thing 0 col) "[\n]")))
        (string-match "[]()\",[{}'=: ]$" query (- col 2)))))


(defun ein:company-handle-doc-buffer-finish (packed content _metadata-not-used_)
  (when (plist-get content :found)
    (funcall (plist-get packed :callback) (company-doc-buffer
                                           (ansi-color-apply (cadr (plist-get content :data)))))))

(defun ein:company-handle-doc-buffer (object cb)
  (ein:kernel-object-info-request (ein:kernel-utils--find-kernel)
                                  object
                                  (list :inspect_reply
                                        (cons #'ein:company-handle-doc-buffer-finish
                                              (list :object object
                                                    :callback cb)))))

;;;###autoload
(defun ein:enable-company-kernel-completion ()
  (interactive)
  (add-to-list 'company-backends #'ein:company-backend))

(provide 'ein-kernel-completion)
