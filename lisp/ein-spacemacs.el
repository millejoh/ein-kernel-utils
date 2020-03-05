(use-package ein
  :defer 60
  :commands (ein:run ein:login)
  :init
  (spacemacs/set-leader-keys
    "ayl" 'ein:login
    "ayr" 'ein:run
    "ayd" 'ein:stop)
  (spacemacs/declare-prefix "ay" "ipython notebook")
  :config
  (mapc
   (lambda (mode)
     (evil-define-minor-mode-key
       mode 'ein:notebook-mode
       (kbd "<C-return>") 'ein:worksheet-execute-cell-km
       (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km))
   '(insert hybrid normal))
  (with-eval-after-load 'ein-notebook
      (evil-define-key nil ein:notebooklist-mode-map "o" 'spacemacs/ace-buffer-links)
      (let ((bindings '(("j" ein:worksheet-goto-next-input-km)
                        ("k" ein:worksheet-goto-prev-input-km)
                        ("J" ein:worksheet-move-cell-down-km)
                        ("K" ein:worksheet-move-cell-up-km)
                        ("e" ein:worksheet-toggle-output-km)
                        ("d" ein:worksheet-kill-cell-km)
                        ("y" ein:worksheet-copy-cell-km)
                        ("p" ein:worksheet-yank-cell-km)
                        ("m" ein:worksheet-merge-cell-km)
                        ("s" ein:worksheet-split-cell-at-point-km)
                        ("o" ein:worksheet-insert-cell-below-km)
                        ("O" ein:worksheet-insert-cell-above-km)
                        ("t" ein:worksheet-toggle-cell-type-km)
                        ("C-m" ein:worksheet-execute-cell-km)
                        ("l" ein:worksheet-clear-output-km)
                        ("L" ein:worksheet-clear-all-output-km)
                        ("C-s" ein:notebook-save-notebook-command-km)
                        ("C-r" ein:notebook-rename-command-km)
                        ("x" ein:notebook-close-km)
                        ("z" ein:notebook-kernel-interrupt-command-km))))
        (apply #'spacemacs/set-leader-keys-for-minor-mode
               (quote ein:notebook-mode)
               (cl-mapcan
                (lambda (bind)
                  (if (fboundp (cl-second bind))
                      bind
                    (prog1 nil
                      (display-warning
                       'warn (format "ipython-notebook/init-ein: undefined %s"
                                     (cl-second bind))))))
                (copy-tree bindings)))
        (eval (append '(spacemacs|define-transient-state
                        ipython-notebook
                        :title "iPython Notebook Transient State"
                        :bindings
                        ("q" nil :exit t))
                      bindings)))))

(provide 'ein-spacemacs)
;; #|
;; :doc "
;;  Operations on Cells^^^^^^            Other
;;  ----------------------------^^^^^^   ----------------------------------^^^^
;;  [_K_/_J_]^^     move up/down         [_C-l_/_C-S-l_] clear/clear all output
;;  [_C-k_/_C-j_]^^ merge above/below    [_C-o_]^^       open console
;;  [_O_/_o_]^^     insert above/below   [_C-s_/_C-r_]   save/rename notebook
;;  [_y_/_p_/_d_]   copy/paste           [_x_]^^         close notebook
;;  [_u_]^^^^       change type          [_q_]^^         quit transient-state
;;  [_RET_]^^^^     execute"
;; |#
