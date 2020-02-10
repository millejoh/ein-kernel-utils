(use-package ein
  :defer 60
  :commands (ein:run ein:login)
  ;; :load-path "/Users/E341194/Documents/github/emacs-ipython-notebook/lisp"
  :init
  (progn
    (spacemacs/set-leader-keys
      "ayl" 'ein:login
      "ayr" 'ein:run
      "ayd" 'ein:stop)
    )
  :config
  (progn
    (require 'ein-jupyter)
    (require 'ob-ein)
    (require 'ein-dev)

    (with-eval-after-load 'ein-notebooklist
      (evilified-state-evilify-map ein:notebooklist-mode-map
        :mode ein:notebooklist-mode
        :bindings
        (kbd "o") 'spacemacs/ace-buffer-links)
      (define-key ein:notebooklist-mode-map "o" 'spacemacs/ace-buffer-links))

    (global-set-key "\C-c\C-j" 'ein:notebook-jump-to-opened-notebook)


    (defun spacemacs/ein:worksheet-merge-cell-next ()
      (interactive)
      (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

    (defun spacemacs//concat-leader (key)
      (if dotspacemacs-major-mode-leader-key
          (concat dotspacemacs-major-mode-leader-key key)
        (concat "," key)))

    (spacemacs/set-leader-keys-for-minor-mode 'ein:notebook
      "y" 'ein:worksheet-copy-cell
      "p" 'ein:worksheet-yank-cell
      "d" 'ein:worksheet-kill-cell
      "i" 'ein:worksheet-insert-cell-below-km
      "I" 'ein:worksheet-insert-cell-above-km
      "j" 'ein:worksheet-goto-next-input-km
      "k" 'ein:worksheet-goto-prev-input-km
      "J" 'ein:worksheet-move-cell-down-km
      "K" 'ein:worksheet-move-cell-up-km
      "t" 'ein:worksheet-toggle-output
      "R" 'ein:worksheet-rename-sheet
      "RET" 'ein:worksheet-execute-cell-and-goto-next-km
      ;; Output
      "C-l" 'ein:worksheet-clear-output
      "C-S-l" 'ein:worksheet-clear-all-output
      ;;Console
      "C-o" 'ein:console-open
      ;; Merge cells
      "C-k" 'ein:worksheet-merge-cell
      "C-j" 'spacemacs/ein:worksheet-merge-cell-next
      "s" 'ein:worksheet-split-cell-at-point
      ;; Notebook
      "C-s" 'ein:notebook-save-notebook-command
      "C-r" 'ein:notebook-rename-command
      "x" 'ein:notebook-close
      "u" 'ein:worksheet-change-cell-type
      "fs" 'ein:notebook-save-notebook-command)

    ;; keybindings for ipython notebook traceback mode
    (spacemacs/set-leader-keys-for-major-mode 'ein:traceback-mode
      "RET" 'ein:tb-jump-to-source-at-point-command
      "n" 'ein:tb-next-item
      "p" 'ein:tb-prev-item
      "q" 'bury-buffer)

    ;; keybindings mirror ipython web interface behavior
    (evil-define-key 'insert ein:notebook-mode-map
      (kbd "<C-return>") 'ein:worksheet-execute-cell
      (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km)

    ;; keybindings mirror ipython web interface behavior
    (evil-define-key 'hybrid ein:notebook-mode-map
      (kbd "<C-return>") 'ein:worksheet-execute-cell
      (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km)

    (evil-define-key 'normal ein:notebook-mode-map
      ;; keybindings mirror ipython web interface behavior
      (kbd "<C-return>") 'ein:worksheet-execute-cell
      (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km
      "gj" 'ein:worksheet-goto-next-input-km
      "gk" 'ein:worksheet-goto-prev-input-km)

    ;; if this is not required then the following keygindings fail
    ;; (require 'ein-multilang)
    (define-key ein:notebook-mode-map (kbd "M-j") 'ein:worksheet-move-cell-down-km)
    (define-key ein:notebook-mode-map (kbd "M-k") 'ein:worksheet-move-cell-up-km)

    (spacemacs|define-transient-state ein-devel
      :title "iPython Notebook Transient State"
      :doc "
 Operations on Cells^^^^^^            Other
 ----------------------------^^^^^^   ----------------------------------^^^^
 [_K_/_J_]^^     move up/down         [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^^ merge above/below    [_C-o_]^^       open console
 [_O_/_o_]^^     insert above/below   [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_]   copy/paste           [_x_]^^         close notebook
 [_u_]^^^^       change type          [_q_]^^         quit transient-state
 [_RET_]^^^^     execute"
      :evil-leader-for-mode (ein:notebook . ".")
      :bindings
      ("q" nil :exit t)
      ("?" spacemacs//ein-devel-ms-toggle-doc)
      ("h" ein:notebook-worksheet-open-prev-or-last)
      ("j" ein:worksheet-goto-next-input-km)
      ("k" ein:worksheet-goto-prev-input-km)
      ;; ("l" ein:notebook-worksheet-open-next-or-first)
      ;; ("H" ein:notebook-worksheet-move-prev)
      ("J" ein:worksheet-move-cell-down-km)
      ("K" ein:worksheet-move-cell-up-km)
      ;; ("L" ein:notebook-worksheet-move-next)
      ("t" ein:worksheet-toggle-output)
      ("d" ein:worksheet-kill-cell)
      ("R" ein:worksheet-rename-sheet)
      ("y" ein:worksheet-copy-cell)
      ("p" ein:worksheet-yank-cell)
      ("o" ein:worksheet-insert-cell-below)
      ("O" ein:worksheet-insert-cell-above)
      ("u" ein:worksheet-change-cell-type)
      ("RET" ein:worksheet-execute-cell-and-goto-next)
      ;; Output
      ("C-l" ein:worksheet-clear-output)
      ("C-S-l" ein:worksheet-clear-all-output)
      ;;Console
      ("C-o" ein:console-open)
      ;; Merge and split cells
      ("C-k" ein:worksheet-merge-cell)
      ("C-j" spacemacs/ein:worksheet-merge-cell-next)
      ("s" ein:worksheet-split-cell-at-point)
      ;; Notebook
      ("C-s" ein:notebook-save-notebook-command)
      ("C-r" ein:notebook-rename-command)
      ;; ("1" ein:notebook-worksheet-open-1th)
      ;; ("2" ein:notebook-worksheet-open-2th)
      ;; ("3" ein:notebook-worksheet-open-3th)
      ;; ("4" ein:notebook-worksheet-open-4th)
      ;; ("5" ein:notebook-worksheet-open-5th)
      ;; ("6" ein:notebook-worksheet-open-6th)
      ;; ("7" ein:notebook-worksheet-open-7th)
      ;; ("8" ein:notebook-worksheet-open-8th)
      ;; ("9" ein:notebook-worksheet-open-last)
      ;; ("+" ein:notebook-worksheet-insert-next)
      ;; ("-" ein:notebook-worksheet-delete)
      ("x" ein:notebook-close))
    (spacemacs/set-leader-keys "ein" 'spacemacs/ein-devel-transient-state/body)))

(provide 'ein-spacemacs)
