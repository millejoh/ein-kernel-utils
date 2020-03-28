# ein-kernel-utils
Add-on for the Emacs IPython Notebook that implements enhanced interaction with Jupyter kernels.

# Installation and Usage

As this code is experimental, download is only support via git. Note that quelpa
doesn't quite work as it will not download the hy or python support files.

Specifically, find a reasonable location in your account and execute:

  `git clone https://github.com/millejoh/ein-kernel-utils <PATH>`

Then add the following to your init file, after whatever code you use to load emacs-ipython-notebook:

```lisp
(use-package ein-kernel-utils
    :load-path <PATH>
    :config
    (require 'ein-kernel-completion)
    (require 'ein-buffer-connect)
    (add-hook 'ein:notebook-mode-hook #'(lambda () (add-to-list 'company-backends 'ein:company-backend)))
    (add-hook 'ein:on-kernel-connect-functions #'(lambda (kernel)
                                                   (ein:kernel-utils-load-safely kernel))))
```

This is all you should need to have code completion using the Jupyter kernel in
ein notebooks and to have the ability to connect Python emacs buffers to a
running notebook. Note that you will need to set your own key bindings for the
connect commands as I no longer enforce any particular configuration. One
option for setting keybindings is below:


```lisp

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-c\C-c" 'ein:connect-run-or-eval-buffer)
            (define-key python-mode-map "\C-c\C-l" 'ein:connect-reload-buffer)
            (define-key python-mode-map "\C-c\C-r" 'ein:connect-eval-region)
            (define-key python-mode-map (kbd "C-:") 'ein:shared-output-eval-string)
            (define-key python-mode-map "\C-c\C-z" 'ein:connect-pop-to-notebook)
            (define-key python-mode-map "\C-c\C-x" 'ein:tb-show)
            (define-key python-mode-map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)))
```
