(use-package! flycheck
  :config
  ; only use pylint, disable others
  ; setq-default acts as a global operation
  (setq-default flycheck-disabled-checkers '(
                python-flake8 python-pycompile python-pyright python-mypy
                )
  )
  ;;; whitelist dir local variables
  ;;; https://stackoverflow.com/q/19806176
  ;;; for example
  ;;(put 'flycheck-gfortran-include-path 'safe-local-variable #'stringp)

  ;;; custom checkers
  ;;; ifort checker, adopted from flycheck fortran-gfortran checker
  ;;; and https://github.com/edbennett/flycheck-ifort/blob/master/flycheck-ifort.el
  (flycheck-def-args-var flycheck-ifort-args fortran-ifort)
  (flycheck-def-option-var flycheck-ifort-include-path nil fortran-ifort
    "A list of include directories for Intel Fortran.
  
  The value of this variable is a list of strings, where each
  string is a directory to add to the include path of ifort.
  Relative paths are relative to the file being checked."
    :type '(repeat (directory :tag "Include directory"))
    :safe #'flycheck-string-list-p)
  
  (flycheck-def-option-var flycheck-ifort-language-standard "f95"
                           fortran-ifort
    "The language standard to use in Intel fortran.
  
  The value of this variable is either a string denoting a language
  standard, or nil, to use the default standard.  When non-nil,
  pass the language standard via the `-stand' option."
    :type '(choice (const :tag "Default standard" nil)
                   (string :tag "Language standard")))
  
  
  (flycheck-def-option-var flycheck-ifort-layout nil fortran-ifort
    "The source code layout to use in Intel Fortran.
  
  The value of this variable is one of the following symbols:
  
  nil
       Let ifort determine the layout from the extension
  
  `free'
       Use free form layout
  
  
  `fixed'
       Use fixed form layout
  
  In any other case, an error is signaled."
    :type '(choice (const :tag "Guess layout from extension" nil)
                   (const :tag "Free form layout" free)
                   (const :tag "Fixed form layout" fixed))
    :safe (lambda (value) (or (not value) (memq value '(free fixed)))))

  (defun flycheck-option-ifort-layout (value)
    "Option VALUE filter for `flycheck-ifort-layout'."
    (pcase value
      (`nil nil)
      (`free "-free")
      (`fixed "-fixed")
      (_ (error "Invalid value for flycheck-ifort-layout: %S" value))))
  
  (flycheck-def-option-var flycheck-ifort-warnings '("all")
                           fortran-ifort
    "A list of warnings for Intel Fortran.
  
  The value of this variable is a list of strings, where each string
  is the name of a warning category to enable.  By default, all
  recommended warnings are enabled (as by `-Wa')."
    :type '(choice (const :tag "No additional warnings" nil)
                   (repeat :tag "Additional warnings"
                           (string :tag "Warning name")))
    :safe #'flycheck-string-list-p)
  
  (flycheck-define-checker fortran-ifort
    "An Fortran syntax checker using Intel Fortran"
    :command ("ifort"
              "-syntax-only"
              ;;; Fortran has similar include processing as C/C++
              ;;"-iquote" (eval (flycheck-c/c++-quoted-include-directory))
              (option "-stand=" flycheck-ifort-language-standard concat)
              (option "" flycheck-ifort-layout concat
                      flycheck-option-ifort-layout)
              (option-list "-warn " flycheck-ifort-warnings)
              (option-list "-I" flycheck-ifort-include-path concat)
              (eval flycheck-ifort-args)
              source)
    :error-patterns
    (
     (error line-start (file-name) "(" line "): error #" (one-or-more (in "0-9")) ": " (message) "\n"
            (zero-or-more not-newline) "\n"
            (zero-or-more not-newline) line-end)
     (warning line-start (file-name) "(" line "): warning #" (one-or-more (in "0-9")) ": " (message) "\n"
            (zero-or-more not-newline) "\n"
            (zero-or-more not-newline) line-end)
     (info line-start (file-name) "(" line "): remark #" (one-or-more (in "0-9")) ": " (message) "\n"
            (zero-or-more not-newline) "\n"
            (zero-or-more not-newline) line-end)
    )
    :modes (fortran-mode f90-mode)
  )
)

;;; language server provider related
;;; used with flycheck
(use-package! eglot
  :config
  (add-hook 'f90-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
)

(after! eglot
  :config
  (set-eglot-client! 'python-mode '("pylsp"))
  (set-eglot-client! 'f90-mode '("fortls"))
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))
  ;;; modified from https://github.com/joaotavora/eglot/issues/590#issuecomment-758233948
  ;;; when using setq, eglot-workspace-configuration is still nil in test project files, opened by projectile
  ;;; use setq-default instead works for me (not enough time to think about why)
  (setq-default eglot-workspace-configuration
        '((pylsp
            (plugins
             (pycodestyle
              (enabled . nil))
             (pyflakes
              (enabled . nil))
             ))
         )
  )
  ;((python-mode . ((eglot-workspace-configuration
  ;    . (
  ;       (:pylsp . (:plugins (:pycodestyle (:enabled nil))))
  ;      )))
  ;  )
  ;)
  ;;; adjust PATH manually, adapted from section 1.2.5 of https://dotdoom.rgoswami.me/config.html
  ;;; put exec-path later to "prepend" effetively
  ;;; I installed pylsp and fortls to my usually working environment, so adding lsp bin to PATH seems not ncessary
  ;(when (string= (system-name) "myz-amd-fedora")
  ;  (setq exec-path (append '(
  ;                          ;(concat (getenv "HOME") "/.pyenv/versions/miniconda3-4.7.12/envs/lsp/bin/") ;; python, fortran
  ;                          "/home/minyez/.pyenv/versions/miniconda3-4.7.12/envs/lsp/bin/"
  ;                          )
  ;                          exec-path 
  ;                          ))
  ;)
)

(provide 'config-lsp)
;;; config-lsp.el ends here
