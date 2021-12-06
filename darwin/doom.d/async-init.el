;;~/.emacs -*- mode: lisp-*-
(setq org-export-async-debug t)

(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/org"))
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/org_contrib/lisp"))

(require 'org)
(require 'ox)
