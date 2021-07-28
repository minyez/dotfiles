;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

(package! rg)
(package! yasnippet-snippets)
(package! jsonrpc) ; to use jieba
(package! move-text)
;(package! writegood-mode)
;(package! zenburn-theme)

;; org-related
;(package! org-journal)
(package! org-ref
  :recipe (:host github :repo "jkitchin/org-ref"))
(package! org-super-agenda)
(package! org-superstar)
(package! org-fancy-priorities)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(unpin! org-roam)
;(package! org-roam)
;(package! org-roam :pin "89e9121"
;  :recipe (:host github :repo "org-roam/org-roam"))
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
(package! org-roam-dashboard
  :recipe (:host github :repo "publicimageltd/org-roam-dashboard"))
;(package! org-download)
(package! org-fragtog)
(package! company-org-roam)
(package! lister
  :recipe (:host github :repo "publicimageltd/lister" :branch "main"))
(package! delve
  :recipe (:host github :repo "publicimageltd/delve" :branch "main"))
; Bibliography
(package! org-roam-bibtex)
(unpin! bibtex-completion helm-bibtex ivy-bibtex)
;; PDF-related tools
(package! pdf-tools)
;(package! org-pdftools)
;(package! interleave
;  :recipe (:host github :repo "rudolfochrist/interleave"))
;(package! golden-ratio
;  :recipe (:host github :repo "roman/golden-ratio.el"))
(package! org-noter)
;(package! org-noter-pdftools)
;; When using org-roam via the `+roam` flag
;(unpin! org-roam company-org-roam)

(package! asymbol
  :recipe (:host github :repo "dwuggh/asymbol"))
(package! company-jedi)

(package! dtrt-indent 
  :recipe (:host github :repo "jscheid/dtrt-indent"))

;; code-related
;(package! pyenv
;  :recipe (:host github :repo " aiguofer/pyenv.el" :branch "master"))

;(package! company-jedi
;  :recipe (:host github :repo " emacsorphanage/company-jedi"))
;(package! zotxt)
;; equation OCR using Mathpix API
;(package! mathpix
;  :recipe (:host github :repo "jethrokuan/mathpix.el"))
;(package! markdown-mode
;	:recipe (:host github :repo "jrblevin/markdown-mode"))
;(package! impatient-mode)
;(package! simple-httpd)
(package! magic-latex-buffer)
(package! yasnippet
  :recipe (:host github :repo "joaotavora/yasnippet"))
(package! cdlatex)
;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
