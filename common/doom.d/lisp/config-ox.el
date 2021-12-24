;;; configurations about org export (ox-), currently including
;;;   - beamer
;;;   - latex

;;; beamer export
(use-package! ox-beamer
  :bind
  ("C-c x b" . org-beamer-export-to-latex)
  :config
  ; default 3rd level heading as frame.
  ; 1st and 2nd are sec and subsec
  (setq org-beamer-frame-level 3)
)

;;; latex export
; https://superuser.com/questions/896741/how-do-i-configure-org-latex-classes-in-emacs
; https://stackoverflow.com/questions/36197545/org-mode-latex-export-making-todos-red
(use-package! ox-latex
  :bind
  ("C-c x l" . org-latex-export-to-latex)
  :config
  ;(fset 'my-org-latex-export-open-pdf
  ; (kmacro-lambda-form [?\C-c ?\C-e ?l ?o] 0 "%d"))
  ;(fset 'my-org-beamer-export-open-pdf
  ; (kmacro-lambda-form [?\C-c ?\C-e ?l ?O] 0 "%d"))
  (setq org-latex-pdf-process '("latexmk -latexoption=\"-interaction=nonstopmode -shell-escape\" -pdf -pdflatex=%latex -bibtex -f %f"))
  ;; prefer custom label
  (setq org-latex-prefer-user-labels t)
  ;; user-defined headline to-do rendering
  (defun org-latex-format-headline-colored-keywords-function (todo todo-type priority text tags info)
        (concat
           ;(cond 
           ;      ; colorize to-do items
           ;      ; comment whole cond to avoid showing to-do markers
           ;      ((string= todo "TODO")(and todo (format "{\\color{Red}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "WIP")(and todo (format "{\\color{ProcessBlue}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "WAITING")(and todo (format "{\\color{Magenta}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "CANCELLED")(and todo (format "{\\color{Orange}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "DONE")(and todo (format "{\\color{Green}\\bfseries\\sffamily %s} " todo)))
           ;)
           ;(and priority (format "\\framebox{\\#%c} " priority))
           text
           (and tags
           (format "\\hfill{}\\textsc{%s}"
             (mapconcat (lambda (tag) 
                            (if (string= tag "STAR") "\\bigstar" (org-latex-plain-text tag info))) tags ",")))
             ;(mapconcat (lambda (tag) (org-latex-plain-text tag info)) tags ":")))
    )
  )
  (setq org-latex-format-headline-function 'org-latex-format-headline-colored-keywords-function)
  ; customized classes for latex export
  ; general Chinese note
  (setq org-latex-classes
               '(
  ; general English note
                ("article"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper]{article}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
[EXTRA]
[PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; minimal working example
                ("mwe"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper]{article}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
\\usepackage{mwe}
\\usepackage{amsmath,amssymb}
[NO-DEFAULT-PACKAGES]
[EXTRA]
[NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ;; General Chinese note
                ("ctexart"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper,scheme=plain]{ctexart}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ;; For org-journal. section for day, subsec for time
                ("journal"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper,scheme=plain]{ctexart}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ;("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ;("\\subparagraph{%s}" . "\\subparagraph*{%s}")
				 )
  ; Note for math. phys. books
                ("book"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[a4paper,11pt,titlepage]{book}
[DEFAULT-PACKAGES]
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
\\usepackage{physics}
\\usepackage{cases}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("report"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt]{report}
[DEFAULT-PACKAGES]
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("ctexbook"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[a4paper,11pt,titlepage]{ctexbook}
[DEFAULT-PACKAGES]
\\usepackage{physics}
\\usepackage{cases}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; General Engilish scientific writing
                ("sci"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[a4paper,11pt]{article}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
\\usepackage{threeparttable}
\\usepackage{physics}
\\usepackage{multirow}
\\usepackage{siunitx}  % align by decimal
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; PRB paper
                ("prb"
                 "\\documentclass[aps,prb,twocolumn,a4paper,floatfix,showpacs]{revtex4-1}
\\bibliographystyle{apsrev4-1}
[NO-DEFAULT-PACKAGES]
\\usepackage[version=3]{mhchem}
\\usepackage{times,mathptmx}
\\usepackage{float}
\\usepackage{array}
\\usepackage{epstopdf}
\\usepackage{threeparttable}
\\usepackage{physics}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage{multirow}
\\usepackage{siunitx}  % align by decimal
\\usepackage{booktabs}
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; Beamer for dual export
; https://orgmode.org/worg/exporters/beamer/beamer-dual-format.html
                ("beamer"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=True,linkcolor=,filecolor=,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0}}{hyperref}
\\documentclass[ignorenonframetext,presentation]{beamer}
[DEFAULT-PACKAGES]
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% space between caption and table
\\captionsetup[table]{belowskip=-6pt}
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 )
                ("beamerarticle"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper]{article} % add twoside for booklet printing
\\usepackage{beamerarticle}
\\usepackage[margin=0.8in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
\\usepackage{physics}
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ))
  ;(setq org-latex-listings t)
  ;(add-to-list 'org-latex-packages-alist '("newfloat"  "minted"))
  ;; or
  (setq org-latex-listings 'minted
        org-latex-minted-options '(
          ("bgcolor" "bg")
          ("breaklines" "true")
          ("autogobble" "true")
          ("fontsize" "\\small")
          )
  )
)

(provide 'config-ox)
;;; config-ox.el ends here
