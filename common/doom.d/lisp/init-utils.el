;;; provide utilities for configuration

;;; =====================
;;; 
(defun mz/set-choice-to-current-hour (var lchoice dchoice &optional start end)
  (let ((hour (string-to-number (substring (current-time-string) 11 13)))
        (start-hour (if start start 6))
        (end-hour (if end end 19)))
    (if (member hour (number-sequence start-hour end-hour))
      (set var lchoice)
      (set var dchoice))))

(defun mz/org-goto-today-node (&optional DTFORMAT)
  "go to the node with the time string of today in DTFORMAT as heading"
  (let* ((dtformat (if DTFORMAT DTFORMAT "%Y-%m-%d %a"))
         (headline (format-time-string dtformat (current-time))))
    ;;; following code copied from org-capture.el L983
    (widen)
    (goto-char (point-min)) ;;; get to the very beginning in the file
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   (regexp-quote headline))
                           nil t)
        ()
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " headline "\n"))))

;;; =====================
;;; system-depedent setup
;;; adapted from https://stackoverflow.com/a/26137517
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ,type)
     ,@body))

(defmacro except-system (type &rest body)
  "Evaluate BODY if `system-type' doest not equal to TYPE."
  (declare (indent defun))
  `(unless (eq system-type ,type)
     ,@body))

;;; =====================
;;; machine setup
(defmacro with-hostname (type &rest body)
  "Evaluate BODY if `system-name' equals TYPE."
  (declare (indent defun))
  `(when (string= system-name ,type)
     ,@body))

(defmacro except-hostname (type &rest body)
  "Evaluate BODY if `system-name' equals TYPE."
  (declare (indent defun))
  `(unless (string= system-name ,type)
     ,@body))

;;; =====================
;;; general helper function
(defun mz/find-other-file (EXT &optional IN-NEWWIN REPLACE-OTHER)
  "open the file of same name but with a different extension.

EXT is the extension name, case-sensitive.

If IN-NEWWIN is nil, the file will be opened in the same window.
If IN-NEWWIN is 'next, the file will be always opened in the next window.
If IN-NEWWIN is non-nil, open the file in a new window if REPLACE-OTHER is nil.
The new window is arranged by `split-window-sensibly'.
Otherwise, if the current buffer of next window has the same extension,
the file will be opened in the next window."
  (interactive (list (read-string "Extension name: ") t t))
  (let ((otherfile (file-name-with-extension (buffer-name) EXT))
        (ext-nw (file-name-extension (save-selected-window
                                       (other-window 1)
                                       (buffer-name)))))
    (if (equal otherfile (buffer-name))
      (progn
        (message "already at %s" otherfile)
        nil)
      (if (file-exists-p otherfile)
        (if IN-NEWWIN
          (progn
            (if (not (equal IN-NEWWIN 'next))
              (if (or (not (equal EXT ext-nw)) (not REPLACE-OTHER))
                (if (not (split-window-sensibly))
                  (split-window-below))))
            (find-file-other-window otherfile))
          (find-file otherfile))
        (progn
          (message "%s does not exist" otherfile)
          nil)))))

(defun mz/find-pdf (&optional IN-NEXT)
  "open the pdf file by `mz/find-other-file'

With \\[universal-argument], pdf will be always opened in the next window."
  (interactive "P")
  (if IN-NEXT
    (mz/find-other-file "pdf" 'next t)
    (mz/find-other-file "pdf" t t)))

(defun insert-brakets (&optional arg)
  "Enclose following ARG sexps in braket.
Leave point after open-paren.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `[]' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries.

This command assumes point is not in a string or comment.
The code is copied from emacs internal lisp.el"
  (interactive "P")
  (insert-pair arg ?\[ ?\]))

(provide 'init-utils)
;;; init-utils.el ends here
