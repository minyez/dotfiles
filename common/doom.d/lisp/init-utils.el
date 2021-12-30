;;; provide utilities for configuration

;;; =====================
;;; system-depedent setup
;;; adapted from https://stackoverflow.com/a/26137517
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ,type)
     ,@body))

(defmacro with-darwin (&rest body)
  "Evaluate BODY when on Darwin."
  (with-system 'darwin ,@body))

(defmacro with-linux (&rest body)
  "Evaluate BODY when on Linux."
  (with-system 'gnu/linux ,@body))
;;; =====================

(provide 'init-utils)
;;; init-utils.el ends here
