(in-package #:sune)

(defun alist-set (alist key value)
  (if-let (acons (assoc key alist))
    (setf (cdr acons) value)
    (push (cons key value) alist))
  alist)
