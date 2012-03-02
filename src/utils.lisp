(in-package #:sune)

(defun alist-set (alist key value)
  (if-let (acons (assoc key alist))
    (setf (cdr acons) value)
    (push (cons key value) alist))
  alist)

(defun async-write (base fd data)
  (check-type data (simple-array ub8 (*)))
  (let ((cursor 0))
   (set-io-handler base fd :write
                   (lambda (fd event exception)
                     (declare (ignore event))
                     (if exception
                         (progn (warn "Error during write")
                                (remove-fd-handlers base fd :write t))
                         (progn
                           (incf cursor
                                 (cffi:with-pointer-to-vector-data (ptr data)
                                   (isys:write fd (cffi:make-pointer
                                                   (+ (cffi:pointer-address ptr)
                                                      cursor))
                                               (- (length data) cursor))))
                           (when (= cursor (length data))
                             (remove-fd-handlers base fd :write t))))))))

(defun trim-left (chars string)
  (loop for idx from 0
        for c across string
        while (find c chars)
        finally (return (displace-array string :offset idx))))

(defun trim-right (chars string)
  (loop for idx from (1- (length string)) downto 0
        while (find (aref string idx) chars)
        finally (return (displace-array string :dimensions (1+ idx)))))

(defun trim (chars string)
  (trim-right chars (trim-left chars string)))
