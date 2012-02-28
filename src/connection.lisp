(in-package #:sune)

(deftype ub8 () '(unsigned-byte 8))

(defconstant +message-max+ 512)
(defparameter +message-terminator+ #(#x0D #x0A))

(defstruct (connection (:constructor %make-connection (base socket)))
  base socket nick desired-nick
  
  (read-buffer (make-array (list +message-max+)
                           :element-type 'ub8))
  (read-fill 0)
  
  (write-buffer (make-array (list +message-max+)
                            :element-type 'ub8))
  (writing nil)
  (write-fill 0)
  (write-queue (make-queue))

  (handlers (make-hash-table :test 'equal)))

(defmethod print-object ((c connection) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "(~A handlers)" (hash-table-count (connection-handlers c)))))

(defun make-connection (base socket)
  (let ((connection (%make-connection base socket)))
    (set-io-handler base (socket-os-fd socket) :read
                    (curry 'handle-read connection))
    connection))

(defun register-handler (connection command function)
  (push function (gethash command (connection-handlers connection))))

(defun get-handlers (connection command)
  (gethash command (connection-handlers connection)))

(defun handle-write (c fd event exception)
  (assert (eq :write event))
  (assert (eq nil exception))
  (flet ((actually-write ()
           (decf (connection-write-fill c)
                 (cffi:with-pointer-to-vector-data (ptr (connection-write-buffer c))
                   (isys:write (socket-os-fd (connection-socket c))
                               ptr (connection-write-fill c))))))
   (cond
     ((< 0 (connection-write-fill c))
      (actually-write))
     ((not (queue-empty-p (connection-write-queue c)))
      (let ((message (dequeue (connection-write-queue c))))
        (replace (connection-write-buffer c)
                 message)
        (replace (connection-write-buffer c)
                 +message-terminator+
                 :start1 (length message))
        (setf (connection-write-fill c) (+ (length +message-terminator+)
                                           (length message))))
      (actually-write))
     (t
      (setf (connection-writing c) nil)
      (remove-fd-handlers (connection-base c) fd :write t)))))

(defun handle-read (c fd event exception)
  (assert (eq :read event))
  (assert (eq nil exception))
  (let ((readbuf (connection-read-buffer c)))
    (incf (connection-read-fill c)
         (cffi:with-pointer-to-vector-data (ptr readbuf)
           (isys:read fd (cffi:make-pointer (+ (cffi:pointer-address ptr)
                                               (connection-read-fill c)))
                      (- +message-max+ (connection-read-fill c)))))
    (loop for pos = (search +message-terminator+
                            readbuf
                            :end2 (connection-read-fill c))
          while pos
          for raw = (octets-to-string readbuf
                                      :end pos
                                      :encoding :utf-8
                                      :errorp nil)
          for parsed = (print (parse-message raw))
          do (mapc (rcurry 'funcall c (first parsed) (cddr parsed))
                   (get-handlers c (second parsed)))
             (let ((len (+ (length +message-terminator+) pos)))
               (replace readbuf readbuf :start2 len)
               (decf (connection-read-fill c) len)))))

;; TODO: Take a higher-level representation than raw protocol string.
(defun enqueue-message (c message)
  (let ((encoded (string-to-octets message :encoding :utf-8)))
    (assert (>= (- +message-max+ (length +message-terminator+))
                (length encoded)))
    (enqueue encoded (connection-write-queue c))
    (unless (connection-writing c)
      (setf (connection-writing c) t)
      (set-io-handler (connection-base c) (socket-os-fd (connection-socket c))
                      :write (curry 'handle-write c)))))
