(in-package #:sune)

(deftype ub8 () '(unsigned-byte 8))

(defconstant +message-max+ 512)
(defparameter +message-terminator+ #(#x0D #x0A))

(defparameter *nick* "Sune")
(defparameter *user* "sune")
(defparameter *name* "Too Sune")

(defstruct (connection (:constructor make-connection (base socket)))
  base socket
  
  (read-buffer (make-array (list +message-max+)
                           :element-type 'ub8))
  (read-fill 0)
  
  (write-buffer (make-array (list +message-max+)
                            :element-type 'ub8))
  (writing nil)
  (write-fill 0)
  (write-queue (make-queue)))


(defmethod print-object ((c connection) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (princ (connection-socket c) stream)))

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
          do (print (parse-message raw))
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

(defun mainloop (base sock)
  (let ((c (make-connection base sock)))
    (enqueue-message c (format nil "USER ~A 0 * :~A" *user* *name*))
    (enqueue-message c (format nil "NICK ~A" *nick*))

    (set-io-handler base (socket-os-fd sock) :read (curry 'handle-read c)))

  (event-dispatch base))

(defun start (host port)
  (let ((base (make-instance 'event-base :exit-when-empty t)))
    (format t "Connecting to ~A:~A..." host port)
    (with-open-socket (sock :connect :active
                            :address-family :internet
                            :type :stream)
      (handler-case (progn (connect sock (lookup-hostname host)
                                    :port port
                                    :wait 5)
                           (format t " success.~%")
                           (mainloop base sock))
        (socket-connection-refused-error ()
          (format t " connection refused.~%"))
        (socket-connection-timeout-error ()
          (format t " connection timed out."))))))
