(in-package #:sune)

(deftype ub8 () '(unsigned-byte 8))

(defconstant +message-max+ 512)

(defvar *readbuf* (make-array (list +message-max+)
                              :element-type 'ub8))
(defparameter *readbuf-fill* 0)

(defun handle-read (fd event exception)
  (assert (eq :read event))
  (assert (eq nil exception))
  (incf *readbuf-fill*
        (cffi:with-pointer-to-vector-data (ptr *readbuf*)
          (isys:read fd ptr (- +message-max+ *readbuf-fill*))))
  (loop for pos = (search #(#x0D #x0A)
                          *readbuf*
                          :end2 *readbuf-fill*)
        while pos
        for raw = (octets-to-string *readbuf*
                                    :end pos
                                    :encoding :utf-8)
        do (print (parse-message raw))
           (let ((len (+ 2 pos)))
             (replace *readbuf* *readbuf* :start2 len)
             (decf *readbuf-fill* len))))

(defun mainloop (base sock)
  (set-io-handler base (socket-os-fd sock) :read 'handle-read)
  (event-dispatch base))

(defun start (host port)
  (let ((base (make-instance 'event-base :exit-when-empty t)))
    (format t "Connecting to ~A:~A..." host port)
    (with-open-socket (sock :connect :active
                            :address-family :internet
                            :type :stream)
      (handler-case (progn (connect sock (sockets:lookup-hostname host)
                                    :port port
                                    :wait 5)
                           (format t " success.~%")
                           (mainloop base sock))
        (socket-connection-refused-error ()
          (format t " connection refused.~%"))
        (socket-connection-timeout-error ()
          (format t " connection timed out."))))))
