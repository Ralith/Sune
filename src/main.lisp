(in-package #:sune)

(defparameter *nick* "Sune")
(defparameter *user* "sune")
(defparameter *name* "Too Sune")

(defun mainloop (base sock)
  (let ((c (make-connection base sock)))
    (mapc (lambda (x) (register-handler c (car x) (cdr x))) *default-handlers*)
    (enqueue-message c (format nil "USER ~A 0 * :~A" *user* *name*))
    (setf (connection-desired-nick c) *nick*)
    (enqueue-message c (format nil "NICK ~A" *nick*)))
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
