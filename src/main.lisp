(in-package #:sune)

(defparameter *nick* "Sune")
(defparameter *user* "sune")
(defparameter *name* "Too Sune")

(defun mainloop (base sock)
  (let ((c (make-connection base sock)))
    (mapc (lambda (x)
            (destructuring-bind (name command func make-state) x
              (declare (ignore name))
              (register-handler c command (curry func (funcall make-state)))))
          *default-handlers*)
    (enqueue-command c (format nil "USER ~A 0 * :~A" *user* *name*))
    (setf (connection-desired-nick c) *nick*)
    (enqueue-command c (format nil "NICK ~A" *nick*)))
  (event-dispatch base))

(defun start (host port)
  (let ((base (make-instance 'event-base :exit-when-empty t)))
    (format t "Connecting to ~A:~A..." host port)
    (with-open-socket (sock :connect :active
                            :address-family :internet
                            :type :stream)
      (handler-case (progn (connect sock (ensure-hostname host)
                                    :port port
                                    :wait 5)
                           (format t " success.~%")
                           (mainloop base sock))
        ((or socket-error resolver-error) (e)
          (format t " ~A~%" e))))))
