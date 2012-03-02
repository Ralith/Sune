(in-package #:sune)

(defparameter *nick* "Sune")
(defparameter *user* "sune")
(defparameter *name* "Too Sune")

(defun mainloop (base sock autojoin)
  (let ((c (make-connection base sock autojoin)))
    (mapc (lambda (x)
            (destructuring-bind (name command func make-state) x
              (declare (ignore name))
              (register-handler c command (curry func (funcall make-state)))))
          *default-handlers*)
    (enqueue-command c (format nil "USER ~A 0 * :~A" *user* *name*))
    (setf (connection-desired-nick c) *nick*)
    (enqueue-command c (format nil "NICK ~A" *nick*)))
  (event-dispatch base))

(defun start (host port autojoin)
  (let ((base (make-instance 'event-base :exit-when-empty t)))
    (format t "Connecting to ~A:~A..." host port)
    (with-open-socket (sock :connect :active
                            :address-family :internet
                            :type :stream)
      (handler-case (progn (connect sock (ensure-hostname host)
                                    :port port
                                    :wait 5)
                           (format t " success.~%")
                           (mainloop base sock autojoin))
        ((or socket-error resolver-error) (e)
          (format t " ~A~%" e))))))

(defun standalone-main ()
  (let ((args #+ccl (ccl::command-line-arguments)
              #-ccl (error "Unimplemented")))
    (destructuring-bind (self host port autojoin) args
      (declare (ignore self))
      (start host (parse-integer port) autojoin))))

(defun saveapp (path)
  (ccl:save-application path
                        :toplevel-function #'standalone-main
                        ;:error-handler :quit
                        :prepend-kernel t))
