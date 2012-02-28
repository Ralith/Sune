(in-package #:sune)

(defparameter *default-handlers* '())

(defmacro defhandler (name command (connection source params) &body body)
  `(push (cons ,command (named-lambda ,name (,connection ,source ,params)
                          ,@body))
         *default-handlers*))

(defhandler autojoin "001" (c server params)
  (declare (ignore server params))
  (setf (connection-nick c) (connection-desired-nick c))
  (enqueue-message c "JOIN #forble"))

(defhandler ping "PING" (c s params)
  (declare (ignore s))
  (enqueue-message c (concatenate 'string "PONG :" (first params))))

(defhandler own-nick-track "NICK" (c sender params)
  (when (string= (first sender) (connection-nick c))
    (setf (connection-nick c) (first params))))

(defun pm? (connection target)
  (string= target (connection-nick connection)))

;;; TODO: Per-connection state?
(let ((state (make-hash-table :test 'equal)))
 (defhandler combo "PRIVMSG" (c sender params)
   (destructuring-bind (target message) params
     (let ((reply-to (if (pm? c target) (first sender) target)))
       (multiple-value-bind (last exists) (gethash reply-to state)
         (cond
           ((not exists)
            (setf last (cons message 0))
            (setf (gethash reply-to state) last))
           ((string= (car last) message)
            (incf (cdr last)))
           (t
            (setf (car last) message
                  (cdr last) 0)))
         (when (>= (cdr last) 2)
           (enqueue-message c (format nil "PRIVMSG ~A :~A" reply-to message))
           (setf (car last) nil)))))))
