(in-package #:sune)

(defvar *default-handlers* '())

(defmacro defhandler (name command (connection source params) state &body body)
  (with-gensyms (state-var)
    `(setf *default-handlers*
           (alist-set *default-handlers* ',name
                      (list ,command
                            (lambda (,state-var ,connection ,source ,params)
                              (declare ,@(unless state `((ignore ,state-var)))
                                       (ignorable ,connection ,source ,params))
                              (symbol-macrolet ,(loop for i from 0
                                                      for name in (mapcar #'first state)
                                                      collect `(,name (svref ,state-var ,i)))
                                ,@body))
                            (lambda () ,(when state
                                          `(vector ,@(mapcar #'second state)))))))))

(defhandler autojoin "001" (c server params) ()
  (setf (connection-nick c) (connection-desired-nick c))
  (enqueue-message c "JOIN #forble"))

(defhandler ping "PING" (c s params) ()
  (enqueue-message c (concatenate 'string "PONG :" (first params))))

(defhandler own-nick-track "NICK" (c sender params) ()
  (when (string= (first sender) (connection-nick c))
    (setf (connection-nick c) (first params))))

(defun pm? (connection target)
  (string= target (connection-nick connection)))

(defhandler combo "PRIVMSG" (c sender params)
            ((state (make-hash-table :test 'equal)))
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
          (setf (car last) nil))))))
