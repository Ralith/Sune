(in-package #:sune)

(defparameter *default-handlers* '())

(defmacro defhandler (name command (connection source params) &body body)
  `(push (cons ,command (named-lambda ,name (,connection ,source ,params)
                          ,@body))
         *default-handlers*))

(defhandler autojoin "001" (c server params)
  (declare (ignore server params))
  (enqueue-message c "JOIN #forble"))

(defhandler ping "PING" (c s params)
  (declare (ignore s))
  (enqueue-message c (concatenate 'string "PONG :" (first params))))
