(in-package #:sune)

(define-parser *message-parser*
  (:start-symbol message)
  (:terminals (word colon space))

  (message
   (prefix word params #'list*))

  (prefix
   (colon word space (lambda (a b c)
                       (declare (ignore a c))
                       (parse-prefix b)))
   ())

  (params
   (middle-params space colon trailing (lambda (a b c d)
                                         (declare (ignore b c))
                                         (append (nreverse a)
                                                 (list (apply #'concatenate 'string
                                                              (nreverse d))))))
   middle-params
   (colon trailing (lambda (a b)
                     (declare (ignore a))
                     (list (apply #'concatenate 'string b)))))

  (middle-params
   (middle-params space word (lambda (a b c)
                               (declare (ignore b))
                               (cons c a)))
   ())

  (trailing
   (trailing word (lambda (a b)
                    (cons b a)))
   (trailing space (lambda (a b)
                     (cons b a)))
   (trailing colon (lambda (a b)
                     (cons b a)))
   ()))

(defun parse-prefix (string)
  (split-sequence-if (rcurry 'member '(#\! #\@)) string))

(defun irc-lexer (stream)
  (loop with buf = (make-array '(32) :element-type 'character
                                     :adjustable t
                                     :fill-pointer 0)
        with midword = nil
        for char = (read-char stream nil)
        do (case char
             (#\Space
              (if midword
                  (progn (unread-char char stream)
                         (return (values 'word buf)))
                  (return (values 'space " "))))
             (#\:
              (if midword
                  (vector-push-extend char buf)
                  (return (values 'colon ":"))))
             ((nil)
              (if midword
                  (return (values 'word buf))
                  (return (values nil nil))))
             (t
              (setf midword t)
              (vector-push-extend char buf)))))

(defun parse-message (string)
  (with-input-from-string (stream string)
    (parse-with-lexer (curry #'irc-lexer stream) *message-parser*)))
