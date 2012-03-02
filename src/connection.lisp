(in-package #:sune)

(deftype ub8 () '(unsigned-byte 8))

(defconstant +message-max+ 512)
(defparameter +message-terminator+ #(#x0D #x0A))

(defstruct (connection (:constructor %make-connection (base socket)))
  base socket nick desired-nick
  
  (read-buffer (make-buffer +message-max+))
  (write-buffer (make-buffer +message-max+))
  (writing nil)
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

(defun handle-write (c fd event exception &aux (buffer (connection-write-buffer c)))
  (assert (eq :write event))
  (assert (eq nil exception))
  (flet ((actually-write ()
           (buffer-write-to buffer fd)))
   (cond
     ((< 0 (buffer-fill buffer))
      (actually-write))
     ((not (queue-empty-p (connection-write-queue c)))
      (let ((message (dequeue (connection-write-queue c))))
        (buffer-read-vec buffer message)
        (buffer-read-vec buffer +message-terminator+))
      (actually-write))
     (t
      (setf (connection-writing c) nil)
      (remove-fd-handlers (connection-base c) fd :write t)))))

(defun handle-read (c fd event exception &aux (buffer (connection-read-buffer c)))
  (assert (eq :read event))
  (assert (eq nil exception))
  (buffer-read-from buffer fd)
  (loop with data = (buffer-contents buffer)
        with lastpos = 0
        for pos = (search +message-terminator+ data :start2 lastpos)
        while pos
        for raw = (octets-to-string data
                                    :start lastpos
                                    :end pos
                                    :encoding :utf-8
                                    :errorp nil)
        for parsed = (parse-message raw)
        do (princ raw) (terpri)
           (mapc (rcurry 'funcall c (first parsed) (cddr parsed))
                 (get-handlers c (second parsed)))
           (let ((len (+ (length +message-terminator+) (- pos lastpos))))
             (buffer-drop buffer len)
             (incf lastpos len)))
  (buffer-compact buffer))

;; TODO: Take a higher-level representation than raw protocol string.
(defun enqueue-command (c command)
  (princ command) (terpri)
  (let ((encoded (string-to-octets command :encoding :utf-8)))
    (assert (>= (- +message-max+ (length +message-terminator+))
                (length encoded)))
    (enqueue encoded (connection-write-queue c))
    (unless (connection-writing c)
      (setf (connection-writing c) t)
      (set-io-handler (connection-base c) (socket-os-fd (connection-socket c))
                      :write (curry 'handle-write c)))))

(defun enqueue-message (c target message)
  (enqueue-command c (format nil "PRIVMSG ~A :~A" target message)))
