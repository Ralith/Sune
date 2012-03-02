(in-package #:sune)

(defvar *default-handlers* '())

(defmacro defhandler (name command (connection source &rest params) state &body body)
  (with-gensyms (state-var)
    `(setf *default-handlers*
           (alist-set *default-handlers* ',name
                      (list ,command
                            (symbol-macrolet ,(loop for i from 0
                                                    for name in (mapcar #'first state)
                                                    collect `(,name (svref ,state-var ,i)))
                             (lambda (,state-var ,connection ,source ,@params)
                               ,@(unless state
                                   `((declare (ignore ,state-var))))
                               ,@body))
                            (lambda () ,(when state
                                          `(vector ,@(mapcar #'second state)))))))))

(defhandler autojoin "001" (c server target message) ()
  (declare (ignore server target message))
  (setf (connection-nick c) (connection-desired-nick c))
  (enqueue-command c (concatenate 'string "JOIN " (connection-autojoin c))))

(defhandler ping "PING" (c s arg) ()
  (declare (ignore s))
  (enqueue-command c (concatenate 'string "PONG :" arg)))

(defhandler own-nick-track "NICK" (c sender newnick) ()
  (when (string= (first sender) (connection-nick c))
    (setf (connection-nick c) newnick)))

;(defhandler command-dispatch (c s params))

(defun pm? (connection target)
  (string= target (connection-nick connection)))

(defun reply-target (connection sender target)
  (if (pm? connection target)
      sender
      target))

(defhandler combo "PRIVMSG" (c sender target message)
            ((state (make-hash-table :test 'equal)))
  (let ((reply-to (reply-target c sender target)))
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
        (enqueue-message c reply-to message)
        (setf (car last) nil)))))

(defun ensure-scheme (url-string)
  (if (and (> (length url-string ) 7)
           (string= "http://" url-string :end2 7))
      url-string
      (concatenate 'string "http://" url-string)))

(defhandler urltitle "PRIVMSG" (c sender target message)
            ()
  (declare (ignore sender))
  (unless (pm? c target)
    (mapc (curry 'register-uri-titler c target)
          (mapcar (compose #'parse-uri #'ensure-scheme)
                  (ppcre:all-matches-as-strings "(?i)(http://|www\\.|youtu\\.)[^\\s]+[^\\.\\)!,\\s]"
                                                message
                                                :sharedp t)))))

(defun handle-url-read (buffer uri sock c reply-to fd event exception)
  (assert (eq :read event))
  (flet ((clean-up ()
           (remove-fd-handlers (connection-base c) fd :read t)
           (close sock)))
    (handler-case
        (cond
          (exception
           (clean-up)
           (enqueue-message c reply-to (concatenate 'string "Error reading from "
                                                    (uri-host uri))))
          ((= (buffer-read-from buffer fd) 0)
           (clean-up)
           (enqueue-message c reply-to (format nil "Couldn't find a title at ~A" uri)))
          (t
           (ppcre:register-groups-bind (title)
               ("(?is)<title>(.*)</title>"
                (octets-to-string (buffer-contents buffer)
                                  :errorp nil
                                  :encoding :utf-8)
                :sharedp t)
             (when title
               (clean-up)
               (enqueue-message c reply-to (trim '(#\Space #\Tab #\Return #\Linefeed)
                                                 (substitute #\Space #\Linefeed title)))))))
      (buffer-overflow-error ()
        (clean-up)
        (enqueue-message c reply-to (format nil "Gave up looking for a title in ~A" uri))))))

(defconstant +title-search-patience+ (expt 2 16)
  "Number of bytes to download in search for the title before giving up.")

(defun handle-url-connect (uri sock c reply-to fd event exception)
  (assert (eq :write event))
  (remove-fd-handlers (connection-base c) fd :write t)
  (if exception
      (progn
        (close sock)
        (enqueue-message c reply-to (concatenate 'string "Failed to connect to ~A"
                                                 (uri-host uri))))
      (handler-case
          (progn (async-write (connection-base c) fd
                              (string-to-octets (make-http-get uri)
                                                :encoding :ascii))
                 (set-io-handler (connection-base c) fd :read
                                 (curry 'handle-url-read
                                        (make-buffer +title-search-patience+)
                                        uri sock c reply-to)))
        (character-encoding-error (e)
          (close sock)
          (enqueue-message c reply-to
                           (format nil "Unable to encode HTTP GET: ~A" e))))))

(defun register-uri-titler (conn reply-to uri)
  ;; TODO: Async DNS
  (let ((sock (make-socket :connect :active
                           :address-family :internet
                           :type :stream)))
    (handler-case (progn
                    (connect sock (ensure-hostname (uri-host uri))
                             :port (or (uri-port uri) 80)
                             :wait nil)
                    (set-io-handler (connection-base conn) (socket-os-fd sock) :write
                                    (curry 'handle-url-connect uri sock conn reply-to)))
      (resolver-error (e)
        (enqueue-message conn reply-to (princ-to-string e))))))

(defun make-http-get (uri)
  (let ((crlf (coerce '(#\Return #\Linefeed) 'string)))
   (concatenate 'string
                "GET "
                (or (uri-path uri) "/")
                (when-let (query (uri-query uri))
                  (concatenate 'string "?" query))
                " HTTP/1.1" crlf
                "Host: " (uri-host uri) crlf
                "User-Agent: Mozilla/5.0 (X11; Linux i686; rv:10.0.2) Gecko/20100101 Firefox/10.0.2" crlf
                "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" crlf
                "Accept-Charset: utf-8" crlf
                "Connection: close" crlf
                crlf)))
