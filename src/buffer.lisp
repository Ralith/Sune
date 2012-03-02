(in-package #:sune)

(deftype ub8 () '(unsigned-byte 8))

(defstruct (buffer (:constructor %make-buffer (array)))
  (array #() :type '(simple-array ub8 (*)))
  (read-pos 0 :type 'fixnum)
  (write-pos 0 :type 'fixnum))

(defmethod print-object ((b buffer) stream)
  (print-unreadable-object (b stream :type t :identity t)
    (format stream "(~A bytes)" (buffer-fill b))))

(defun make-buffer (&optional (size 512))
  (%make-buffer (make-array size :element-type 'ub8)))

(defun buffer-contents (buffer)
  (displace-array (buffer-array buffer)
                  :offset (buffer-read-pos buffer)
                  :dimensions (buffer-fill buffer)))

(defun buffer-fill (buffer)
  (- (buffer-write-pos buffer)
     (buffer-read-pos buffer)))

(defun buffer-space-remaining (buffer)
  (- (length (buffer-array buffer))
     (buffer-write-pos buffer)))

(defun buffer-reset (buffer)
  (setf (buffer-read-pos buffer) 0
        (buffer-write-pos buffer) 0))

(define-condition buffer-error (error)
  ((buffer :initform (error "Must supply buffer")
           :initarg :buffer :reader buffer-of)))

(define-condition buffer-overflow-error (buffer-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Tried to write more data than ~A has room for"
                     (buffer-of condition)))))

(define-condition buffer-underflow-error (buffer-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Tried to read more data than ~A contains"
                     (buffer-of condition)))))

(defun buffer-read-from (buffer fd)
  (let ((remaining (buffer-space-remaining buffer)))
    (unless (> remaining 0)
      (error 'buffer-overflow-error :buffer buffer))
    (with-accessors ((wpos buffer-write-pos)) buffer
      (let ((bytes (cffi:with-pointer-to-vector-data (ptr (buffer-array buffer))
                     (isys:read fd (cffi:make-pointer (+ (cffi:pointer-address ptr) wpos))
                                remaining))))
        (incf wpos bytes)
        bytes))))

(defun buffer-read-vec (buffer vec)
  (unless (> (buffer-space-remaining buffer) (length vec))
    (error 'buffer-overflow-error :buffer buffer))
  (replace (buffer-array buffer) vec :start1 (buffer-write-pos buffer))
  (incf (buffer-write-pos buffer) (length vec)))

(defun buffer-write-to (buffer fd)
  (let ((waiting (buffer-fill buffer)))
    (unless (> waiting 0)
      (error 'buffer-underflow-error :buffer buffer))
   (with-accessors ((rpos buffer-read-pos)) buffer
     (let ((bytes (cffi:with-pointer-to-vector-data (ptr (buffer-array buffer))
                    (isys:write fd (cffi:make-pointer (+ (cffi:pointer-address ptr) rpos))
                                waiting))))
       (incf rpos bytes)
       (when (= 0 (buffer-fill buffer))
         (buffer-reset buffer))
       bytes))))

(defun buffer-drop (buffer bytes)
  (assert (<= bytes (buffer-fill buffer)))
  (incf (buffer-read-pos buffer) bytes)
  (when (= 0 (buffer-fill buffer))
    (buffer-reset buffer)))

(defun buffer-compact (buffer)
  (when (> (buffer-fill buffer) 0)
    (replace (buffer-array buffer) (buffer-array buffer)
             :start2 (buffer-read-pos buffer)
             :end2 (buffer-write-pos buffer))
    (setf (buffer-write-pos buffer) (buffer-fill buffer)
          (buffer-read-pos buffer) 0)))
