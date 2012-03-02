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

(defun buffer-read-from (buffer fd)
  (let ((remaining (buffer-space-remaining buffer)))
    (unless (> remaining 0)
      (error "Tried to read into a full buffer."))
    (with-accessors ((wpos buffer-write-pos)) buffer
      (incf wpos
            (cffi:with-pointer-to-vector-data (ptr (buffer-array buffer))
              (isys:read fd (cffi:make-pointer (+ (cffi:pointer-address ptr) wpos))
                         remaining))))))

(defun buffer-read-vec (buffer vec)
  (replace (buffer-array buffer) vec :start1 (buffer-write-pos buffer))
  (incf (buffer-write-pos buffer) (length vec)))

(defun buffer-write-to (buffer fd)
  (let ((waiting (buffer-fill buffer)))
    (unless (> waiting 0)
      (error "Tried to write from an empty buffer."))
   (with-accessors ((rpos buffer-read-pos)) buffer
     (incf rpos
           (cffi:with-pointer-to-vector-data (ptr (buffer-array buffer))
             (isys:write fd (cffi:make-pointer (+ (cffi:pointer-address ptr) rpos))
                         waiting))))
    (when (= 0 (buffer-fill buffer))
      (buffer-reset buffer))))

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
