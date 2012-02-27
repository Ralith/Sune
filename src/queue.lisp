(in-package #:sune)

(defstruct (queue (:constructor make-queue ()))
  (items '() :type list)
  (tail '() :type list))
 
(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))
 
(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))))
 
(defun dequeue (queue)
  "Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
      (values nil nil)
      (prog1 (values (pop (queue-items queue)) t)
        (unless (queue-items queue)
          (setf (queue-tail queue) nil)))))
