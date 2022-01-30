(defpackage cl-input-interface
  (:use :cl)
  (:nicknames :cl-ii)
  (:export
   :input-response
   :input-callback
   :make-input-handler
   :input-handler-flush
   :input-handler-add-bind
   :input-handler-input))

(in-package :cl-ii)


;;; (funcall T x) := (R {T}*)
;;; R := (t {S}*) | nil
;;; S := (t . "response") | (nil . "loop back")

(defstruct (input-handler (:constructor %make-input-handler))
  binds
  states
  candidate)

(defun input-response (response)
  (cons t response))
(defun input-callback (callback)
  (cons nil callback))

(defun input-response-p (x)
  (and (consp x) (car x)))
(defun input-callback-p (x)
  (and (consp x) (not (car x))))

(defun get-input-value (x)
  (or (input-response-p x) (input-callback-p x))
  (cdr x))

(defun make-input-handler ()
  (labels ((echo-back (echo)
             `((t ,(input-response echo)))))
    (%make-input-handler
     :binds (list #'echo-back)
     :states nil
     :candidate nil)))

(defun input-handler-flush (input-handler)
  (with-slots (binds states candidate) input-handler
    (unless candidate (return-from input-handler-flush nil))
    (setf states nil)
    (destructuring-bind (input-stack . results) candidate
      (setf candidate nil)
          (append (mapcan (lambda (result)
                            (cond ((input-response-p result)
                                   (list (get-input-value result)))
                                  ((input-callback-p result)
                                   (input-handler-input
                                    input-handler (get-input-value result)))
                                  (t (error "invalid result")))) 
                          results)
                  (mapcan (lambda (input)
                            (input-handler-input input-handler input))
                          (reverse input-stack))))))

(defun input-handler-add-bind (input-handler bind)
  (check-type input-handler input-handler)
  (with-slots (binds) input-handler
    (push bind binds))
  input-handler)

(defun input-handler-input (input-handler input)
  (check-type input-handler input-handler)
  (unless (input-handler-states input-handler)
    (setf (input-handler-states input-handler)
          (input-handler-binds input-handler)))
  (let* ((called (mapcar (lambda (f) (funcall f input))
                         (input-handler-states input-handler)))
         (result (some #'identity (mapcar #'car called)))
         (next-states (mapcan #'cdr called)))
    (unless (or result next-states)
      (return-from input-handler-input
        (append (input-handler-flush input-handler)
                (input-handler-input input-handler input))))
    (if result
        (setf (input-handler-candidate input-handler)
              (cons nil (cdr result)))
        (push input (car (input-handler-candidate input-handler))))
    (cond (next-states
           (setf (input-handler-states input-handler) next-states)
           nil)
          (t (input-handler-flush input-handler)))))