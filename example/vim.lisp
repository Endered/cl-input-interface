(ql:quickload :cl-input-interface)
(use-package :cl-ii)

(defun make-vim-map (source destination response)
  (check-type source string)
  (check-type destination string)
  (assert (plusp (length source)))
  (let* ((rev (reverse source))
         (first-char (aref rev 0))
         (remain (subseq rev 1)))
    (reduce (lambda (a b)
              (lambda (s)
                (if (eq b s)
                    (list nil a)
                    (list nil))))
            remain
            :initial-value
            (lambda (s)
              (if (eq s first-char)
                  `((t ,@(map 'list response destination)))
                  (list nil))))))

(defun make-map (source destination)
  (make-vim-map source destination #'input-callback))
(defun make-noremap (source destination)
  (make-vim-map source destination #'input-response))

(defun ->list (string)
  (coerce string 'list))

(defun ->string (string)
  (coerce string 'string))

(defun example ()
  (let ((input-handler (make-input-handler)))
    (input-handler-add-bind
     input-handler
     (make-map "fd" "ESC"))
    (input-handler-add-bind
     input-handler
     (make-map "@w" "[WINDOW]"))
    (input-handler-add-bind
     input-handler
     (make-map "[WINDOW]a" "HOGEEE"))
    (input-handler-add-bind
     input-handler
     (make-map "[WINDOW]" "NON"))
    (dolist (inputs (list "fd" "@wb" "@wa" "#ifdef"))
      (format t "INPUT: ~a~%" inputs)
      (dolist (input (->list inputs))
        (format t "~a: ~a~%" input (->string (input-handler-input input-handler input))))
      (format t "flush: ~a~%" (->string (input-handler-flush input-handler)))
      (terpri))))

(example)