(defun memproc (proc)
  (let ((already-run nil)
	(result nil))
    (lambda ()
      (if (not already-run)
	  (progn
	    (setf already-run t)
	    (setf result (funcall proc))
	    result)
	  result
	  )
      )
    )
  )
  
(defmacro delay (expr)
  `(memproc (lambda () ,expr)))

(defun force (func)
  (funcall func))

(defmacro cons-stream (head tail)
  `(cons ,head
	 (delay ,tail)))

(defun head (stream)
  (car stream))

(defun tail (stream)
  (force (cdr stream))
)

(defun add-stream (s1 s2)
  (cons-stream (+ (head s1) (head s2))
	      (add-stream (tail s1) (tail s2))))
(defparameter fib
  (cons-stream 1
	       (cons-stream 2
			    (add-stream fib (tail fib)))))

(defparameter ones
  (cons-stream 1
	       ones ))
(defparameter intg
  (cons-stream 1
	       (add-stream intg ones)))
 
(defun nth-stream (s n)
  (format t "~a~%" (head s))
  (if (> n 0)
      (nth-stream (tail s) (- n 1)))
      (head s))


(defun test ()
  (nth-stream ones 100)
  (nth-stream intg 100)
  (nth-stream fib  100))