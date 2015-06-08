(defun hello-world () (format t "hello world,fjc"))

(defun test ()
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p)))

(defmacro do-primes ((var start end) &body body)
(let ((ending-value-name (gensym)))  
  `(do (
	(,var (next-prime ,start) (next-prime (1+ ,var)))
	(,ending-value-name ,end))
       ((> ,var ,ending-value-name))
     ,@body)))

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4))) 

