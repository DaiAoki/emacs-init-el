(defun temp_test ()
  (loop for i
     from 5
     to 10
     sum i)
  (loop for i
     in '(100 20 3)
     sum i)
  (loop for i
     below 5
     do (print i))
  (loop for i
     below 10
     when (oddp i)
     sum i)
  )

(defun temp_test2 ()
  (loop for x below 10
     for y below 10
     collect (+ x y))
  )

(defun temp_test3 ()
  (loop for x below 10
     collect (loop for y below 10
		collect (+ x y))))

