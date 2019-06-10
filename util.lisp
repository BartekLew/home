
(defun pos-not-escaped (char input &optional (pos 0))
(cond	((>= pos (length input)) nil)
	((eql (char input pos) char) pos)
	((eql (char input pos) #\\) (pos-not-escaped char input (+ pos 2)))
	(t (pos-not-escaped char input (+ pos 1)))))


(defun pos-not-escaped+ (chars input &optional (pos 0))
(cond	((>= pos (length input)) nil)
	((find (char input pos) chars) pos)
	((eql (char input pos) #\\) (pos-not-escaped+ chars input (+ pos 2)))
	(t (pos-not-escaped+ chars input (+ pos 1)))))


(defun chars-count (chars input &optional (pos 0))
	(if (>= pos (length input)) 0
	(let ((c (char input pos)))
	(if (find c chars) (+ (chars-count chars input (+ pos 1)) 1) 0))))


(defmacro try-classes (classes validator &rest params)
(cons 'or (loop for class in classes collect
	`(let ((inst (make-instance ,class ,@params)))
	(if (,validator inst) inst)))))

(defmacro !+ (class &body body)
	`(make-instance ,class ,@body))

;; join elements of list using joiner class.
(defun join (joiner list)
	(if (eql list nil) '()
	(if (eql (second list) nil) (car list)
	(apply joiner (list (car list) (join joiner (cdr list)))))))

;; standard string joiner
(defmacro sep (str)
	`(lambda (a b) (format nil "~A~A~A" a ,str b)))

(defun read-avail (stream)
	(if (not (listen stream)) '()
	(let ((l (read-line stream nil :eof)))
	(if (eql l :eof) '(:eof)
	(cons l (read-avail stream))))))

(defun read-until (stream end-tag)
	(let ((l (read-line stream)))
	(if (string= l end-tag) '()
	(cons l (read-until stream end-tag)))))

(defmacro >f (file string)
	`(with-open-file (o ,file :direction :output :if-exists :supersede)
		(format o "~A~%" ,string)))

(defmacro <f (file &key until)
	`(with-open-file (i ,file :direction :input :if-does-not-exist nil)
		(if i ,(if until `(read-until i ,until)
		'(read-avail i)) '(:error))))

;; The result is passed to the next call, for first char, acc
;; parameter is passed.
(defun redstr (fun acc str)
	(if (equal str "") acc
	(let ((v (apply fun (list acc (char str 0)))))
	(redstr fun v (subseq str 1)))))


;; Try different options to match x. Options are functions accepting
;; parameter of the same type as x. Trying stops on first option
;; returning non-NIL
(defun match (x options)
	(and options
		(let ((special (apply (car options) (list x))))
		(or special (match x (cdr options))))))

(defun white? (c)
	(find c '(#\Space #\Tab #\Newline #\Return)))

(defun 1st (pred rdl)
	(let ((e (apply rdl '())))
	(if e (if (apply pred (list e)) e (1st pred rdl)))))
	
