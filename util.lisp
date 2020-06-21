(defvar *pwd* ".")

(defmacro include (file)
	`(load (merge-pathnames ,file *load-truename*)))

(defun pos-not-escaped (char input &optional (pos 0))
(cond	((>= pos (length input)) nil)
	((eql (char input pos) char) pos)
	((eql (char input pos) #\\) (pos-not-escaped char input (+ pos 2)))
	(t (pos-not-escaped char input (+ pos 1)))))

(defun pos-not-escaped-if (pred input &optional (pos 0))
(cond	((>= pos (length input)) nil)
	((apply pred (list (char input pos))) pos)
	((eql (char input pos) #\\) (pos-not-escaped-if pred input (+ pos 2)))
	(t (pos-not-escaped-if pred input (+ pos 1)))))

(defun pos-not-escaped+ (chars input &optional (pos 0))
(cond	((>= pos (length input)) nil)
	((find (char input pos) chars) pos)
	((eql (char input pos) #\\) (pos-not-escaped+ chars input (+ pos 2)))
	(t (pos-not-escaped+ chars input (+ pos 1)))))

(defmacro pos-not (char input &rest rest)
	`(position-if-not (lambda (c) (eql c ,char)) ,input ,@rest))

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

(defmacro !++ (&rest params)
	`(apply 'make-instance ',params))

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
	
(defun nonblank-line? (line)
	(position-if-not #'white? line))

(defun blank-line? (line)
	(not (nonblank-line? line)))


(defmacro orf (&rest funs)
	`(lambda (&rest args) ,(cons 'or (loop for x in funs
					collect `(apply ,x args)))))

(defmacro s+ (&rest strings) `(concatenate 'string ,@strings))

(defmacro s+ln (&rest strings)
	(join (sep #\Newline) strings))

(defmacro cuts (string length)
	`(if (< ,length (length ,string))
		(subseq ,string 0 ,length)
		,string))

(defun outf (name)
	(let ((extpos (position #\. name :from-end t)))
	(concatenate 'string (subseq name 0 extpos) ".html")))

(defun linkf (name)
	(subseq (outf name) (length *pwd*)))

(defun filedir (name)
	(let ((pos (position #\/ name :from-end t)))
	(if (not pos) "./"
	(subseq name 0 (+ pos 1)))))

(defun files (glob)
	(if (listp glob) (apply #'concatenate (cons 'list (loop for x in glob collect (files x))))
	(let* ((p (sb-ext:run-program "/bin/sh" (list "-c" (s+ "ls " *pwd* glob)) :output :stream))
		(out (sb-ext:process-output p)))
	(loop for x = (read-line out nil :eof) until (eql x :eof) collect x))))

(defmacro +constructor (type keys &body body)
	`(defmethod make-instance :around ((type (eql (find-class ',type))) &key ,@keys)
		,@body))

(defun times-str (str n)
	(if (= n 0) "" (s+ str (times-str str (- n 1)))))

(defmacro +simple-printer (type &body fields)
	`(defmethod print-object ((this ,type) out)
		,(append `(format out (s+ "<~S" (times-str " ~S" (length ',fields)) ">")
			 (type-of this))
			(loop for x in fields collect `(slot-value this ,x)))))

(defun list-iterator (list)
	(let ((next list))
	(lambda ()
		(let ((ret (car next)))
		(setf next (cdr next))
		ret))))

(defmacro test-case ((input test-fun expected-output) &body body)
	`(let ((output (apply (lambda (input) ,@body) '(,input))))
	(unless (apply #',test-fun (list ,expected-output output))
		(format t "TEST FAILED: ~S -> ~S != ~S~%	action: ~S~%"
			',input ,expected-output output ',body))))

(defmacro test-case* ((test-fun expected-output) &body body)
	`(let ((output (apply (lambda () ,@body) '())))
	(unless (apply #',test-fun (list ,expected-output output))
		(format t "TEST FAILED: ~S != ~S~%	action: ~S~%"
			,expected-output output ',body))))
(defun apply* (fun args)
	(handler-case
		(apply fun args)
		(error (e) (warn (format nil "~A~A: ~A" fun args e)) nil)))

(defun hash[] (key hash)
	(if (not hash) nil
		(gethash key hash)))

(defun !hash (&optional (elements nil) (acc (make-hash-table)))
	(if (not elements) acc
	(let ((e (first elements)))
	(setf (gethash (first e) acc) (second e))
	(!hash (cdr elements) acc))))

(defmacro for-val (val test &body body)
	`(lambda (x) (if (funcall ,test x ,val) (or ,@body T))))

(defun filteriter (iter test)
	(let ((next (funcall iter)))
	(if (and iter (not (apply test (list next))))
		(filteriter iter test)
		next)))

(defun splitstr (str delim &optional acc)
	(if (> (length str) 0) (let ((p (position delim str)))
			(if p (splitstr (subseq str (+ p 1)) delim (append acc (list (subseq str 0 p))))
				(append acc (list str))))
		acc))

(defun relative-path (base file)
	(labels ((skip-common (a b)
		(if (or a b)
			(if (string= (car a) (car b)) (skip-common (cdr a) (cdr b))
				(list a b)))))
	(let ((diff (skip-common (splitstr base #\/) (splitstr file #\/))))
		(if diff
			(s+ (times-str "../" (- (length (car diff)) 1))
				(join (sep #\/) (second diff)))))))

(define-condition not-in-category (error)
  ((category :initarg :in :reader catg)
   (key :initarg key :reader key)))

(defmacro category (name &optional default)
  `(let ((domain (make-hash-table)))
    (defun ,name (key)
      (let ((ans (gethash key domain)))
        (or ans (error 'not-in-category :in ',name :key key))))
    (defun (setf ,name) (val key)
      (setf (gethash key domain) val))))

(defun @ (tree idx)
  (if (not idx) tree
    (@ (nth (first idx) tree) (rest idx))))

(defun listp+ (x)
  (and x (listp x)))

(defun exp-symbol (form sym value &optional acc)
  (cond ((not form) (reverse acc))
        ((eql sym (first form))
           (exp-symbol (rest form)
                       sym value
                       (cons value acc)))
        ((listp+ (first form))
             (exp-symbol (rest form) sym value
                         (cons (exp-symbol (first form) sym value) acc)))
        (t (exp-symbol (rest form)
                          sym value
                          (cons (first form) acc)))))

(defmacro f (args &body body)
  `(labels ((self ,args ,@body))
     (lambda (&rest args)
       (apply #'self args))))

(defun curry (fun &rest pre-args)
  (lambda (&rest args)
    (apply fun (append pre-args args))))

(defun sh-name (name)
  (cond ((symbolp name) (string-downcase (format nil "~A" name)))
        ((stringp name) (format nil "~A" name))
        (t (format nil "~A" name))))

(defun os-run (args)
  (let ((proc (sb-ext:run-program (first args)
                                  (mapcar #'sh-name (rest args)) :output :stream)))
    (if (= (process-exit-code proc) 0) proc
      (error 
                     (loop for x = (read-line (process-output proc) nil nil)
                           while (not (eq x nil))
                           collect x)))))
