(load (merge-pathnames "util.lisp" *load-truename*))
(include "doc.lisp")

(defmacro line-case (input out-type &optional (expected input))
	`(let ((out (>line ,input)))
	(unless (and (eql (type-of out) ,out-type)
			(equal (value out) ,expected))
		(format t "TEST FAILED (~a -> ~a:~a)~%	result is ~a(~a)~%"
			,input ,expected ,out-type (type-of out) (value out)))))

(doList (test '(("foo" paragraph) ("bar baz uu" paragraph)
		("" blank nil) ("	" blank nil)
		("    " blank nil) ("=====" header-line)
		("!!AUDIO: foo.mp3" keyval-line ("AUDIO" "foo.mp3"))
		("```" blockcode-line)))

	(if (third test) (line-case (first test) (second test) (third test))
		(line-case (first test) (second test))))


(defmacro chunk-case (lines out-type out-value)
	`(let ((out (chunk (mapcar #'>line ,lines))))
	(unless (and (eql (type-of out) ,out-type)
			(equal ,out-value (value out)))
	(format t "TEST FAILED (~a -> ~a:~a)~%	result is ~a:~a~%"
		,lines ,out-value ,out-type (value out) (type-of out)))))

(doList (test `(
		(("foo" "bar" "baz") paragraph ,(format nil "foo~%bar~%baz"))
		(("foo") paragraph "foo")
		(("" " " "   ") blank "")
		(("foo" "bar" "====") header ,(format nil "foo~%bar"))
		(("```" "code" "is" "comming here" "```") code-block
			,(format nil "code~%is~%comming here"))))
	(chunk-case (first test) (second test) (third test)))

(defmacro chunk-tail-case (lines tail)
	`(let ((out (chunk (mapcar #'>line ,lines))))
	(unless (chunk= ,tail (tail out))
		(format t "TEST FAILED (tail~A = ~A)~%	result is: ~A~%"
			,lines ,tail (tail out)))))

(doList (test `(
		(("foo" "bar" "baz" "" "umma") ,(mapcar '>line '("" "umma")))
		(("foo" "bar" "===" "uffa" "la") ,(mapcar '>line '("uffa" "la")))
		(("!!FOO: BAR" "wookaa" "42") ,(mapcar '>line '("wookaa" "42")))
		(("foo" "bar") nil)))
	(chunk-tail-case (first test) (second test)))

(defmacro chunks-case (lines output)
	`(let ((out (chunks (mapcar #'>line ,lines))))
	(unless (chunks= out ,output)
		(format t "TEST FAILED (~A -> ~A)~%	result is: ~A~%"
			,lines ,output out))))

(doList (test `(
		(("foo" "bar" "" "ur" "===" "uaa") 
			,(chunktail (!+ 'paragraph := (s+ln "foo" "bar"))
				(!+ 'blank := "") (!+ 'header := "ur")
				(!+ 'paragraph := "uaa")))
		(("A title" "===" "" "par1" "par1" "" "par2" "" "h2" "====" "u")
			,(chunktail (!+ 'header := "A title") (!+ 'blank := "")
				(!+ 'paragraph := (s+ln "par1" "par1"))
				(!+ 'blank := "") (!+ 'paragraph := "par2") (!+ 'blank := "")
				(!+ 'header := "h2") (!+ 'paragraph := "u")))))
	(chunks-case (first test) (second test)))


(defmacro tags-case (input output)
	`(let ((out (>tags (chunks (mapcar #'>line ,input)))))
	(unless (tag= out ,output)
		(format t "TEST FAILED (~A -> ~A)~%	result is: ~A~%"
			,input ,output out))))

(doList (test `(
		(("foo" "bar") (,(!+ 'tag := "p" :< (s+ln "foo" "bar"))))
		(("ka" "===" "boom" "aa" "" "u")
			,(list (!+ 'tag := "h1" :< "ka")
				(!+ 'tag := "p" :< (s+ln "boom" "aa"))
				(!+ 'tag := "p" :< "u")))))
	(tags-case (first test) (second test)))

(test-case ("foo baria" string= "foo baria")
	(value (!+ 'string-iterator :< input)))

(test-case ("This is quite longer text :" equal '(#\h #\i #\s #\Space #\i #\s))
	(let ((i (!+ 'string-iterator :< input)))
	(next i)
	(loop for x from 0 to 5
		collect (next i))))

(test-case ("Another text" equal '("Another" "text"))
	(value (split-text (!+ 'string-iterator :< input) 7)))

(test-case ("12345678" equal '("123" "4" "5678"))
	(value (push-back (split-text (!+ 'string-iterator :< input) 3) "4")))

(test-case (("foo" "bar" "baz") equal "baz")
	(text (discard-text (!+ 'split-string-iterator :<+ input :< "  baz") 0 2)))
	
(sb-ext:quit)
