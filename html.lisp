(defun tag>html (tag)
	(if (string= (second tag) "") ""
	(let ((type (first tag)) (val (second tag)))
	(format nil "<~A>~A</~A>" type val type))))

(defun art>html (art)
	(if (eql art '()) ""
	(concatenate 'string
		(tag>html (car art))
		(art>html (cdr art)))))

(defun params>str (list)
	(if (eql list '()) ""
	(let* ((nkey (car list))
		(r1 (cdr list))
		(nval (car r1))
		(rest (cdr r1)))
	(format nil " ~A=\"~A\"~A" nkey nval (params>str rest)))))

(defun tag> (type params content)
	(let ((parstr (params>str params)))
	(if (eql content '()) (format nil "<~A~A/>~%" type parstr)
	(format nil "<~A~A>~%~A~%</~A>~%" type parstr (content> content) type))))

(defun tags> (l)
	(if (eql l '()) ""
		(let ((f (first l)))
		(concatenate 'string
			(tag> (first f) (second f) (third f))
			"" (tags> (cdr l))))))

(defun content> (x)
	(if (stringp x) x
	(if (stringp (first x))
		(tag> (first x) (second x) (third x))
	(tags> x))))

(defun join (joiner list)
	(if (eql list nil) '()
	(if (eql (second list) nil) (car list)
	(apply joiner (list (car list) (join joiner (cdr list)))))))

(defun css-rule (selector rules)
	(format nil "~A {~%	~A~%}~%" selector
		(join #'(lambda (a c) (format nil "~A;~%	~A" a c)) rules)))

(defun stylesheet (rules)
	(join (lambda (a c) (format nil "~A~%~A" a c))
		(mapcar (lambda (x) (css-rule (first x) (second x))) rules)))

(defun copyleft ()
	(tag> "span" '("style" "display:inline-block; transform:rotate(180deg)") "&copy;"))

(defun template (body style footer)
	(tag> "html" '() `(
		("head" () (
			("meta" ("charset" "utf-8") ())
			("meta" ("name" "viewport" "content" "width=device-width, initial-scale=1.0") ())
			("style" ("type" "text/css") ,style)
		))
		("body" () (
			("div" ("id" "art") ,body)
			("div" ("id" "footer") ,footer))))))


