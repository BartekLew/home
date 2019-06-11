(load "util.lisp")
(load "js.lisp")

;; Create css-rule string
(defun css-rule (selector rules)
	(format nil "~A {~%	~A~%}~%" selector
		(join #'(lambda (a c) (format nil "~A;~%	~A" a c)) rules)))


;; Take css-rules (in list of lists form) and build css stylesheet
(defun stylesheet (rules)
	(join (lambda (a c) (format nil "~A~%~A" a c))
		(mapcar (lambda (x) (css-rule (first x) (second x))) rules)))


;; Create tag representning COPYLEFT symbol.
(defun copyleft ()
	(!+ 'tag := "span" :& '("style" "display:inline-block; transform:rotate(180deg)") :< "&copy;"))

(defun params>str (list)
	(if (eql list '()) ""
	(let* ((nkey (car list))
		(r1 (cdr list))
		(nval (car r1))
		(rest (cdr r1)))
	(format nil " ~A=\"~A\"~A" nkey nval (params>str rest)))))

(defclass tag ()
	((type :initarg := :initform "span" :reader tagtype)
	(par :initarg :& :initform '())
	(boolpar :initarg :&- :initform "")
	(content :initarg :< :initform nil :reader content)))

(defmethod initialize-instance :after ((this tag) &key)
	(with-slots (type content) this
	(if (and (string= type "div") (eql content nil)) (setf content ""))))

(defmethod print-object ((this tag) out)
	(with-slots (type par content) this
	(format out "<TAG \"~A\" ~A: ~A>" type (params>str par) content)))

(defgeneric html (src))
(defmethod html ((text string))
	text)

(defmethod html ((this tag))
(with-slots (type par boolpar content) this
	(let ((parstr (params>str par)))
	(if (eql content '())
		(format nil "<~A~A ~A/>" type parstr boolpar)
	(if (or (string= type "span") (string= type "i"))
		(format nil "<~A~A ~A>~A</~A>" type parstr boolpar (html content) type)
	(format nil "<~A~A ~A>~%~A~%</~A>~%" type parstr boolpar (html content) type))))))

(defmethod html ((l list))
	(if l (concatenate 'string (html (car l)) (html (cdr l)))
		""))

(defvar *base-style* '(
	("body" ("background-color: #d0f0d0" "color: #000080"))
	("#art" ("max-width: 75ex" "margin-left: auto" "margin-right: auto"
		"margin-top:5em" "margin-bottom: 4em" "font-size: 14pt"))
	("h1" ("text-align: center" "margin-bottom: 2em" "font-size: 2em"))
	("h2" ("margin-top: 2em" "font-size: 1.5em" "font-weight: 700"))
	("p" ("line-height: 1.4" "text-indent: 1em" "margin-top: 0px"))
	("#footer" ("font-size: 0.9em" "color: #008040" "text-align:center"
		"margin-bottom: 2em"))
	("a" ("color: inherit" "font-size: inherit"))
	("code" ("word-break: break-any"
		"font-family: monospace" "color: #f0e0f0" "font-size: 0.8em"
		"padding: 2em" "padding-top: 1em" "display: block"
		"background-color: #004000" "margin-top: 2em" "font-weight:100"
		"margin-bottom: -0.5em"))
	("pre" ("margin-bottom: 0px" "padding: 0px"))
	(".inline-code" ("font-family: monospace" "font-size:0.9em"
		"font-weight: 600"))
	("audio" ("width:50%" "margin-left: 25%" "margin-top: 1em" "margin-bottom: 2em" ))
))

(defun addStyle (key vals)
	(setf *base-style* (append *base-style* `((,key ,vals)))))

