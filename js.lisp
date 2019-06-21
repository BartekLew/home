(load (merge-pathnames "util.lisp" *load-truename*))

(defclass js ()
	((recipee :initarg := :reader js)))

(defclass js-node (js) ())
(defclass js-array (js) ())
(defclass js-statement (js) ())

(defclass js-function (js)
	((name :initarg :name :initform nil)
	(args :initarg :args :initform nil)
	(body :initarg :body)))

(defmethod js ((fun js-function))
	(with-slots (name args body) fun
	(format nil "function ~A(~A) {~%~A~%}"
		name (or (join (sep #\Space) args) "") (js body))))

(defgeneric as-function (name body))
(defmethod as-function ((name string) (body js-statement))
	(!+ 'js-function :name name :body body))

(defun js-select (&key class)
(if class (!+ 'js-array := (format nil
		"Array.from(document.getElementsByClassName(\"~a\"))" class))))

(defgeneric each (set var-name &rest commands))
(defmethod each ((arr js-array) id &rest commands)
	(!+ 'js-statement := (format nil "~A.forEach(function(~A) {~A~%});" (js arr) id
		(join (sep #\Newline) commands))))
		
(defgeneric hide (what))
(defmethod hide ((elements js-array))
	(each elements "e" "e.innerHTML = \"...\";"))

(defclass js-iterator ()
	((action :initarg :do)))
