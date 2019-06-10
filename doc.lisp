(load "html.lisp")
(load "util.lisp")

	
(defclass lines ()
	((remaining :initarg :<  :initform '())))

(defgeneric rd (input))
(defmethod rd ((i lines))
	(with-slots (remaining) i
	(if remaining (let ((l (first remaining)))
			(setf remaining (cdr remaining))
			l)
		nil)))

(defun headr? (line)
	(not (position-if-not (lambda (c) (eql c #\=)) line)))

(defun chunk (i)
	(flet ((rdl () (rd i)))
	(let*	((l (1st (lambda (x) 
			(or (not x) (position-if-not #'white? x))) #'rdl))
		(chunk (cons l (loop for x = (rdl)
				until (or (not x) (not (position-if-not #'white? x)))
				collect x))))
	(cond ((eql (car chunk) nil) nil)
		((headr? (car (last chunk)))
			(!+ 'tag := "h1" :< (join (sep #\Newline) (subseq chunk 0 (- (length chunk) 1)))))
		(t (!+ 'tag := "p" :< (join (sep #\Newline) chunk)))))))

(defun chunks (lines)
	(loop for x = (chunk lines)
		until (eql x nil)
		collect x))

;; document class
(defclass document ()
	(content
	(title :initarg :title :initform "Peace!")
	(style :initarg :style :initform '())
	(init :initarg :init :initform '())))

(defmethod initialize-instance :after((this document) &key from-file by-val)
	(if from-file (setf (slot-value this 'content)
		(let ((lines (<f from-file)))
			(if (eql (first lines) :error) 
				(progn (setf (slot-value this 'title) "file not found!")
				'("p" '("style" "color: #b03030") "File not found!"))
			(chunks (!+ 'lines :< lines)))))
	(if by-val (setf (slot-value this 'content) by-val))))

(defmethod html ((d document))
	(with-slots (content title style init) d
	(html (!+ 'tag := "html" :< (list
		(!+ 'tag := "head" :< (list
			(!+ 'tag := "title" :< title)
			(!+ 'tag := "meta" :& '("charset" "utf-8"))
			(!+ 'tag := "meta" :& '("name" "viewport" "content" "width=device-width, initial-scale=1.0"))
			(!+ 'tag := "style" :& '("type" "text/css") :< (stylesheet (append *base-style* style)))
			(if init (!+ 'tag := "script" :& '("type" "text/javascript") :< (js (as-function "init" init))))
		))
		(!+ 'tag := "body" :& (if init '("onload" "init()") nil)
			:< (!+ 'tag := "div" :& '("id" "art") :< content)))))))

