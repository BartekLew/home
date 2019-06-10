(load "html.lisp")
(load "util.lisp")

	
(defclass lines ()
	((remaining :initarg :<  :initform '())))

(defmethod print-object ((o lines) stream)
	(with-slots (remaining) o
	(format stream "<LINES (~A...)>" (car remaining))))

(defgeneric rd (input))
(defmethod rd ((i lines))
	(with-slots (remaining) i
	(if remaining (let ((l (first remaining)))
			(setf remaining (cdr remaining))
			l)
		nil)))

(defun headr? (line)
	(not (position-if-not (lambda (c) (eql c #\=)) line)))

(defclass chunk ()
	((content :initarg := :initform '())))

(defmethod initialize-instance :after ((this chunk) &key loader)
	(with-slots (content) this
	(setf content (cons content
		(loop for x = (apply loader '())
			until (apply (orf #'not #'blank-line?) `(,x))
			collect x)))))

(defgeneric >tag (src))
(defmethod >tag ((c chunk))
	(with-slots (content) c
	(if (first content)
	(if (headr? (car (last content)))
		(!+ 'tag := "h1" :< (join (sep #\Newline) (subseq content 0 (- (length content) 1))))
		(!+ 'tag := "p" :< (join (sep #\Newline) content))))))

(defun chunk (i)
	(flet ((rdl () (rd i)))
	(let*	((l (1st (orf #'not #'nonblank-line?) #'rdl)))
	(!+ 'chunk := l :loader #'rdl))))

(defun chunks (lines)
	(loop for x = (>tag (chunk lines))
		until (eql x nil)
		collect x))

;; document class
(defclass document ()
	(content
	(title :initarg :title :initform "Peace!")
	(style :initarg :style :initform '())
	(footer :initarg :footer :initform '())
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
	(with-slots (content title style init footer) d
	(html (!+ 'tag := "html" :< (list
		(!+ 'tag := "head" :< (list
			(!+ 'tag := "title" :< title)
			(!+ 'tag := "meta" :& '("charset" "utf-8"))
			(!+ 'tag := "meta" :& '("name" "viewport" "content" "width=device-width, initial-scale=1.0"))
			(!+ 'tag := "style" :& '("type" "text/css") :< (stylesheet (append *base-style* style)))
			(if init (!+ 'tag := "script" :& '("type" "text/javascript") :< (js (as-function "init" init))))
		))
		(!+ 'tag := "body" :& (if init '("onload" "init()") nil)
			:< (list (!+ 'tag := "div" :& '("id" "art") :< content)
				(!+ 'tag := "div" :& '("id" "footer") :< footer))))))))

