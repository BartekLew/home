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
(defclass code-chunk (chunk) ())

(defgeneric chunk-limiter (chunk))
(defmethod chunk-limiter ((chunk chunk))
	#'blank-line?)

(defmethod chunk-limiter ((chunk code-chunk))
	(lambda (line) (string= line "```")))

(defmethod initialize-instance :after ((this chunk) &key loader)
	(with-slots (content) this
	(let ((rest (loop for x = (apply loader '())
			until (apply (orf #'not (chunk-limiter this)) `(,x))
			collect x)))
	(setf content (if content (cons content rest)
				rest)))))

(defun modtag (mod content)
	(cond	((eql mod #\`) (!+ 'tag := "span" :& '("class" "inline-code") :< content))
		((eql mod #\%) (!+ 'tag := "i" :< content))))

(defun ~format (string)
	(if (string= string "") '()
	(let ((modpos (position-if (lambda (c) (find c '(#\` #\%))) string)))
	(if modpos
	(if (and (> modpos 0) (eql #\\ (char string (- modpos 1))))
		(list (subseq string 0 (- modpos 1)) (subseq string modpos))
	(let*	((mod (char string modpos))
		(endpos (position mod string :start (+ modpos 1))))
	(append (list (subseq string 0 modpos)
		(modtag mod (subseq string (+ modpos 1) endpos))
		(~format (subseq string (+ endpos 1)))))))
	`(,string)))))
	
(defun merge-lines (lines)
	(~format (join (sep #\Newline) lines)))

(defgeneric >tag (src))
(defmethod >tag ((c chunk))
	(with-slots (content) c
	(if (first content)
	(if (headr? (car (last content)))
		(!+ 'tag := "h1" :< (merge-lines (subseq content 0 (- (length content) 1))))
		(!+ 'tag := "p" :< (merge-lines content))))))

(defmethod >tag ((chunk code-chunk))
	(with-slots (content) chunk
	(!+ 'tag := "pre" :< (!+ 'tag := "code" :< (join (sep #\Newline) content)))))

(defun chunk (i)
	(flet ((rdl () (rd i)))
	(let*	((l (1st (orf #'not #'nonblank-line?) #'rdl)))
	(if (string= l "```") (!+ 'code-chunk :loader #'rdl)
		(!+ 'chunk := l :loader #'rdl)))))

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

