(load "html.lisp")
(load "util.lisp")
(load "string.lisp")


(defclass chunk ()
	((value :initarg := :initform nil :accessor value)
	(tail :initform nil :accessor tail)))

(defmethod value ((x (eql nil))) nil)

(defun taillen (chunk)
	(if (not chunk) 0
		(+ 1 (taillen (tail chunk)))))

(defun chunktail (&rest chunks)
	(if (car chunks) (let ((head (car chunks)))
		(setf (tail head) (apply #'chunktail (cdr chunks)))
		head)))

(defmethod print-object ((this chunk) out)
	(format out "<~A '~A'/~A>" (type-of this) (s/- (value this) 20)
		(if (listp (tail this)) (length (tail this)) (taillen (tail this)))))

(defclass paragraph (chunk) ())
(defclass blank (chunk) ())

(defmacro line-type (name &body pred)
	`(progn (defclass ,name (chunk) ())
	(defun ,name (input)
		(if ,@pred (!+ ',name := input)))))


(line-type blank (not (position-if-not #'white? input)))
(line-type header-line (not (pos-not #\= input)))
(line-type keyval-line (string= (s/- input 2) "!!"))
(line-type blockcode-line (string= "```" input))


(defmethod initialize-instance :after ((this keyval-line) &key)
	(with-slots (value) this
	(let* ((colon (pos-not-escaped #\: value))
		(valpos (position-if-not #'white? value :start (+ colon 1))))
	(setf value `(,(subseq value 2 colon) ,(subseq value valpos))))))


(defun >line (input)
	(match input (list 'blank 'header-line 'keyval-line 'blockcode-line
			(lambda (x) (make-instance 'paragraph := x)))))
	

(defclass header(chunk) ())

(defgeneric page-bookmark (element))
(defmethod page-bookmark ((h header))
	(string-downcase (~format (value h)
		`(,(!+ 'spechar := #'white? :! (replace-fun "_"))))))

(defgeneric link (element))
(defmethod link ((this header))
	(!+ 'tag := "a" :& (list "href" (s+ "#" (page-bookmark this)))
		:< (value this)))

(defclass code-block(chunk) ((closed :initform nil :accessor closed)))

(defgeneric chunk+ (a b))

(defmethod chunk+ (a (b (eql nil))) a)
(defmethod chunk+ ((a (eql nil)) b) b)
(defmethod chunk+ ((a chunk) (b chunk)) nil)

(defmethod chunk+ ((a chunk) (b list))
	(if (not b) a
	(let ((con (chunk+ a (car b))))
	(if con (chunk+ con (cdr b))
		(progn (setf (tail a) b) a)))))

(defmethod chunk+ ((a paragraph) (b paragraph))
	(setf (value a) (format nil "~A~%~A" (value a) (value b)))
	a)

(defmethod chunk+ ((a blank) (b blank)) a)

(defmethod chunk+ ((a paragraph) (b header-line))
	(!+ 'header := (value a)))

(defmethod chunk+ ((a paragraph) (b blank)) nil)

(defmethod chunk+ ((a blockcode-line) (b paragraph))
	(!+ 'code-block := (value b)))

(defmethod chunk+ ((a code-block) (b paragraph))
	(if (not (closed a))
		(!+ 'code-block := (format nil "~A~%~A" (value a) (value b)))))

(defmethod chunk+ ((a code-block) (b blockcode-line))
	(setf (closed a) T) a)

(defmethod chunk+ ((a blockcode-line) (b keyval-line))
	(let ((kv (value b)))
	(!+ 'code-block := (format nil "!!~A: ~A" (first kv) (second kv)))))

(defmethod chunk+ ((a code-block) (b keyval-line))
	(if (not (closed a))
		(let ((kv (value b)))
		(!+ 'code-block := (format nil "~A~%!!~A: ~A" (value a) (first kv) (second kv))))))

(defmethod chunk+ ((a code-block) (b blank))
	(if (not (closed a))
		(progn (setf (value a) (format nil "~A~%" (value a))) a)))

(defgeneric chunk (lines &optional acc))

(defmethod chunk ((lines list) &optional acc)
	(if (not lines) acc
	(chunk+ (chunk+ acc (car lines)) (cdr lines) )))

(defun chunks (lines)
	(if lines
	(let ((head (chunk lines)))
	(if (tail head) (setf (tail head) (chunks (tail head))))
	head)))

(defgeneric chunk= (c1 c2))

(defmethod chunk= ((c1 chunk) (c2 chunk))
	(and (eql (type-of c1) (type-of c2))
		(equal (value c1) (value c2))))

(defmethod chunk= ((l1 list) (l2 list))
	(let ((result T))
	(loop for a in l1
		for b in l2
		collect (unless (chunk= a b) (setf result nil)))
	result))

(defgeneric chunks= (cs1 cs2))

(defmethod chunks= (a b)
	(and (not a) (not b)))

(defmethod chunks= ((cs1 chunk) (cs2 chunk))
	(and (chunk= cs1 cs2) (chunks= (tail cs1) (tail cs2))))

(defvar *paragraph-spechars* (append *default-spechars* `(
	,(!+ 'spechar := #\\ :! (lambda (iter)
		(with-slots (text pos) iter
		(setf text (s+ (subseq text 0 pos) (subseq text (+ 1 pos))))
		iter)))
	,(!+ 'spechar := #\` :! (region-tag-fun #\`
		(lambda (content)
			(!+ 'tag := "span" :& '("class" "inline-code") :< content))))
	,(!+ 'spechar := #\% :! (region-tag-fun #\%
		(lambda (content)
			(!+ 'tag := "i" :< content)))))))

(defgeneric >tags (chunk))
(defmethod >tags ((c chunk))
	(if (tail c) (>tags (tail c))))

(defmethod >tags ((p paragraph))
	(cons (!+ 'tag := "p" :< (~format (value p) *paragraph-spechars*)) (call-next-method)))

(defmethod >tags ((h header))
	(cons (!+ 'tag := "h2" :& `("id" ,(page-bookmark h))
		:< (~format (value h) *paragraph-spechars*)) (call-next-method)))

(defmethod >tags ((c code-block))
	(cons (!+ 'tag := "pre" :< (!+ 'tag := "code" :< (~format (value c)))) (call-next-method)))

(defmethod >tags ((k keyval-line))
	(if (string= (first (value k)) "AUDIO")
		(cons (!+ 'tag := "audio" :&- "controls"
			:< (!+ 'tag := "source" :& `("src" ,(second (value k))
						"type" "audio/mpeg"))) 
			(call-next-method))
		(call-next-method)))


;; document class
(defclass document ()
	(content
	(title :initarg :title :initform "Peace!")
	(style :initarg :style :initform '())
	(footer :initarg :footer :initform '())
	(init :initarg :init :initform '())))

(defgeneric with-content (doc chunk))

(defun chunk-iterator (chunk)
	(let ((cur chunk))
	(lambda () (if cur (let ((x cur))
				(setf cur (tail cur))
				x)))))
(defgeneric toc? (chunks))
(defmethod toc? ((ch chunk))
	(let* ((iter (chunk-iterator ch))
		(chunks (loop for x = (apply iter nil) until (not x) collect x))
		(toc-title (second (value (car (remove-if-not (lambda (x) (and (listp (value x)) (string= (first (value x)) "TOC"))) chunks))))))
	(if toc-title
		(!+ 'tag := "div" :& '("id" "toc")
			:< (list (!+ 'tag := "h3" :< toc-title) (!+ 'tag := "ol"
				:< (mapcar (lambda (x) (!+ 'tag := "li" :< (link x)))
					(remove-if-not (lambda (x) (eql (type-of x) 'header)) chunks))))))))
		
(defmethod with-content ((this document) (chunks header))
	(setf (slot-value this 'title) (value chunks))
	(with-content this (tail chunks)))

(defmethod with-content ((this document) (chunks chunk))
	(with-slots (content) this
	(setf content (cons (toc? chunks) (>tags chunks)))))
	
(defmethod initialize-instance :after((this document) &key from-file by-val)
	(if from-file (setf (slot-value this 'content)
		(let ((lines (<f from-file)))
			(if (eql (first lines) :error) 
				(progn (setf (slot-value this 'title) "file not found!")
				'("p" '("style" "color: #b03030") "File not found!"))
			(with-content this (chunks (mapcar #'>line lines))))))
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
			:< (list (!+ 'tag := "div" :& '("id" "art") 
					:< (cons (!+ 'tag := "h1" :< title) content))
				(!+ 'tag := "div" :& '("id" "footer") :< footer))))))))

