(load (merge-pathnames "util.lisp" *load-truename*))
(include "html.lisp")
(include "string.lisp")


(defclass chunk ()
	((value :initarg := :initform nil :accessor value)
	(tail :initform nil :accessor tail)))

(defun chunk-iterator (chunk)
	(let ((cur chunk))
	(lambda () (if cur (let ((x cur))
				(setf cur (tail cur))
				x)))))

(defmethod value ((x (eql nil))) nil)

(defun taillen (chunk)
	(if (not chunk) 0
		(+ 1 (taillen (tail chunk)))))

(defun chunktail (&rest chunks)
	(if (car chunks) (let ((head (car chunks)))
		(setf (tail head) (apply #'chunktail (cdr chunks)))
		head)))

(defmethod print-object ((this chunk) out)
	(format out "<~A '~A'/~A>" (type-of this) (value this)
		(if (listp (tail this)) (length (tail this)) (taillen (tail this)))))

(defclass paragraph (chunk) ())
(defclass blank (chunk) ())

(defmacro line-type (name &body pred)
	`(progn (defclass ,name (chunk) ())
	(defun ,name (input)
		(if ,@pred (!+ ',name := input)))))


(line-type blank (~= (rgx `(:nil ,/s* :nil)) input))
(line-type header-line (not (pos-not #\= input)))
(line-type subheader-line (not (pos-not #\- input)))
(line-type keyval-line (string= (cuts input 2) "!!"))
(line-type block-line (or (string= "```" input) (string= "\"\"\"" input)))

(defclass blockquote-line (block-line)())
(defclass blockcode-line (block-line)())

(defmethod make-instance ((class (eql (find-class 'block-line))) &key =)
	(if (string= = "```") (make-instance 'blockcode-line := =) 
		(make-instance 'blockquote-line := =)))

(defmethod initialize-instance :after ((this keyval-line) &key)
	(with-slots (value) this
	(let* ((colon (pos-not-escaped #\: value))
		(valpos (position-if-not #'white? value :start (+ colon 1))))
	(setf value `(,(subseq value 2 colon) ,(subseq value valpos))))))


(defun >line (input)
	(match input (list 'blank 'header-line 'subheader-line 'keyval-line 'block-line
			(lambda (x) (make-instance 'paragraph := x)))))
	
(defclass header(chunk) ((level :initarg :level :reader level)))

(defgeneric page-bookmark (element))
(defmethod page-bookmark ((h header))
	(string-downcase (~format (value h)
		`(,(!+ 'spechar := #'white? :! (replace-fun "_"))))))

(defgeneric link (element))
(defmethod link ((this header))
	(!+ 'tag := "a" :& (list "href" (s+ "#" (page-bookmark this)))
		:< (value this)))

(defclass code-block(chunk)
	((closed :initform nil :accessor closed)
	(block-type :initarg :block-type :reader block-type)))

(defmethod block-type ((x blockquote-line)) :quote)
(defmethod block-type ((x blockcode-line)) :code)

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
	(!+ 'header := (value a) :level 2))

(defmethod chunk+ ((a paragraph) (b subheader-line))
	(!+ 'header := (value a) :level 3))

(defmethod chunk+ ((a paragraph) (b blank)) nil)

(defmethod chunk+ ((a blockcode-line) (b paragraph))
	(!+ 'code-block := (value b) :block-type :code))

(defmethod chunk+ ((a blockquote-line) (b paragraph))
	(!+ 'code-block := (value b) :block-type :quote))

(defun line-len (text len)
	(if (> (length text) len)
		(format nil "~A~%~A" (subseq text 0 len) 
				(line-len (subseq text len) len))
		text))

(defmethod chunk+ ((a code-block) (b paragraph))
	(if (not (closed a))
		(!+ 'code-block := (format nil "~A~%~A" (value a) (line-len (value b) 76)) :block-type (block-type a))))

(defmethod chunk+ ((a code-block) (b block-line))
	(setf (closed a) T) a)

(defmethod chunk+ ((a block-line) (b keyval-line))
	(let ((kv (value b)))
	(!+ 'code-block := (format nil "!!~A: ~A" (first kv) (second kv)) :block-type (block-type a))))

(defmethod chunk+ ((a code-block) (b keyval-line))
	(if (not (closed a))
		(let ((kv (value b)))
		(!+ 'code-block := (format nil "~A~%!!~A: ~A" (value a) (first kv) (second kv)) :block-type (block-type a)))))

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

(defvar *paragraph-spechars* nil)
(setf *paragraph-spechars* (append *default-spechars* `(
	,(!+ 'spechar := #\\ :! (lambda (iter)
		(with-slots (text pos) iter
		(setf text (s+ (subseq text 0 (- pos 1)) (subseq text pos)))
		iter)))
	,(!+ 'spechar := #\$ :! (region-tag-fun #'white?
		(lambda (content)
			(let* ((colon (pos-not-escaped #\= content))
				(link (if colon (subseq content (+ colon 1)) content))
				(title (if colon (~format (subseq content 0 colon) *paragraph-spechars*) content)))
			(!+ 'tag := "a" :& `("href" ,link) :< title)))))
	,(!+ 'spechar := #\~ :! (replace-fun "&nbsp;"))
	,(!+ 'spechar := #\` :! (region-tag-fun #\`
		(lambda (content)
			(!+ 'tag := "span" :& '("class" "inline-code") :< (~format content *paragraph-spechars*)))))
	,(!+ 'spechar := #\% :! (region-tag-fun #\%
		(lambda (content)
			(!+ 'tag := "i" :< (~format content *paragraph-spechars*))))))))

(defvar *poem-spechars* (append *paragraph-spechars*
	`(,(!+ 'spechar := #\Newline :! (lambda (iterator)
		(with-slots (pos text) iterator
		(push-back (split-text iterator pos)(!+ 'tag := "br"))))))))

(defvar *title-spechars* (append *default-spechars* `(
	,(!+ 'spechar := #\\ :! (replace-fun ""))
	,(!+ 'spechar := #\% :! (replace-fun ""))
	,(!+ 'spechar := #\$ :! (replace-fun ""))
	,(!+ 'spechar := #\~ :! (replace-fun " "))
	,(!+ 'spechar := #\` :! (replace-fun "")))))

 (defun get-keys (doc)
	(let ((keys (make-hash-table :test #'equal))
		(iter (chunk-iterator doc)))
	(loop (let ((v (apply iter nil)))
		(if (not v) (return))
		(if (typep v 'keyval-line)
			(setf (gethash (first (value v)) keys) (second (value v))))))
	keys))

(defclass source-file ()
	((name :initarg := :reader name)
	(content :reader content)
	(keys :reader keys)))

(defmethod initialize-instance :after ((this source-file) &key)
	(with-slots (name content keys) this
	(setf content (chunks (mapcar #'>line (<f name))))
	(setf keys (get-keys content))))

(defgeneric title (doc))
(defmethod title ((this source-file))
	(value (content this)))

(defgeneric param (doc key))
(defmethod param ((this source-file) (key string))
	(gethash key (keys this)))

(defgeneric date> (a b))
(defmethod date> ((a source-file) (b source-file))
	(let* ((date-a (~format (param a "DATE") (split-spechar #\.)))
		(date-b (~format (param b "DATE") (split-spechar #\.)))
		(reltab (loop for ia in date-a
				for ib in date-b
				collect (- (read-from-string ia) (read-from-string ib)))))
	(or (> (third reltab) 0)
		(and (= (third reltab) 0)
			(or (> (second reltab) 0)
			(and (= (second reltab) 0) (> (first reltab) 0)))))))
	
(defun doc-link (doc)
	(!+ 'tag := "div" :& '("class" "docs-item") :< (list
		(!+ 'tag := "a" :& `("href" ,(linkf (name doc))) :< (~format (title doc) *paragraph-spechars*))
		(!+ 'tag := "span" :& '("class" "art-date")
			:< (format nil " (~A)" (param doc "DATE"))))))

(defun index-tag (files)
	(!+ 'tag := "div" :& '("id" "arts-list") 
		:< (mapcar #'doc-link (sort (mapcar (lambda (x) (!+ 'source-file := x)) files) #'date>))))

(defgeneric >tags (chunk))
(defmethod >tags ((c chunk))
	(if (tail c) (>tags (tail c))))

(defmethod >tags ((p paragraph))
	(cons (!+ 'tag := "p" :< (~format (value p) *paragraph-spechars*)) (call-next-method)))

(defmethod >tags ((h header))
	(cons (!+ 'tag := (format nil "h~a" (level h)) :& `("id" ,(page-bookmark h))
		:< (~format (value h) *paragraph-spechars*)) (call-next-method)))

(defmethod >tags ((c code-block))
	(cons (if (eql (block-type c) :code)
		(!+ 'tag := "pre" :< (!+ 'tag := "code" :< (~format (value c))))
		(!+ 'tag := "div" :& '("class" "poem-block") :< (~format (value c) *poem-spechars*))) (call-next-method)))

(defmethod >tags ((k keyval-line))
	(cons (if (string= (first (value k)) "AUDIO")
		(!+ 'tag := "audio" :&- "controls"
			:< (!+ 'tag := "source" :& `("src" ,(second (value k))
						"type" "audio/mpeg"))) 
	(if (string= (first (value k)) "INDEX")
		(index-tag (files (second (value k))))
	(if (string= (first (value k)) "IMAGE")
		(!+ 'tag := "img" :& `("src" ,(second (value k)))))))
		(call-next-method)))

;; document class
(defclass document ()
	(content
	(header :initarg :header :initform '())
	(title :initarg :title :initform "Peace!")
	(style :initarg :style :initform '())
	(footer :initarg :footer :initform '())
	(init :initarg :init :initform '())
	(refs :initform '() :reader refs)))

(defgeneric with-content (doc chunk))

(defclass toc ()
	((title :reader title) (tree :reader tree)))

(defmethod initialize-instance ((this toc) &key <)
	(with-slots (title tree) this
	(setf title nil)
	(labels ((read-chunk (chunk &optional (current nil))
		(cond ((not chunk) (list current))
			((and (typep chunk 'keyval-line) (string= (first (value chunk)) "TOC"))
				(setf title (second (value chunk)))
				(read-chunk (tail chunk) current))
			((and (typep chunk 'header) (= (level chunk) 2))
				(cons current (read-chunk (tail chunk) (list chunk))))
			((and (typep chunk 'header) (= (level chunk) 3))
				(read-chunk (tail chunk) (append current (list chunk))))
			(t (read-chunk (tail chunk) current)))))
		(setf tree (cdr (read-chunk <))))))

(defgeneric toc? (chunks))
(defmethod toc? ((ch chunk))
	(let ((toc (!+ 'toc :< ch)))
	(if (title toc) (!+ 'tag := "div" :& '("id" "toc")
		:< (list (!+ 'tag := "h3" :< (title toc)) (!+ 'tag := "ol"
			:< (mapcar (lambda (x) (!+ 'tag := "li" 
				:< (if (= (length x) 1) (link (car x))
					(list (link (car x)) (!+ 'tag := "ul" :< (mapcar (lambda (x)
						(!+ 'tag := "li" :< (link x)))(cdr x))))))) (tree toc))))))))
		
(defmethod with-content ((this document) (chunks header))
	(setf (slot-value this 'title) (value chunks))
	(with-content this (tail chunks)))

(defmethod with-content ((this document) (chunks chunk))
	(with-slots (content refs) this
	(setf refs (let ((idx (gethash "INDEX" (get-keys chunks))))
		(if idx (files idx))))
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
	(with-slots (content title style init footer header) d
	(html (!+ 'tag := "html" :< (list
		(!+ 'tag := "head" :< (list
			(!+ 'tag := "title" :< (~format title *title-spechars*))
			(!+ 'tag := "meta" :& '("charset" "utf-8"))
			(!+ 'tag := "meta" :& '("name" "viewport" "content" "width=device-width, initial-scale=1.0"))
			(!+ 'tag := "style" :& '("type" "text/css") :< (stylesheet (append *base-style* style)))
			(if init (!+ 'tag := "script" :& '("type" "text/javascript") :< (js (as-function "init" init))))
		))
		(!+ 'tag := "body" :& (if init '("onload" "init()") nil)
			:< (list (if header (!+ 'tag := "div" :& '("id" "header") :< header))
				 (!+ 'tag := "div" :& '("id" "art") 
					:< (cons (!+ 'tag := "h1" :< (~format title *paragraph-spechars*)) content))
				(!+ 'tag := "div" :& '("id" "footer") :< footer))))))))

