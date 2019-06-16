(load "html.lisp")
(load "util.lisp")


(defclass chunk ()
	((value :initarg := :initform nil :accessor value)
	(tail :initform nil :accessor tail)))

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


(defclass string-iterator ()
	((text :initarg :< :accessor text)
	(pos :initform -1 :accessor pos)))

(defmethod value ((i string-iterator))
	(text i))

(defmethod print-object ((si string-iterator) out)
	(format out "<~A @~A '~A'>" (type-of si) (pos si) (s/- (format nil "~A" (value si)) 20)))

(defgeneric next (iter))
(defmethod next ((i string-iterator))
	(with-slots (text pos) i
	(setf pos (+ 1 pos))
	(if (>= pos (length text)) nil
		(char text pos))))

(defclass spechar ()
	((chr :initarg := :reader chr)
	(action :initarg :! :reader action)))

(defmethod print-object ((sc spechar) out)
	(format out "<~A:'~A'>" (type-of sc) (chr sc)))

(defmacro replace-fun (new-val)
	`(lambda (iterator)
		(with-slots (text pos) iterator
		(setf text (s+ (subseq text 0 pos)
				,new-val
				(subseq text (+ pos 1))))
		(setf pos (+ pos (length ,new-val)))
		iterator)))

(defvar *default-spechars* `(
	,(!+ 'spechar := #\< :! (replace-fun "&lt;"))
	,(!+ 'spechar := #\> :! (replace-fun "&gt;"))))

(defclass split-string-iterator (string-iterator)
	((split-part :initarg :<+ :initform '() :reader split-part)))

(defmethod value ((i split-string-iterator))
	(with-slots (text split-part) i
	(append split-part (list text))))

(defgeneric split-text (iterator pos))
(defmethod split-text ((i string-iterator) pos)
	(with-slots (text) i
	(!+ 'split-string-iterator :<+ (list (subseq text 0 pos))
			:< (subseq text (+ pos 1)))))

(defmethod split-text ((i split-string-iterator) pos)
	(with-slots (split-part text) i
	(let ((x (call-next-method)))
	(!+ 'split-string-iterator :<+ (append split-part (split-part x))
		:< (slot-value x 'text)))))

(defgeneric push-back (collection element))
(defmethod push-back ((i split-string-iterator) val)
	(with-slots (split-part) i
	(setf split-part (append split-part (list val)))
	i));

(defgeneric discard-text (text-container startpos endpos))
(defmethod discard-text ((i string-iterator) (startpos (eql 0)) endpos)
	(with-slots (text) i
	(setf text (subseq text endpos)) i))

(defmacro region-tag-fun (lim make-tag)
	`(lambda (iterator)
		(with-slots (text pos) iterator
		(let* ((endpos (position ,lim text :start (+ 1 pos))))
		(discard-text (push-back (split-text iterator pos)
					(,make-tag (subseq text (+ 1 pos) endpos)))
				0 (- endpos pos))))))

(defvar *paragraph-spechars* (append *default-spechars* `(
	,(!+ 'spechar := #\\ :! (lambda (iter)
		(with-slots (pos) iter
		(setf pos (+ pos 1)) iter)))
	,(!+ 'spechar := #\` :! (region-tag-fun #\`
		(lambda (content)
			(!+ 'tag := "span" :& '("class" "inline-code") :< content))))
	,(!+ 'spechar := #\% :! (region-tag-fun #\%
		(lambda (content)
			(!+ 'tag := "i" :< content)))))))

(defgeneric ~format (input &optional spechars))

(defmethod ~format ((input string) &optional (spechars *default-spechars*))
	(~format (!+ 'string-iterator :< input) spechars))

(defmethod ~format ((input string-iterator) &optional (spechars *default-spechars*))
	(let ((n (next input)))
	(if n (let ((spechar (find n spechars
				:test (lambda (chr spechar)
					(eql chr (chr spechar))))))
		(if spechar (~format (apply (action spechar) `(,input)) spechars)
			(~format input spechars)))
		(value input))))

(defgeneric >tags (chunk))
(defmethod >tags ((c chunk))
	(if (tail c) (>tags (tail c))))

(defmethod >tags ((p paragraph))
	(cons (!+ 'tag := "p" :< (~format (value p) *paragraph-spechars*)) (call-next-method)))

(defmethod >tags ((h header))
	(cons (!+ 'tag := "h2" :< (~format (value h) *paragraph-spechars*)) (call-next-method)))

(defmethod >tags ((c code-block))
	(cons (!+ 'tag := "pre" :< (!+ 'tag := "code" :< (~format (value c)))) (call-next-method)))


;; document class
(defclass document ()
	(content
	(title :initarg :title :initform "Peace!")
	(style :initarg :style :initform '())
	(footer :initarg :footer :initform '())
	(init :initarg :init :initform '())))

(defgeneric with-content (doc chunk))

(defmethod with-content ((this document) (chunks header))
	(setf (slot-value this 'title) (value chunks))
	(with-content this (tail chunks)))

(defmethod with-content ((this document) (chunks chunk))
	(with-slots (content) this
	(setf content (>tags chunks))))
	
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

