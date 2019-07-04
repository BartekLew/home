(load (merge-pathnames "util.lisp" *load-truename*))

(defclass string-iterator ()
	((text :initarg :< :accessor text)
	(pos :initform 0 :accessor pos)))

(defmethod value ((i string-iterator))
	(text i))

(defmethod print-object ((si string-iterator) out)
	(with-slots (text pos) si
	(let ((startpos (if (> pos 20) pos 0))
		(endpos (if (> (+ pos 20) (length text)) (length text) (+ pos 20))))
	(format out "<~A @~A '~A'>" (type-of si) (pos si) (subseq text startpos endpos)))))

(defgeneric next (iter))
(defmethod next ((i string-iterator))
	(with-slots (text pos) i
	(if (>= (+ pos 1) (length text))
		nil

		(let ((result (char text pos)))
		(setf pos (+ 1 pos))
		result))))

(defclass spechar ()
	((chr :initarg := :reader chr)
	(action :initarg :! :reader action)))

(defgeneric =~ (target pattern))
(defmethod =~ (char (sc spechar))
	(with-slots (chr) sc
	(if (functionp chr) (apply chr (list char))
		(eql char chr))))

(defmethod print-object ((sc spechar) out)
	(format out "<~A:'~A'>" (type-of sc) (chr sc)))

(defmacro replace-fun (new-val)
	`(lambda (iterator)
		(with-slots (text pos) iterator
		(setf text (s+ (subseq text 0 (- pos 1))
				,new-val
				(subseq text pos)))
		(setf pos (- (+ pos (length ,new-val)) 1))
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
		(let* ((endpos ,(if (characterp lim)
				`(pos-not-escaped ,lim text pos)
				`(pos-not-escaped-if ,lim text pos))))
		(discard-text (push-back (split-text iterator (- pos 1))
					(,make-tag (subseq text pos endpos)))
				0 (- endpos (- pos 1)))))))

(defun split-spechar (delimiter)
	(list (!+ 'spechar := delimiter :! (lambda (iterator)
		(with-slots (text pos) iterator
		(split-text iterator pos))))))

(defgeneric ~format (input &optional spechars))

(defmethod ~format ((input string) &optional (spechars *default-spechars*))
	(~format (!+ 'string-iterator :< input) spechars))

(defmethod ~format ((input string-iterator) &optional (spechars *default-spechars*))
	(let ((n (next input)))
	(if n (let ((spechar (find n spechars :test #'=~)))
		(if spechar (~format (apply (action spechar) `(,input)) spechars)
			(~format input spechars)))
		(value input))))

(defclass regex ()
	((exp :initarg := :initform nil)
	(conditions :initarg :& :initform nil)))

(defmacro rgx (exp &optional cond)
	`(make-instance 'regex := ,exp :& ,cond))

(+constructor regex (=)
	(cond ((not =) (warn "regex constructor without exression, return nil") nil)
		(t (call-next-method))))

(+simple-printer regex 'exp)

(defgeneric ~+ (to &rest what))
(defmethod ~+ ((this regex) &rest symbols)
	(with-slots (exp) this
	(let ((symbol-i (list-iterator symbols)))
	(apply #'format (append (list 'nil (times-str "~A" (length  exp)))
			(loop for x in exp
				collect (if (typep x 'symbol) (apply symbol-i nil) x)))))))

(defmethod ~+ ((this list) &rest symbols)
	(apply '~+ (cons (rgx this nil) symbols)))

(defgeneric  ~= (regex value &key))
(defmethod ~= ((pattern string) (value string) &key (pos 0))
	(let ((endpos (+ pos (length pattern))))
	(if (<= endpos (length value))
		(if (string= (subseq value pos endpos) pattern) (list pos endpos)
			(~= pattern value :pos (+ pos 1))))))

(defmethod ~= (pattern (value string) &key (pos 0))
	(if (>= (length value) pos)
	(if (functionp pattern)
		(let ((endpos (apply* pattern (list value pos))))
		(if endpos (list pos endpos)
			(~= pattern value :pos (+ pos 1)))))))


(defmethod ~= ((this regex) (value string) &key (pos 0))
	(with-slots (exp conditions) this
	(let ((expi (list-iterator exp)) (keys nil) (lastkey nil) x (startpos pos))
	(if (eql (first exp) :nil) (apply expi nil)) 
	(tagbody
	load (setf x (apply expi nil))
	test (if (not x) (go quit))
		(if (typep x 'symbol)
			(let ((next (apply expi nil)))
			(cond ((not next)
					(let ((newkey (subseq value pos)))
					(or (not (hash[] x conditions))
						(apply* (hash[] x conditions) (list newkey 0))
						(go quit))
					(setf keys (append keys (list newkey)))
					(if (not (and (eql x :nil) (> (length newkey) 0)))
						(setf x nil)))
					(go quit))
				(t (setf lastkey x) (setf x next) (go test))))
		;; not symbol
			(let ((range (~= x value :pos pos)))
			(if (or (not range) (and (not lastkey) (\/= (first range) pos))) (go quit))
			(if lastkey
				(let ((newkey (subseq value pos (first range))))
				(or (not (hash[] lastkey conditions))
					(apply* (hash[] lastkey conditions) (list newkey 0))
					(go quit))
						 (setf keys (append keys (list newkey)))))
			(setf lastkey nil)
			(setf pos (second range))
			(go load)))
	quit)
	(if x
		(if (and (not (eql (first exp) :nil)) (< startpos (length value)))
			(~= this value :pos (+ startpos 1)))
		(or keys T)))))

(defun digit? (c)
	(and (char>= c #\0) (char<= c #\9)))

(let ((classes (make-hash-table)))
	(doList (cc `((#\d ,#'digit?) (#\s ,#'white?)))
		(setf (gethash (first cc) classes) (second cc)))
	(handler-case (defconstant +char-classes+ classes)( error(e) (format nil "~A" e))))

(labels ((one-or-more (str pos test)
	(if (and (> (length str) pos) (apply test (list (char str pos))))
		(or (one-or-more str (+ pos 1) test) (+ pos 1))))
	(any (str pos test)
		(or (one-or-more str pos test) pos))
	(maybe (str pos test)
		(if (and (> (length str) pos) (apply test (list (char str pos))))
			(+ pos 1)
			pos)))
			
	(let ((rss (make-hash-table)))
	(doList (rs `((#\+ ,#'one-or-more) (#\* ,#'any) (#\? ,#'maybe)))
		(setf (gethash (first rs) rss) (second rs)))
	(handler-case (defconstant +rep-specs+ rss) (error (e) (format nil "~A" e))) ))


(defun regex-atom-reader (stream char)
	(declare (ignore char))
	(let* ((c (read-char stream))
		(cc (hash[] c +char-classes+))
		(rs (hash[] (peek-char nil stream) +rep-specs+)))

	(unless cc (error "Unknown character class: ~S." c))

	(if rs (progn (read-char stream)
		(lambda (str pos)
			(apply rs (list str pos cc))))
		cc)))

(set-macro-character #\/ 'regex-atom-reader)

(apply /d+ '("1234 foo" 0))
(apply /d+ '("1234 foo" 5))
