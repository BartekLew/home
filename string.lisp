(load (merge-pathnames "util.lisp" *load-truename*))

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
		(let* ((endpos ,(if (characterp lim)
				`(pos-not-escaped ,lim text (+ 1 pos))
				`(pos-not-escaped-if ,lim text (+ 1 pos)))))
		(discard-text (push-back (split-text iterator pos)
					(,make-tag (subseq text (+ 1 pos) endpos)))
				0 (- endpos pos))))))

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
