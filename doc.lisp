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
(defclass embedded-chunk (chunk) ())

(defgeneric chunk-limiter (chunk))
(defmethod chunk-limiter ((chunk chunk))
	#'blank-line?)

(defmethod chunk-limiter ((chunk code-chunk))
	(lambda (line) (string= line "```")))

(defmethod initialize-instance :after ((this chunk) &key loader)
	(if loader
	(with-slots (content) this
	(let ((rest (loop for x = (apply loader '())
			until (apply (orf #'not (chunk-limiter this)) `(,x))
			collect x)))
	(setf content (if content (cons content rest)
				rest))))))

(defmethod initialize-instance :after ((this embedded-chunk) &key)
	(with-slots (content) this
	(setf content
	(if (and (> (length content) 8) (string= (subseq content 2 9) "AUDIO: "))
		(!+ 'tag := "audio" :&- "controls" :<
			(!+ 'tag := "source" :& `("src" ,(subseq content 9) "type" "audio/mpeg")))))))
		
(defun embedded? (line)
	(and (> (length line) 2) (string= (subseq line 0 2) "!!")))

(defun modtag (mod content)
	(cond	((eql mod #\`) (!+ 'tag := "span" :& '("class" "inline-code") :< content))
		((eql mod #\%) (!+ 'tag := "i" :< content))))

(defun apply-esc (string pos)
	(cons (subseq string 0 pos) (~format (subseq string (+ pos 2)))))

(defun apply-<> (string pos)
	(let* ((mod (char string pos))
		(subst (if (eql mod #\<) "&lt;" "&gt;")))
	`(,(subseq string 0 pos) ,subst (subseq string (+1 pos)))))

(defun apply-mod (string pos)
	(let* ((mod (char string pos))
		(endpos (pos-not-escaped mod string (+ pos 1))))
	`(,(subseq string 0 pos)
		,(modtag mod (subseq string (+ pos 1) endpos))
		,(~format (subseq string (+ endpos 1))))))

(defun ~format (string)
	(if (string= string "") '()
	(let ((modpos (pos-not-escaped+ '(#\\ #\` #\% #\< #\>) string)))
	(if (not modpos) string

	(let ((mod (char string modpos)))
	(cond ((eql mod #\\) (apply-esc string modpos))
		((find mod '(#\> #\<)) (apply-<> string modpos))
		(t (apply-mod string modpos))))))))
	
(defun merge-lines (lines)
	(~format (join (sep #\Newline) lines)))

(defvar *has-h1?* nil) 		;; should be shadowed on >tag call
(defun top-header ()
	(if *has-h1?* "h2"
		(progn (setf *has-h1?* t) "h1")))

(defgeneric >tag (src))
(defmethod >tag ((c chunk))
	(with-slots (content) c
	(if (first content)
	(if (headr? (car (last content)))
		(!+ 'tag := (top-header) :< (merge-lines (subseq content 0 (- (length content) 1))))
		(!+ 'tag := "p" :< (merge-lines content))))))

(defmethod >tag ((c embedded-chunk))
	(slot-value c 'content))

(defun esc-<> (string)
	(let ((pos (pos-not-escaped+ '(#\> #\<) string)))
	(if (not pos) string
	(s+ (subseq string 0 pos)
		(if (eql (char string pos) #\>) "&gt;" "&lt;")
		(esc-<> (subseq string (+ 1 pos)))))))

(defmethod >tag ((chunk code-chunk))
	(with-slots (content) chunk
	(!+ 'tag := "pre" :< (!+ 'tag := "code" :< (esc-<> (join (sep #\Newline) content))))))

(defun chunk (i)
	(flet ((rdl () (rd i)))
	(let*	((l (1st (orf #'not #'nonblank-line?) #'rdl)))
	(if (string= l "```") (!+ 'code-chunk :loader #'rdl)
	(if (embedded? l) (!+ 'embedded-chunk := l)
		(!+ 'chunk := l :loader #'rdl))))))

(defun chunks (lines)
	(let ((*has-h1?* nil)) 		;; shadow top-header marker
	(loop for x = (>tag (chunk lines))
		until (eql x nil)
		collect x)))

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

