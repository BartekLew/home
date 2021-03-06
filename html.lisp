(load (merge-pathnames "util.lisp" *load-truename*))
(include "js.lisp")
(include "string.lisp")

(defun style (&rest params)
  (list "style" (format nil "~{~A;~}" params)))

;; Create css-rule string
(defun css-rule (selector rules)
	(format nil "~A {~%	~A~%}~%" selector
		(join #'(lambda (a c) (format nil "~A;~%	~A" a c)) rules)))


;; Take css-rules (in list of lists form) and build css stylesheet
(defun stylesheet (rules)
	(join (lambda (a c) (format nil "~A~%~A" a c))
		(mapcar (lambda (x) (css-rule (first x) (second x))) rules)))

;; Create tag representning COPYLEFT symbol.
(defun copyleft ()
	(!+ 'tag := "span" :& '("style" "display:inline-block; transform:rotate(180deg)") :< "&copy;"))

(defun params>str (list)
	(if (eql list '()) ""
	(let* ((nkey (car list))
		(r1 (cdr list))
		(nval (car r1))
		(rest (cdr r1)))
	(format nil " ~A=\"~A\"~A" nkey nval (params>str rest)))))

(defclass tag ()
	((type :initarg := :initform "span" :reader tagtype)
	(par :initarg :& :initform '())
	(boolpar :initarg :&- :initform "")
	(content :initarg :< :initform nil :reader content)
    (idstyle :initarg :# :initform nil :reader idstyle)))

(defmethod idstyle ((x list))
  (if x (reduce (lambda (a v) (if v (format nil "~A~%~A" a v) a))
                (mapcar #'idstyle x))
    ""))

(let ((no 0))
  (defun genid (&optional (prefix "gen"))
    (incf no)
    (format nil "~A~A" prefix no)))

(defmethod initialize-instance :after ((this tag) &key)
	(with-slots (type content idstyle par) this
    (if idstyle 
      (let ((id (or (loop for i from 0 to (\/ (length par) 2)
                          do (if (string= (nth (* i 2) par) "id")
                               (return (nth (+ (* i 2) 1) par))))
                    (let ((id (genid type)))
                      (setf par (append par `("id" ,id)))
                      id))))
        (setf idstyle (css-rule (format nil "#~A" id) idstyle))))
    (loop for x in (if (listp+ content) content (list content))
          do (if (and (typep x 'tag) (idstyle x))
               (setf idstyle (or (and idstyle
                                      (format nil "~A~%~A" idstyle
                                                             (idstyle x)))
                                 (idstyle x)))))
	(if (and (string= type "div") (eql content nil)) (setf content ""))))

(defmethod print-object ((this tag) out)
	(with-slots (type par content idstyle) this
	(format out "<TAG \"~A\" ~A // ~A : ~A>" type (params>str par) idstyle content)))

(defgeneric tag= (a b))

(defmethod tag= ((a (eql nil)) (b (eql nil))) T)

(defmethod tag= ((a string) (b string))
	(equal a b))

(defmethod tag= ((a tag) (b tag))
	(and (string= (tagtype a) (tagtype b))
		(string= (slot-value a 'par) (slot-value b 'par))
		(string= (slot-value b 'boolpar) (slot-value b 'boolpar))
		(tag= (content a) (content b))))

(defmethod tag= ((a list) (b list))
	(not (find nil (loop for x in a
		for y in b
		collect (and x y)))))
		
(defgeneric html (src))
(defmethod html ((text string))
	text)

(defmethod html ((this tag))
(with-slots (type par boolpar content) this
  (format nil "~A"
	(let ((parstr (params>str par)))
	(if (eql content '())
		(format nil "<~A~A ~A/>" type parstr boolpar)
	(if (or (string= type "span") (string= type "i") (string= type "a"))
		(format nil "<~A~A ~A>~A</~A>" type parstr boolpar (html content) type)
	(~+ '("<" tagname " " parstr " " boolpar ">" content "</" parstr ">") type parstr boolpar (html content) type)))))))

(defmethod html ((l list))
	(if l (concatenate 'string (html (car l)) (html (cdr l)))
		""))

(defvar *base-style* '(
	("body" ("background-color: #d0f0d0" "color: #000080"))
	("#art" ("max-width: 75ex" "margin-left: auto" "margin-right: auto"
		"margin-top:5em" "margin-bottom: 4em" "font-size: 14pt"))
	("h1" ("text-align: center" "margin-bottom: 2em" "font-size: 2em"))
	("h2" ("margin-top: 2em" "font-size: 1.5em" "font-weight: 700"))
	("p" ("line-height: 1.4" "text-indent: 1em" "margin-top: 0px"))
	("#footer" ("font-size: 0.9em" "color: #008040" "text-align:center"
		"margin-bottom: 2em"))
	("a" ("color: inherit" "font-size: inherit"))
	("code" ("word-break: break-any"
		"font-family: monospace" "color: #f0e0f0" "font-size: 0.7em"
		"padding: 2em" "display: block"
		"background-color: #101010" "margin-top: 2em" "font-weight:100"
		"margin-bottom: 2em"))
	("pre" ("margin-bottom: 0px" "padding: 0px"))
	(".inline-code" ("font-family: monospace" "font-size:0.9em"
		"font-weight: 600"))
	("audio" ("width:50%" "margin-left: 25%" "margin-top: 1em" "margin-bottom: 2em" ))
	("#toc" ("margin-top: 2em" "margin-bottom: 3em"))
	("h3" ("margin-bottom: 0.5em" "margin-top: 1.5em"))
	("ol" ("margin-top: 0px"))
	("#arts-list" ("margin-top: 1.5em"))
	(".art-date" ("font-size: 0.8em" "font-style: italic"))
	(".docs-item" ("margin-left: 1em" "margin-right: 1em" "margin-bottom:0.5em" "text-align: center"))
	("img" ("max-width:80%" "margin: 1em 10%"))
	(".poem-block" ("text-align: center" "text-style: italic" "margin-bottom: 1em"))
	("#header" ("text-align: center"))
	("#closing" ("text-align: center" "margin-bottom: 2em"))
	("ul" ("list-style-type: none" "padding: 0.4em" "margin: 0em"))
))

(defun addStyle (key vals)
	(setf *base-style* (append *base-style* `((,key ,vals)))))

(defun canvas (&rest params)
  (let ((id (js-id "canvas")))
    (values
      (list
        (apply #'js (remove-if #'not (loop for par in params
              collect (if (find (first par)
                                '("onmousedown" "onmouseup" "onmousemove" "onclick" "onload")
                                :test #'string=)
                        `(fun ,(format nil "~A_~A" (first par) id) ("event")
                              ,@(exp-symbol (second par) 'this `(by-id ,id)))))))
        (!+ 'tag := "canvas"
            :& (apply #'append (cons `("id" ,id) (loop for par in params
                          collect (if (find (first par)
                                            '("onmousedown" "onmouseup" "onmousemove" "onclick" "onload")
                                            :test #'string=)
                                    (list (first par) (format nil "~A_~A(event)" (first par) id))
                                    par))))
            :< (txt 'no-canvas) )
        (!+ 'tag := "script" :< (format nil "onload_~A(0);" id)))
      id)))

(defun draw-box (&key on-change on-load)
    (canvas (style "background-color:#ffffff;margin-top:0.5em;margin-bottom:0.5em")
         `("onload" ((let x this)
            (= x.style.width "100%")
            (= x.width x.offset-width)
            (= x.height (* x.width 0.6))
            (= x.cpath ([]))
            ,@on-load))
         '("onmousedown" ((let canvas this)
            (on-pixels canvas
                ((event.offset-x event.offset-y) (0 0 0)))
            (canvas.cpath.push ([] event.offset-x event.offset-y))
            (= canvas.last-x event.offset-x)
            (= canvas.last-y event.offset-y)))
         '("onmousemove" ((let canvas this)
            (if (== event.buttons 1)
                ((canvas.cpath.push ([] event.offset-x event.offset-y))
                (draw-line canvas 5
                    (canvas.last-x canvas.last-y)
                    ( (:= canvas.last-x event.offset-x)
                      (:= canvas.last-y event.offset-y)))))))
        `("onmouseup" ((let x this)
                       ,@on-change
                       (= x.cpath ([]))))))

(defun button (caption id action)
  (list (js `(fun ,(format nil "~A_onclick" id) ()
                   ,@(exp-symbol action 'this `(by-id id))))
        (!+ 'tag := "button"
                 :< caption
                 :& `("id" ,id 
                      "onclick" ,(format nil "~A_onclick()" id)))))

(defun div (style &rest content)
  (!+ 'tag := "div" :& (list "style" style) :< (apply #'append content)))


(defun latex-equation (formula name)
  (let ((imgname (format nil "~A~A" *pwd* name)))
    (os-run `("/usr/bin/pdflatex" -jobname home-formula
              ,(format nil (s+ "\\documentclass[border=2pt]{standalone}"
                               "\\usepackage{amsmath}"
                               "\\begin{document}"
                               "\\Large"
                               "\\begin{math}"
                               "~A"
                               "\\end{math}"
                               "\\end{document}") formula)))
    (os-run `("/usr/bin/convert" -density 300 "home-formula.pdf" -quality 90 ,imgname))
    (!+ 'tag := "div" :& '("style" "text-align: center")
        :< (!+ 'tag := "img" :& `("src" ,name)))))

(defun graphviz-graph (formula name)
  (let ((imgname (format nil "~A~A" *pwd* name)))
    (os-run `("/usr/bin/dot" "-Tpng" "-o" ,name) :input formula)
    (!+ 'tag := "div" :& '("style" "text-align: center")
        :< (!+ 'tag := "img" :& `("src" ,name)))))
