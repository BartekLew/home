(load (merge-pathnames "util.lisp" *load-truename*))
(include "lang.lisp")
(include "doc.lisp")

(defvar *pwd* ".")
(defun use (file)
  (load (format nil "~A/~A" *pwd* file))
  (if (boundp '**home-modinit)
     (apply (eval '**home-modinit) nil)))

(let ((argv (cdr sb-ext:*posix-argv*)))
(if (eql argv nil) (progn (format nil "Missing parameter") (sb-ext:quit)))

(defun get-title (content)
	(if content
	(let ((next (car content)))
	(if (string= (tagtype next) "h1") (content next)
		(get-title (cdr content))))))

(defun head-for (file parent)
	(!+ 'tag := "a" :& `("href" ,(relative-path file parent)) :< "więcej artykułów"))

(defun build-doc (f &optional (header nil) closing)
	(format t "build ~A~%" f)
	(let ((doc (!+ 'document :from-file f
			:header header
			:closing closing
			:footer `(,(copyleft) ,(txt 'who-when)
		"<br/><a href=\"http://creativecommons.org/licenses/by-sa/4.0/\">"
		"<img alt=\"Creative Commons Licence\" style=\"border-width:0\""
		" src=\"https://i.creativecommons.org/l/by-sa/4.0/80x15.png\" /></a>"))))
	(>f (outf f) (html doc))
	(if (refs doc)
        (doList (ref (refs doc))
            (let ((*pwd* (filedir ref)))
                (build-doc ref (head-for (outf ref) (outf f)) (childclosing doc)))))))

(handler-case
  (doList (f argv)
    (let ((*pwd* (filedir f)))
  	  (build-doc f)))
  (error (e) (format T "Compilation error: ~a~%" e))))

(sb-ext:quit)
