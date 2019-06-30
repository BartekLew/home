(load (merge-pathnames "util.lisp" *load-truename*))
(include "doc.lisp")

(let ((argv (cdr sb-ext:*posix-argv*)))
(if (eql argv nil) (progn (format nil "Missing parameter") (sb-ext:quit)))

(defun get-title (content)
	(if content
	(let ((next (car content)))
	(if (string= (tagtype next) "h1") (content next)
		(get-title (cdr content))))))

(defun head-for (file)
	(!+ 'tag := "a" :& `("href" ,(s+ "../" (if (string= *pwd* "./") file (subseq file (length *pwd*))) )) :< "więcej artykułów"))

(defun build-doc (f &optional (header nil))
	(let ((doc (!+ 'document :from-file f
			:header header
			:footer `(,(copyleft) "Lew, 2019"
				"<br/><a href=\"http://creativecommons.org/licenses/by-sa/4.0/\">"
				"<img alt=\"Creative Commons Licence\" style=\"border-width:0\""
				" src=\"https://i.creativecommons.org/l/by-sa/4.0/80x15.png\" /></a>"))))
	(format t "build ~A~%" f)
	(>f (outf f) (html doc))
	(if (refs doc) (doList (ref (refs doc)) (build-doc ref (head-for (outf f)))))))

(doList (f argv)
(let ((*pwd* (filedir f)))
	(build-doc f))))

(sb-ext:quit)
