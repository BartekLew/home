(load "doc.lisp")

(let ((argv (cdr sb-ext:*posix-argv*)))
(if (eql argv nil) (progn (format nil "Missing parameter") (sb-ext:quit)))

(defun get-title (content)
	(if content
	(let ((next (car content)))
	(if (string= (tagtype next) "h1") (content next)
		(get-title (cdr content))))))

(doList (f argv)
(let ((*pwd* (filedir f)))
	(>f (outf f) (html (!+ 'document :from-file f
			:footer `(,(copyleft) "Lew, 2019"
				"<br/><a href=\"http://creativecommons.org/licenses/by-sa/4.0/\">"
				"<img alt=\"Creative Commons Licence\" style=\"border-width:0\""
				" src=\"https://i.creativecommons.org/l/by-sa/4.0/80x15.png\" /></a>")))))))

(sb-ext:quit)
