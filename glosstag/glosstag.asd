;; (C) 2019 IBM Corporation
;;  Author: Alexandre Rademaker

(asdf:defsystem #:glosstag
  :serial t
  :depends-on (#:cxml #:cl-ppcre #:flexi-streams #:cl-fad :fare-csv :alexandria)
  :components ((:file "package")
	       (:file "glosstag" :depends-on ("package"))))

