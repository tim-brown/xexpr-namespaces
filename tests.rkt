#lang racket

(require rackunit)
(require "xexpr-namespaces.rkt")

;; These XML documents are examples from http://www.w3.org/TR/2006/REC-xml-names11-20060816/
((xml-normalise-namespace cons null)
 `(html:html
   ((xmlns:html "http://www.w3.org/1999/xhtml"))
   (html:head () (html:title "Frobnostication"))
   (html:body ()
              (html:p "Moved to "
                      (html:a ((href "http://frob.example.com"))"here.")))))

((xml-normalise-namespace cons null)
 `(html:html
   ((xmlns:html "http://www.w3.org/1999/xhtml")
    (xmlns "http://www.w3.org/1999/xhtml"))
   (html:head () (html:title "Frobnostication"))
   (html:body ()
              (html:p "Moved to "
                      (html:a ((href "http://frob.example.com"))"here.")))))


;; From the documentation for: xexpr?
;;The following grammar describes expressions that create X-expressions:
;;
;;  xexpr	 = 	string
;; 	 | 	(list symbol (list (list symbol string) ...) xexpr ...)
;; 	 | 	(cons symbol (list xexpr ...))
;; 	 | 	symbol
;; 	 | 	valid-char?
;; 	 | 	cdata
;; 	 | 	misc
;;
;; A string is literal data. When converted to an XML stream, the characters of the data will be
;; escaped as necessary.
;;
;; A pair represents an element, optionally with attributes. Each attribute’s name is represented by
;; a symbol, and its value is represented by a string.
;;
;; A symbol represents a symbolic entity. For example, 'nbsp represents &nbsp;.
;;
;; An valid-char? represents a numeric entity. For example, #x20 represents &#20;.
;;
;; A cdata is an instance of the cdata structure type, and a misc is an instance of the comment or
;; p-i structure types.

;; TODO: test against all of these