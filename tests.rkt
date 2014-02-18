#lang racket

(require rackunit xml/xml)
(require "xexpr-namespaces.rkt")

(define xn2/cons (xml-normalise-namespace cons null))
(define xn2/cons+ns1 (xml-normalise-namespace cons '((ns1 "http://timb.net"))))

(check-equal? (xn2/cons "woo") "woo" "xexpr: string")

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

;; TODO: test against all of the above
;; XEXPR Atoms
(check-equal? (xn2/cons "woo") "woo"
              "xexpr: string should be untouched")
(check-equal? (xn2/cons 'woo) 'woo
              "xexpr: symbol should be untouched")
(check-equal? (xn2/cons #x20) #x20
              "xexpr: valid-char? should be untouched")
(check-equal? (xn2/cons (make-cdata #f #f "<![CDATA[woo]]>"))
              (make-cdata #f #f "<![CDATA[woo]]>")
              "xexpr: cdata should be untouched")
(check-equal? (xn2/cons (make-comment "a comment")) (make-comment "a comment")
              "xexpr: comment should be untouched")

;; XEXPR Atoms in namespace
(check-equal? (xn2/cons+ns1 "woo") "woo"
              "xexpr: string in namespace should be untouched")
(check-equal? (xn2/cons+ns1 'woo) 'woo
              "xexpr: symbol in namespace should be untouched")
(check-equal? (xn2/cons+ns1 #x20) #x20
              "xexpr: valid-char? in namespace should be untouched")
(check-equal? (xn2/cons+ns1 (make-cdata #f #f "<![CDATA[woo]]>"))
              (make-cdata #f #f "<![CDATA[woo]]>")
              "xexpr: cdata in namespace should be untouched")
(check-equal? (xn2/cons+ns1 (make-comment "a comment")) (make-comment "a comment")
              "xexpr: comment in namespace should be untouched")

;; Shallow XML tags
;; 	 | 	(cons symbol)
(check-equal?
 (xn2/cons '(woo))
 '(woo ())
 "shallow xexpr w/o attribute w/o namespaces w/o context namespaces should get empty attr list")
;; 	 | 	(list symbol (list (list symbol string) ...))
(check-equal?
 (xn2/cons '(woo ((att1 "aval1"))))
 '(woo ((att1 "aval1")))
 "shallow xexpr w/ attribute w/o namespaces w/o context namespaces should be untouched")

;; Deep (recursive) XML tags
;; 	 | 	(list symbol (list (list symbol string) ...) xexpr ...)
;; 	 | 	(cons symbol (list xexpr ...))


;; These XML documents are examples from http://www.w3.org/TR/2006/REC-xml-names11-20060816/
(xn2/cons
 `(html:html
   ((xmlns:html "http://www.w3.org/1999/xhtml"))
   (html:head () (html:title "Frobnostication"))
   (html:body ()
              (html:p "Moved to "
                      (html:a ((href "http://frob.example.com"))"here.")))))

(xn2/cons
 `(html:html
   ((xmlns:html "http://www.w3.org/1999/xhtml")
    (xmlns "http://www.w3.org/1999/xhtml"))
   (html:head () (html:title "Frobnostication"))
   (html:body ()
              (html:p "Moved to "
                      (html:a ((href "http://frob.example.com"))"here.")))))


