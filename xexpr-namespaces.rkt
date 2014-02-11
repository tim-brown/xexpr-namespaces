#lang racket
;;; xexpr-namespaces
;;; ----------------
;;; This module removes "xmlns" attributes from an xexpr document, and replaces the tags to which
;;; they refer with "normalised" versions, which include the namespace on every element.
;;;
;;; How this is done is up to the user, but (struct ns:tag) is provided to allow for,
;;; e.g. (struct ns:tag _ _) clauses in matches.
;;;
;;; The reverse process allows a consistent mapping of namespaced tags to xmlns: attributes. This
;;; allows matches to be performed on xexprs, where the tag has a guaranteed value of a symbol of the
;;; form nmspc:tag, where nmspc is (optionally) derived for a dictionary of some sort.
(require racket/match racket/contract)

;;; The workhorse match expander that splits a 'namespace:tagname tag symbol into its two component
;;; symbols, identified by ns-id and tag-id
(define-match-expander x:y
  (λ (stx)
    (syntax-case stx ()
      [(_ ns-id tag-id)
       #'(app
          symbol->string 
          (pregexp
           "(.*):(.*)"
           (list
            _
            (app string->symbol ns-id)
            (app string->symbol tag-id))))])))


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

;; 
(define (xml-normalise-namespace current-namespaces combine-ns-tag)
  (match-lambda
    ((and (list (? symbol? tag)
                (list-no-order (list (app symbol->string (and at (regexp #rx"^xmlns:(.*)$" (list _ (app string->symbol ns-abbr))))) ns)
                               attrs ...)
                content ...))
     ((xml-normalise-namespace (hash-set current-namespaces ns-abbr ns)) (list* tag attrs content)))
    
    ((and (list (? symbol? tag)
                (list-no-order (list (app symbol->string (and at (regexp #rx"^xmlns$"))) ns)
                               attrs ...)
                content ...))
     ((xml-normalise-namespace (hash-set current-namespaces 'default ns)) (list* tag attrs content)))
    
    ((and (list (? symbol?
                   (app symbol->string
                        (and ns:tag (regexp "(.*):(.*)"
                                            (list _ (app string->symbol ns)
                                                  (app string->symbol tag))))))
                (list attrs ...) content ...))
     (list* (combine-ns-tag tag (hash-ref current-namespaces ns))
            attrs (filter-map (xml-normalise-namespace current-namespaces) content)))
    
    ((and (list (? symbol? tag) (list attrs ...) content ...))
     (list* (combine-ns-tag tag (hash-ref current-namespaces 'default))
            attrs (filter-map (xml-normalise-namespace current-namespaces) content)))
    
    ((and spc (pregexp #px"^[[:space:]]*$")) #f)
    
    (else else)))

#|
(define (xexpr->fqns-xexpr x (default-ns #f)
                           (current-ns-map (hash)) (have-default-ns? #f))
  (printf "xexpr->fqns-xexpr ~a ~a~%" x current-ns-map)
  (match
    x

    ;; namespace introduction
    ((list-rest tag (list-no-order (list (x:y 'xmlns ns) uri) other-attrs ...)
                content)
     (let ((new-ns-map (hash-set current-ns-map ns uri)))
       (printf "new-ns-map -> ~a~%" new-ns-map)
       (xexpr->fqns-xexpr (list* tag other-attrs content)
                          default-ns new-ns-map have-default-ns?)))

    ;; default-namespace introduction
    ((list-rest tag (list-no-order (list 'xmlns uri) other-attrs ...) content)
     (=> skip)
     (when have-default-ns? (skip))
     (xexpr->fqns-xexpr (list* tag other-attrs content) uri current-ns-map #t))

    ;; element in default ns
    ((list-rest (x:y tag el) attributes content)
     (=> skip)
     (unless (eq? (hash-ref current-ns-map tag 'uri:ccl:no-namespace) default-ns)
       (skip))
     (list* el attributes
            (map (lambda (c)
                  (xexpr->fqns-xexpr c default-ns current-ns-map #f)) content)))

    ;; leave as is
    ((list-rest (x:y tag el) attributes content)
     (list* (cons tag el) attributes
            (map (lambda (c)
                  (xexpr->fqns-xexpr c default-ns current-ns-map #f)) content)))

    (x x)))
|#

(module+ test
  (require rackunit)
  (xexpr->fqns-xexpr
   `(html:html
     ((xmlns:html "http://www.w3.org/1999/xhtml"))
     (html:head () (html:title "Frobnostication"))
     (html:body ()
                (html:p "Moved to "
                        (html:a ((href "http://frob.example.com"))"here.")))))
  
  (xexpr->fqns-xexpr
   `(html:html
     ((xmlns:html "http://www.w3.org/1999/xhtml")
      (xmlns "http://www.w3.org/1999/xhtml"))
     (html:head () (html:title "Frobnostication"))
     (html:body ()
                (html:p "Moved to "
                        (html:a ((href "http://frob.example.com"))"here.")))))
  )