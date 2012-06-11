#lang racket
(require racket/match srfi/1)

(define-match-expander x:y
 (lambda (stx)
  (syntax-case stx ()
   [(_ ns el)
    #'(and
       (app symbol->string 
       (pregexp "(.*):(.*)" (list _
               (app string->symbol ns)
               (app string->symbol el)))))])))

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
