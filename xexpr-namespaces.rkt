#lang racket
(require racket/match racket/contract)

(provide xml-normalise-namespace
         (struct-out ns:tag))

(struct ns:tag (tag ns))

;;; The workhorse match expander that splits a 'namespace:tagname tag symbol into its two component
;;; symbols, identified by ns-id and tag-id
(define-match-expander x:y
  (λ (stx)
    (syntax-case stx ()
      [(_ ns-id tag-id)
       #'(or
          (app
          symbol->string 
          (pregexp
           "(.*):(.*)"
           (list
            _
            (app string->symbol ns-id)
            (app string->symbol tag-id))))
          (? symbol? (and tag-id (app (const 'default) ns-id))))])))

(define ((xml-normalise-namespace
          combine-ns-tag initial-namespaces
          #:elimiate-whitespace? (eliminate-whitespace? #t)
          #:add-empty-attributes? (add-empty-attributes? #t))
         X)
  (define (recur current-namespaces sub-X)
    (define (ns.elem ns-abbr tag)
      (combine-ns-tag (dict-ref current-namespaces ns-abbr) tag))
    (define ((ns.attr ns-abbr) tag) ; unlike ns.elem, this is pre-curried
      (if (eq? ns-abbr 'default) tag (combine-ns-tag (dict-ref current-namespaces ns-abbr) tag)))
    (match sub-X
      ;; concatenate new namespace abbreviation to current-namespaces
      [(list (? symbol? tag) (list-no-order (list (x:y _ ns-abbr) ns) attrs ...) content ...)
       (recur (dict-set current-namespaces ns-abbr ns) (list* tag attrs content))]
      
      ;; concatenate new DEFAULT namespace abbreviation to current-namespaces
      [(list (? symbol? tag)
             (list-no-order (list (app symbol->string (regexp #rx"^xmlns$")) ns)  attrs ...)
             content ...)
       (recur (dict-set current-namespaces 'default ns) (list* tag attrs content))]
      
      ;; xexpr tag with namespace abbreviation w/ attributes
      [(list (x:y ns tag)
             (and attrs (list (list (? symbol?) (? string?)) ...))
             content ...)
       (list* (ns.elem ns tag)
              attrs
              (filter-map (curry recur current-namespaces) content))]
      
      ;; xexpr tag w/o namespace abbreviation w/ attributes
      [(list (? symbol? tag)
             (and attrs (list (list (? symbol?) (? string?)) ...))
             content ...)
       (list* (ns.elem 'default tag)
              attrs
              (filter-map (xml-normalise-namespace current-namespaces) content))]
      
      ;; xexpr tag w/ namespace abbreviation without attributes
      [(cons (x:y ns tag) content)
       (cons (ns.elem ns tag)
             (append
              (if add-empty-attributes? '(()) '())
              (filter-map (curry recur current-namespaces) content)))]
      
      ;; xexpr tag w/o namespace abbreviation w/o attributes
      #;[(list (? symbol? tag) content ...)
       (cons (ns.elem 'default tag)
             (append
              (if add-empty-attributes? '(()) '())
              (filter-map (xml-normalise-namespace current-namespaces) content)))]
      
      [(pregexp #px"^[[:space:]]*$") (=> skip-match) (and (not eliminate-whitespace?) (skip-match))]
      
      (else (eprintf "~s~%" else) else)))
  
  (recur initial-namespaces X))