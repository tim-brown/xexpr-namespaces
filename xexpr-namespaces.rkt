#lang racket
(require racket/match racket/contract xml/xml)

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
            (list _ (app string->symbol ns-id) (app string->symbol tag-id))))
          #;(? symbol? (and tag-id (app (const 'default) ns-id)))
          )])))

(define ((xml-normalise-namespace
          combine-ns-tag
          initial-abbrevs-dict
          #:elimiate-whitespace? (eliminate-whitespace? #t)
          #:add-empty-attributes? (add-empty-attributes? #t)
          #:replace-element
          (replace-element
           (λ (abbrevs-dict ns-abbr tag)
             (if (not (dict-has-key? abbrevs-dict ns-abbr))
                 tag
                 (combine-ns-tag (dict-ref abbrevs-dict ns-abbr) tag))))
          #:replace-attribute
          (replace-attribute
           (λ (abbrevs-dict ns-abbr tag)
             (combine-ns-tag (dict-ref abbrevs-dict ns-abbr) tag)
             (if (eq? ns-abbr 'default)
                 tag
                 (replace-element abbrevs-dict ns-abbr tag)))))
         X)
  
  (define (recur current-abbrevs-dict sub-X)
    (match sub-X
      ;; get the atomic forms out of the way...
      [(? symbol? s) s]
      [(? string? s) s]
      [(? valid-char? v) v]
      [(? comment? c) c]
      [(? p-i? p-i) p-i]
      [(? cdata? c-d) c-d]      
      
      ;; Attributes that change the namespace context
      
      ;; concatenate new namespace abbreviation to current-namespaces
      [(list (? symbol? tag)
             (list-no-order (list (x:y _ ns-abbr) ns) attrs ...) content ...)
       (recur (dict-set current-abbrevs-dict ns-abbr ns) (list* tag attrs content))]
      
      ;; concatenate new DEFAULT namespace abbreviation to current-namespaces
      [(list (? symbol? tag)
             (list-no-order (list (app symbol->string (regexp #rx"^xmlns$")) ns)  attrs ...)
             content ...)
       (recur (dict-set current-abbrevs-dict 'default ns) (list* tag attrs content))]
      
      ;; xexprs that need rewriting
      
      ;; xexpr tag with namespace abbreviation w/ attributes
      [(list (x:y ns tag)
             (and attrs (list (list (? symbol?) (? string?)) ...))
             content ...)
       (list* (replace-element current-abbrevs-dict ns tag)
              attrs
              (filter-map (curry recur current-abbrevs-dict) content))]
      
      ;; xexpr tag w/o namespace abbreviation w/ attributes
      [(list (? symbol? tag)
             (and attrs (list (list (? symbol?) (? string?)) ...))
             content ...)
       (list* (replace-element current-abbrevs-dict 'default tag)
              attrs
              (filter-map (curry recur current-abbrevs-dict) content))]
      
      ;; xexpr tag w/ namespace abbreviation without attributes
      [(cons (x:y ns tag) content)
       (cons (replace-element current-abbrevs-dict ns tag)
             (append
              (if add-empty-attributes? '(()) '())
              (filter-map (curry recur current-abbrevs-dict) content)))]
      
      ;; xexpr tag w/o namespace abbreviation w/o attributes
      [(list (? symbol? tag) content ...)
       (cons (replace-element current-abbrevs-dict 'default tag)
             (append
              (if add-empty-attributes? '(()) '())
              (filter-map (curry recur current-abbrevs-dict) content)))]
      
      ; [(pregexp #px"^[[:space:]]*$") (=> skip-match) (and (not eliminate-whitespace?) (skip-match))]      
      (else (eprintf "~s~%" else) else)))
  
  (recur initial-abbrevs-dict X))