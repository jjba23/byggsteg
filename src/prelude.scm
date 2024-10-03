


(define-module (byggsteg-prelude)
  #:use-module (byggsteg-i18n-en)
  #:use-module (byggsteg-i18n-nl)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  )

(define-public locale (car (string-split (getenv "LANG") #\_) ))


(define-public (ii sym)
  (cond
   ((equal? locale "nl")
    (assoc-ref dict-nl sym))
   (else (assoc-ref dict-en sym))
   )
  )

