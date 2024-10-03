
(setlocale LC_ALL "")

(define-module (byggsteg-prelude)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  )

(define-public (G_ msg) (gettext msg "byggsteg"))
