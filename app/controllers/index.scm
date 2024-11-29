;; Controller index definition of byggsteg
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller index) ; DO NOT REMOVE THIS LINE!!!

(import (byggsteg html))

(get "/"
  #:mime 'xml
  (lambda (rc)
    (response-emit (:mime rc (welcome-page)))))
