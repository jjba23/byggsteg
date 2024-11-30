;; Controller logs definition of byggsteg
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller logs) ; DO NOT REMOVE THIS LINE!!!

(import (byggsteg html))

(get "/logs/:id"
  #:mime 'xml
  (lambda (rc)
    (response-emit (:mime rc (log-page (params rc "id"))))))
