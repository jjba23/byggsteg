;; Controller profiles definition of byggsteg
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller profiles) ; DO NOT REMOVE THIS LINE!!!

(import (byggsteg html))

(get "/profiles"
  #:mime 'xml
  (lambda (rc)
    (response-emit (:mime rc (profile-list-page)))))

(get "/profiles/:path"
  #:mime 'xml
  (lambda (rc)
    (response-emit (:mime rc (profile-page (params rc "path"))))))

(get "/profiles/new"
  #:mime 'xml
  (lambda (rc)
    (response-emit (:mime rc (add-profile-form-page)))))

(post "/profiles/submit"
  (lambda (rc)
    (profile-submit-endpoint request body)))

(post "/profiles/delete"
  (lambda (rc)
    (profile-delete-endpoint request body)))
