;; Controller jobs definition of byggsteg
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller jobs) ; DO NOT REMOVE THIS LINE!!!

(import (byggsteg html))

(get "/jobs/request"
  #:mime 'xml
  (lambda (rc)
    (response-emit (:mime rc (job-request-form-page)))))

(post "/jobs/submit" job-submit-endpoint)

(post "/jobs/delete" profile-delete-endpoint)
