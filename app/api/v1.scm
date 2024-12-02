;; RESTful API v1 definition of byggsteg
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-restful-api v1) ; DO NOT REMOVE THIS LINE!!!

(import (byggsteg rest))

(post "/api/v1/jobs/manage/submit"
  #:mime 'json
  (lambda (rc)
    (:mime rc (job-submit-api rc))))

(post "/api/v1/logs/:path"
  #:mime 'json
  (lambda (rc)
    (:mime rc (log-api-page (params rc "path")))))
