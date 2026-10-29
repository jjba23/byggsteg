;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-artanis-controller activity)

(import (byggsteg html)
        (byggsteg preferences)
        (byggsteg heatmap)
        (byggsteg domain job)
        (byggsteg sqlite job)
        (app views activity))

(get "/activity"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((job-repo (make-sqlite-job-repository))
              ;; Only the day each job was queued on is read, one column for
              ;; the whole table, so the figure costs a query rather than the
              ;; jobs themselves.
              (timestamps (map (lambda (row)
                                 (assq-ref row
                                           'created-at))
                               (get-job-created-at job-repo))))
         (response-emit (:mime rc
                               (activity-page rc
                                              #:heatmap (heatmap-of timestamps)
                                              #:is-logged-in (:cookies-value
                                                              rc
                                                              "x-bewaking-byggsteg")))))))
