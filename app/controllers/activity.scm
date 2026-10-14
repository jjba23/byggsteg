;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

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
