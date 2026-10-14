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

(define-artanis-controller artifacts)

(import (byggsteg artifacts)
        (byggsteg errors)
        (byggsteg domain artifact)
        (byggsteg sqlite artifact)
        (kracht prelude)
        (ice-9 format))

;; A tarball is answered as bytes rather than as a page, so the route declares
;; no mime: `(:mime rc ...)' is what turns sxml into a document, and there is no
;; document here.
;;
;; Downloading needs no credential, which is the same reading a job's log is
;; given: the log and the job data behind it are already public pages of this
;; deployment, and an artifact is what those commands built.  What is checked is
;; that the row points inside the artifact directory (see
;; `artifact-download-path'), so no row -- by accident or by a hand-written
;; insert -- can hand out a file that was never an artifact.
(get "/artifacts/:id"
     (lambda (rc)
       (let* ((artifact-repo (make-sqlite-artifact-repository))
              (artifact (get-artifact-by-id artifact-repo
                                            (params rc "id")))
              (path (and artifact
                         (artifact-download-path artifact))))
         (cond
           ((not artifact)
            (response-emit (error-page 404)
                           #:status 404
                           #:headers '((content-type text/html))))
           ((not path)
            ;; Recorded, but not servable: the file is gone, or the row points
            ;; outside the directory artifacts are written to.
            (response-emit (error-page 404)
                           #:status 404
                           #:headers '((content-type text/html))))
           (else
            ;; The body is a vector of bytes and the two headers are the two
            ;; shapes Guile's response writer validates: a content type as a list
            ;; holding its type, and a disposition as the disposition itself with
            ;; its parameters.  The header line a browser reads is written from
            ;; them; a string holding that line is refused as a bad value for a
            ;; header, which is why the filename is a field of the disposition
            ;; rather than something already formatted into one.
            (response-emit (read-artifact-file path)
                           #:headers `((content-type application/gzip)
                                       (content-disposition attachment
                                                            (filename unquote
                                                                      (assq-ref
                                                                       artifact
                                                                       'title))))))))))
