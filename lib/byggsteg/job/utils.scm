;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (byggsteg job utils)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg base64)
  #:use-module (byggsteg process))

(define-public (read-job-success log-filename)
  (cond
    ((file-exists? (string-append job-success-location log-filename))
     #t)
    (else #f)))

(define-public (read-job-failure log-filename)
  (cond
    ((file-exists? (string-append job-failure-location log-filename))
     #t)
    (else #f)))

(define-public (get-file-list dir)
  (string-split (syscall-silent (format #f "ls -1 --sort=time ~a" dir))
                #\newline))

(define-public (create-empty-file filename)
  "Create an empty file."
  (when (file-exists? filename)
    (delete-file filename))
  (with-output-to-file filename
    (lambda ()
      (display ""))))

(define-public (create-file-with filename data)
  "Create a file with."
  (when (file-exists? filename)
    (delete-file filename))
  (with-output-to-file filename
    (lambda ()
      (display (safe-base64-encode data)))))

