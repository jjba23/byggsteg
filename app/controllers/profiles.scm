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

(define-artanis-controller profiles)

(import (byggsteg html)
        (byggsteg url)
        (byggsteg preferences)
        (app views profiles))

(get "/profiles"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (profile-list-page)))))

(get "/profiles/:path"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (profile-page (params rc "path"))))))

(get "/profiles/manage/create"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (add-profile-form-page)))))

(post "/profiles/manage/submit"
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (pk 'body
                       (read-url-encoded-body body)))
               (profile-code (uri-decode (car (assoc-ref kv "job-code"))))
               (kvv (eval-string profile-code))
               (project (assoc-ref kvv
                                   'project))
               (clone-url (assoc-ref kvv
                                     'clone-url))
               (branch-name (assoc-ref kvv
                                       'branch-name))
               (task (assoc-ref kvv
                                'task))
               (profile-name (car (assoc-ref kv "profile-name")))
               (profile-link (format #f "/profiles/~a" profile-name)))
          (with-output-to-file (string-append profile-location profile-name)
            (lambda ()
              (display profile-code)))
          (redirect-to rc profile-link))))

(post "/profiles/manage/delete"
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (profile-name (uri-decode (car (assoc-ref kv "profile-name")))))
          
          (syscall (format #f "rm -rfv ~a"
                           (string-append profile-location profile-name)))
          (redirect-to rc "/profiles"))))
