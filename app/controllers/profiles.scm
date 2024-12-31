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
        (web uri)
        (byggsteg process)
        (byggsteg uuid)
        (byggsteg base64)
        (byggsteg preferences)
        (app views profiles)
        (app models profile)
        (ice-9 string-fun))

(get "/profiles"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (profile-list-page ($profile 'get
                                                          #:order-by
                                                          "human_id ASC"))))))

(get "/profiles/:id"
     #:mime 'xml
     (lambda (rc)
       (let ((p ($profile 'get
                          #:ret 'top
                          #:condition (where #:id (params rc "id")))))
         (response-emit (:mime rc
                               (profile-page (car p)))))))

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
               (profile-data (uri-decode (car (assoc-ref kv "job-code"))))
               (profile-name (car (assoc-ref kv "profile-name")))
               (new-uuid (string-replace-substring (uuidgen) "\n" ""))
               (found-profile ($profile 'get
                                        #:ret 'top
                                        #:condition (where #:human_id (safe-base64-encode
                                                                       profile-name)))))
          
          (cond
            ((not (nil? found-profile))
             (DB-query (DB-open rc)
                       (->sql update
                              'profile set
                              `((profile_data ,(safe-base64-encode
                                                profile-data))
                                (updated_at ,(current-time)))
                              (where #:human_id (safe-base64-encode
                                                 profile-name))))
             (redirect-to rc
                          (string->uri (format #f "~a/profiles/~a"
                                               (current-myhost)
                                               (assoc-ref found-profile "id")))))
            (else ($profile 'set
                            #:id new-uuid
                            #:human_id (safe-base64-encode profile-name)
                            #:profile_data (safe-base64-encode profile-data)
                            #:created_at (current-time))
                  (redirect-to rc
                               (string->uri (format #f "~a/profiles/~a"
                                                    (current-myhost) new-uuid))))))))

(post "/profiles/manage/delete"
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (profile-id (uri-decode (car (assoc-ref kv "profile-id")))))

          ($profile 'mod 'row-delete (where #:id profile-id))

          (redirect-to rc
                       (string->uri (format #f "~a/profiles"
                                            (current-myhost)))))))
