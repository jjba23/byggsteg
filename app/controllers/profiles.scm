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

(define-artanis-controller profiles)

(import (byggsteg html)
        (byggsteg url)
        (byggsteg env)
        (web uri)
        (byggsteg process)
        (uuid generate)
        (byggsteg auth)
        (byggsteg preferences)
        (byggsteg query-escape)
        (app views profiles)
        (app models profile)
        (artanis i18n)
        (artanis i18n locale)
        (ice-9 string-fun))

(get "/profiles"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (profile-list-page rc
                                                ($profile 'get
                                                          #:order-by '(human_id
                                                                       asc))
                                                #:is-logged-in (:cookies-value
                                                                rc
                                                                "x-bewaking-byggsteg"))))))

(get "/profiles/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let ((p ($profile 'get
                          #:ret 'top
                          #:condition (where #:id (params rc "id")))))
         (response-emit (:mime rc
                               (profile-page rc
                                             (car p)
                                             #:is-logged-in (:cookies-value rc
                                                             "x-bewaking-byggsteg")))))))

(get "/profiles/manage/create"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (add-profile-form-page rc
                                                    #:is-logged-in (:cookies-value
                                                                    rc
                                                                    "x-bewaking-byggsteg"))))))

(post "/profiles/manage/submit"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-submit)
          (let* ((body (rc-body rc))
                 (kv (pk 'body
                         (read-url-encoded-body body)))
                 (profile-data (uri-decode (car (assoc-ref kv "job-code"))))
                 (profile-name (car (assoc-ref kv "profile-name")))
                 (new-uuid (generate-string-uuid))
                 (found-profile ($profile 'get
                                          #:ret 'top
                                          #:condition (where #:human_id (escape-str
                                                                         profile-name)))))
            
            (cond
              ((not (nil? found-profile))
               (DB-query (DB-open rc)
                         (->sql update
                                'profile set
                                `((profile_data ,(escape-str profile-data))
                                  (updated_at ,(current-time)))
                                (where #:human_id (escape-str profile-name))))
               (redirect-to rc
                            (string->uri (format #f "~a/profiles/~a"
                                                 (current-myhost)
                                                 (assoc-ref found-profile "id")))
                            #:status 302))
              (else ($profile 'set
                              #:id new-uuid
                              #:human_id (escape-str profile-name)
                              #:profile_data (escape-str profile-data)
                              #:created_at (current-time))
                    (redirect-to rc
                                 (string->uri (format #f "~a/profiles/~a"
                                                      (current-myhost)
                                                      new-uuid))
                                 #:status 302)))))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:profile-create)
                           #:rc rc
                           #:happy-flow do-submit
                           #:unhappy-flow back-to-login)))

(post "/profiles/manage/delete"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (profile-id (uri-decode (car (assoc-ref kv "profile-id")))))
            
            ($profile 'mod
                      'row-delete
                      (where #:id profile-id))

            (redirect-to rc
                         (string->uri (format #f "~a/profiles"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:profile-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))
