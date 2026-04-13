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
        (byggsteg env)
        (web uri)
        (uuid generate)
        (byggsteg auth)
        (byggsteg preferences)
        (kracht prelude)
        (app views profiles)
        (byggsteg domain profile)
        (byggsteg sqlite profile)
        (artanis i18n)
        (artanis i18n locale)
        (ice-9 string-fun))

(get "/profiles"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((profile-repo (make-sqlite-profile-repository rc))
              (profiles (get-profiles profile-repo)))
         (response-emit (:mime rc
                               (profile-list-page rc profiles
                                                  #:is-logged-in (:cookies-value
                                                                  rc
                                                                  "x-bewaking-byggsteg")))))))

(get "/profiles/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((profile-repo (make-sqlite-profile-repository rc))
             (p (get-profile-by-id profile-repo
                                   (params rc "id"))))
         (response-emit (:mime rc
                               (profile-page rc p
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
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (profile-data (assoc-ref body "job-code"))
                 (profile-name (assoc-ref body "profile-name"))
                 (new-uuid (generate-string-uuid))
                 (profile-repo (make-sqlite-profile-repository rc))
                 (found-profile (get-profile-by-title profile-repo (html-escape
                                                       profile-name))))
            
            (cond
              ((not (nil? found-profile))
               (DB-query (DB-open rc)
                         (->sql update
                                'profile set
                                `((profile_data ,(html-escape profile-data))
                                  (updated_at ,(current-time)))
                                (where #:title (html-escape profile-name))))
               (redirect-to rc
                            (string->uri (format #f "~a/profiles/~a"
                                                 (current-myhost)
                                                 (assoc-ref found-profile "id")))
                            #:status 302))
              (else (insert-profile profile-repo
                                    `((id unquote new-uuid)
                                      (title unquote
                                             (html-escape profile-name))
                                      (profile-data unquote
                                                    (html-escape profile-data))
                                      (created-at unquote
                                                  (current-time))))
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
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (profile-id (assoc-ref body "profile-id"))
                 (profile-repo (make-sqlite-profile-repository rc)))
            (delete-profile-by-id profile-repo profile-id)

            (redirect-to rc
                         (string->uri (format #f "~a/profiles"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:profile-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))
