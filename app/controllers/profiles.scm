;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-artanis-controller profiles)

(import (byggsteg html)
        (byggsteg errors)
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
       (let* ((profile-repo (make-sqlite-profile-repository))
              (profiles (get-profiles profile-repo)))
         (response-emit (:mime rc
                               (profile-list-page (:i18n rc)
                                                  profiles
                                                  #:is-logged-in (:cookies-value
                                                                  rc
                                                                  "x-bewaking-byggsteg")
                                                  ;; The way to a new profile is a
                                                  ;; form nobody but a reader who
                                                  ;; may save one is offered.
                                                  #:permissions (session-permissions
                                                                 rc)))))))

(get "/profiles/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((profile-repo (make-sqlite-profile-repository))
              (p (get-profile-by-id profile-repo
                                    (params rc "id"))))
         ;; A saved profile that is not there is a 404 rather than a page about
         ;; nothing: the id is the whole of the request, so it either names a
         ;; profile or the path names none -- the answer the log and artifact
         ;; pages give for the same question.
         (if (not p)
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type text/html)))
             (response-emit (:mime rc
                                   (profile-page (:i18n rc)
                                                 p
                                                 #:is-logged-in (:cookies-value
                                                                 rc
                                                                 "x-bewaking-byggsteg")
                                                 ;; The page reads the saved job --
                                                 ;; which is public -- and draws
                                                 ;; the forms under it for whoever
                                                 ;; may send them.
                                                 #:permissions (session-permissions
                                                                rc))))))))

(get "/profiles/manage/create"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (define (show-the-form)
         (response-emit (:mime rc
                               (add-profile-form-page (:i18n rc)
                                                      #:is-logged-in (:cookies-value
                                                                      rc
                                                                      "x-bewaking-byggsteg")))))
       ;; The page is a form for saving a profile, and saving one takes
       ;; `byggsteg:profile-create', so a reader who does not hold it is sent to
       ;; sign in rather than shown a form they cannot send.
       (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                          #:needed-permissions '(byggsteg:profile-create)
                          #:rc rc
                          #:happy-flow show-the-form
                          #:unhappy-flow back-to-login)))

(post "/profiles/manage/submit"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (to-profile profile-id)
          "Answer the submission by sending the reader to the profile it saved."
          (redirect-to rc
                       (string->uri (format #f "~a/profiles/~a"
                                            (current-myhost)
                                            profile-id))
                       #:status 302))

        (define (saved-back repo profile-id name data)
          "Write NAME and DATA over the profile PROFILE-ID, and open its page.

The moment it was saved back is the only field of the row this writes beside the
two the form carries: the id it was saved under never changes, which is what
makes this an edit of one profile rather than a second profile under a new name."
          (update-profile-by-id repo
                                profile-id
                                `((title unquote
                                         name)
                                  (profile-data unquote
                                                data)
                                  (updated-at unquote
                                              (current-time))))
          (to-profile profile-id))

        (define (bad-request)
          "Answer a submission that is not a profile with the 400 page.

A profile is a name and a pipeline, and neither may be blank: a row saved with an
empty name is one a reader cannot tell from the next, and a row whose pipeline is
empty is one that runs nothing.  Both answer here rather than being written."
          (response-emit (error-page 400)
                         #:status 400
                         #:headers '((content-type text/html))))

        (define (do-submit)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 ;; The profile the form was drawn from, and empty or absent for
                 ;; the form that saves a new one.
                 (profile-id (assoc-ref body "profile-id"))
                 (profile-name (string-trim-both (or (assoc-ref body
                                                                "profile-name")
                                                     "")))
                 (profile-data (assoc-ref body "job-code"))
                 (profile-repo (make-sqlite-profile-repository)))
            (cond
              ((or (string-null? profile-name)
                   (not profile-data)
                   (string-null? (string-trim-both profile-data)))
               (bad-request))
              (else
               (let ((existing (get-profile-by-title profile-repo
                                                     profile-name)))
                 (cond
                  ;; The form of a profile being edited says which one, and the
                  ;; id is the whole of that reading: a form that carried only
                  ;; its name would save a rename as a second profile.
                  ((and profile-id
                        (not (string-null? profile-id)))
                   (saved-back profile-repo profile-id profile-name
                               profile-data))
                  ;; A name that is already saved is the profile that name, as
                  ;; the JSON API has it: what arrives replaces what it held.
                  (existing
                   (saved-back profile-repo (assq-ref existing
                                                      'id)
                               profile-name profile-data))
                  (else
                   (let ((new-id (generate-string-uuid)))
                     (insert-profile profile-repo
                                     `((id unquote new-id)
                                       (title unquote
                                              profile-name)
                                       (profile-data unquote
                                                     profile-data)
                                       (created-at unquote
                                                   (current-time))))
                     (to-profile new-id)))))))))
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
                 (profile-repo (make-sqlite-profile-repository)))
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
