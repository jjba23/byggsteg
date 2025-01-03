(create-artanis-migration user_20250103093008)

(use-modules (artanis artanis)
             (artanis security nss))

(define* (add-test-user! #:key name email password salt)
  (let ((mt (map-table-from-DB (get-conn-from-pool!)))
        (real-pass (string->sha-256 (string-append password salt))))
    (mt 'set
        'user
        #:username name
        #:email email
        #:password real-pass
        #:salt salt)))

(migrate-up (display "Adding user...")
            (add-test-user! #:name "Joe"
                            #:email "jjbigorra@gmail.com"
                            #:password ""
                            #:salt "asdf")
            (display "done.\n"))
