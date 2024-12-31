(import (artanis mvc model))

(create-artanis-model profile
                      (:deps)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (human_id char-field
                                (#:not-null #:unique #:maxlen 1024))
                      (profile_data char-field
                                    (#:not-null #:maxlen 8192))
                      (created_at date-field
                                  (#:not-null))
                      (updated_at date-field)
                      )

