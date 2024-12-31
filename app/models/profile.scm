(import (artanis mvc model))

(create-artanis-model profile
                      (:deps)
                      (id char-field
                          (#:primary-key #:not-null #:maxlen 36))
                      (human_id char-field
                                (#:unique #:not-null))
                      (profile_data char-field
                                    (#:not-null))
                      (created_at date-field
                                  (#:not-null))
                      (updated_at date-field)
                      )

