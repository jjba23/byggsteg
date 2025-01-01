(import (artanis mvc model))

(create-artanis-model log
                      (:deps job)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (human_id char-field
                                (#:not-null #:unique #:maxlen 1024))
                      (job_id char-field
                              (#:not-null #:maxlen 36))
                      (log_data char-field
                                (#:not-null #:maxlen 16384))
                      (created_at date-field
                                  (#:not-null))
                      (updated_at date-field))

