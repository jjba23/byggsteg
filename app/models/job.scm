(import (artanis mvc model))

(create-artanis-model job
                      (:deps)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (human_id char-field
                                (#:not-null #:unique #:maxlen 1024))
                      (job_data char-field
                                (#:not-null #:maxlen 8192))
                      (job_success tiny-integer
                                   (#:not-null))
                      (job_in_progress tiny-integer
                                       (#:not-null))
                      (job_failed tiny-integer
                                  (#:not-null))
                      (created_at date-field
                                  (#:not-null))
                      (updated_at date-field))
