;; (import (artanis mvc model))

;; (create-artanis-model job
;;                       (:deps)
;;                       (id char-field
;;                           (#:primary-key #:maxlen 36))
;;                       (human_id char-field
;;                                 (#:unique #:not-null))
;;                       (job_data char-field
;;                                 (#:not-null))
;;                       (created_at date-field
;;                                   (#:not-null))
;;                       (updated_at date-field)
;;                       )

