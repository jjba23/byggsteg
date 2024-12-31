;; (import (artanis mvc model))

;; (create-artanis-model log
;;                       (:deps)
;;                       (id char-field
;;                           (#:primary-key #:maxlen 36))
;;                       (human_id char-field
;;                                 (#:unique #:not-null))
;;                       (log_data char-field
;;                                 (#:not-null))
;;                       (created_at date-field
;;                                   (#:not-null))
;;                       (updated_at date-field)
;;                       )

