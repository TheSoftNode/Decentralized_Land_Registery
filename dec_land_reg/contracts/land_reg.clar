;; Decentralized Land Registry

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-price (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-not-for-sale (err u105))


;; Data Variables
(define-map properties
  { property-id: uint }
  {
    owner: principal,
    details: (string-ascii 256),
    price: uint,
    for-sale: bool,
    registration-date: uint
  }
)

(define-map property-transfers
  { property-id: uint }
  {
    from: principal,
    to: principal,
    status: (string-ascii 7),
    price: uint,
    transfer-date: uint
  }
)

(define-map property-history
  { property-id: uint, index: uint }
  {
    previous-owner: principal,
    new-owner: principal,
    transfer-date: uint,
    price: uint
  }
)

;; Private Functions
(define-private (get-block-height)
  block-height
)


;; Public Functions
(define-public (register-property (property-id uint) (details (string-ascii 256)))
  (let ((existing-property (map-get? properties { property-id: property-id })))
    (if (is-some existing-property)
      err-already-registered
      (ok (map-set properties 
        { property-id: property-id } 
        {
          owner: tx-sender,
          details: details,
          price: u0,
          for-sale: false,
          registration-date: (get-block-height)
        }
      ))
    )
  )
)


(define-public (transfer-property (property-id uint) (new-owner principal))
  (let ((existing-property (map-get? properties { property-id: property-id })))
    (if (is-none existing-property)
      err-not-found
      (let ((current-owner (get owner (unwrap-panic existing-property))))
        (if (is-eq tx-sender current-owner)
          (begin
            (map-set property-transfers { property-id: property-id } { from: tx-sender, to: new-owner, status: "pending", price: (get price (unwrap-panic (map-get? properties { property-id: property-id }))), transfer-date: (get-block-height) })
            (ok true)
          )
          err-owner-only
        )
      )
    )
  )
)

(define-public (accept-transfer (property-id uint))
  (let ((transfer (map-get? property-transfers { property-id: property-id })))
    (if (is-none transfer)
      err-not-found
      (let ((transfer-data (unwrap-panic transfer)))
        (if (and (is-eq (get to transfer-data) tx-sender) (is-eq (get status transfer-data) "pending"))
          (begin
            (map-set properties 
              { property-id: property-id } 
              { 
                owner: tx-sender, 
                details: (get details (unwrap-panic (map-get? properties { property-id: property-id }))), 
                price: (get price (unwrap-panic (map-get? properties { property-id: property-id }))), 
                for-sale: (get for-sale (unwrap-panic (map-get? properties { property-id: property-id }))), 
                registration-date: (get registration-date (unwrap-panic (map-get? properties { property-id: property-id }))) 
              }
            )
            (map-delete property-transfers { property-id: property-id })
            (ok true)
          )
          err-owner-only
        )
      )
    )
  )
)

(define-public (list-property-for-sale (property-id uint) (asking-price uint))
  (let ((existing-property (map-get? properties { property-id: property-id })))
    (if (is-none existing-property)
      err-not-found
      (let ((current-owner (get owner (unwrap-panic existing-property))))
        (if (and (is-eq tx-sender current-owner) (> asking-price u0))
          (ok (map-set properties 
            { property-id: property-id }
            (merge (unwrap-panic existing-property)
              {
                price: asking-price,
                for-sale: true
              }
            )
          ))
          err-owner-only
        )
      )
    )
  )
)


(define-public (remove-property-from-sale (property-id uint))
  (let ((existing-property (map-get? properties { property-id: property-id })))
    (if (is-none existing-property)
      err-not-found
      (let ((current-owner (get owner (unwrap-panic existing-property))))
        (if (is-eq tx-sender current-owner)
          (ok (map-set properties 
            { property-id: property-id }
            (merge (unwrap-panic existing-property)
              {
                for-sale: false
              }
            )
          ))
          err-owner-only
        )
      )
    )
  )
)

(define-public (buy-property (property-id uint))
  (let ((existing-property (map-get? properties { property-id: property-id })))
    (if (is-none existing-property)
      err-not-found
      (let (
        (property-data (unwrap-panic existing-property))
        (current-owner (get owner property-data))
        (sale-price (get price property-data))
        (is-for-sale (get for-sale property-data))
      )
        (if (and is-for-sale (not (is-eq tx-sender current-owner)))
          (begin
            (map-set property-transfers 
              { property-id: property-id }
              {
                from: current-owner,
                to: tx-sender,
                status: "pending",
                price: sale-price,
                transfer-date: (get-block-height)
              }
            )
            (ok true)
          )
          err-not-for-sale
        )
      )
    )
  )
)

(define-public (accept-transfer (property-id uint))
  (let ((transfer (map-get? property-transfers { property-id: property-id })))
    (if (is-none transfer)
      err-not-found
      (let (
        (transfer-data (unwrap-panic transfer))
        (existing-property (unwrap-panic (map-get? properties { property-id: property-id })))
        (history-index (default-to u0 (get-last-history-index property-id)))
      )
        (if (and 
          (is-eq (get to transfer-data) tx-sender)
          (is-eq (get status transfer-data) "pending")
        )
          (begin
            ;; Update property ownership
            (map-set properties
              { property-id: property-id }
              (merge existing-property
                {
                  owner: tx-sender,
                  for-sale: false,
                  price: u0
                }
              )
            )
            ;; Record in history
            (map-set property-history
              { property-id: property-id, index: (+ history-index u1) }
              {
                previous-owner: (get from transfer-data),
                new-owner: tx-sender,
                transfer-date: (get transfer-date transfer-data),
                price: (get price transfer-data)
              }
            )
            ;; Clean up transfer record
            (map-delete property-transfers { property-id: property-id })
            (ok true)
          )
          err-owner-only
        )
      )
    )
  )
)

;; Read-only Functions
(define-read-only (get-property-details (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-transfer-details (property-id uint))
  (map-get? property-transfers { property-id: property-id })
)

(define-read-only (get-property-history (property-id uint) (index uint))
  (map-get? property-history { property-id: property-id, index: index })
)

(define-read-only (get-last-history-index (property-id uint))
  (let ((history (map-get? property-history { property-id: property-id, index: u0 })))
    (if (is-none history)
      u0
      (fold check-next-index (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0 property-id)
    )
  )
)

(define-private (check-next-index (index uint) (last-index uint) (property-id uint))
  (let ((history (map-get? property-history { property-id: property-id, index: index })))
    (if (is-some history)
      index
      last-index
    )
  )
)

