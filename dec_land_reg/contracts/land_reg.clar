;; Decentralized Land Registry

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-registered (err u102))

;; Data Variables
(define-map properties
  { property-id: uint }
  { owner: principal, details: (string-ascii 256) }
)

(define-map property-transfers
  { property-id: uint }
  { from: principal, to: principal, status: (string-ascii 20) }
)

