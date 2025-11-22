;; Shimmer Relay - Zero-Knowledge Professional Credential Verification System
;; A decentralized platform for verifying professional credentials without exposing sensitive data

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-reputation (err u105))

;; Minimum stake required for validators (in microSTX)
(define-constant min-validator-stake u1000000)

;; Data Variables
(define-data-var platform-fee-percentage uint u2) ;; 2% platform fee

;; Data Maps

;; Validator registry: tracks institutional validators and their stakes
(define-map validators
  principal
  {
    stake: uint,
    reputation-score: uint,
    total-verifications: uint,
    successful-verifications: uint,
    is-active: bool,
    registered-at: uint
  }
)

;; Professional profiles: stores shimmer reputation and metadata
(define-map professional-profiles
  principal
  {
    reputation-score: uint,
    total-endorsements: uint,
    verification-count: uint,
    credential-hash: (buff 32), ;; Zero-knowledge proof of credentials
    last-updated: uint,
    is-verified: bool
  }
)

;; Verification requests: tracks credential verification events
(define-map verification-requests
  uint ;; request-id
  {
    professional: principal,
    validator: principal,
    credential-type: (string-ascii 64),
    verification-level: uint, ;; 1-5 granularity levels
    status: (string-ascii 20), ;; pending, approved, rejected
    timestamp: uint,
    fee: uint
  }
)

;; Endorsements: peer-to-peer professional endorsements
(define-map endorsements
  { endorser: principal, endorsed: principal }
  {
    skill-category: (string-ascii 64),
    weight: uint,
    timestamp: uint
  }
)

;; Counter for verification request IDs
(define-data-var next-verification-id uint u1)

;; Read-only functions

(define-read-only (get-validator-info (validator principal))
  (map-get? validators validator)
)

(define-read-only (get-professional-profile (professional principal))
  (map-get? professional-profiles professional)
)

(define-read-only (get-verification-request (request-id uint))
  (map-get? verification-requests request-id)
)

(define-read-only (get-endorsement (endorser principal) (endorsed principal))
  (map-get? endorsements { endorser: endorser, endorsed: endorsed })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-read-only (calculate-reputation-bonus (verifications uint) (endorsement-count uint))
  (+ (* verifications u10) (* endorsement-count u5))
)

;; Public functions

;; Register as a validator with minimum stake
(define-public (register-validator (stake-amount uint))
  (let
    (
      (caller tx-sender)
      (existing-validator (map-get? validators caller))
    )
    (asserts! (is-none existing-validator) err-already-exists)
    (asserts! (>= stake-amount min-validator-stake) err-insufficient-stake)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? stake-amount caller (as-contract tx-sender)))
    
    ;; Register validator
    (ok (map-set validators caller {
      stake: stake-amount,
      reputation-score: u100,
      total-verifications: u0,
      successful-verifications: u0,
      is-active: true,
      registered-at: block-height
    }))
  )
)

;; Create a professional profile with credential hash
(define-public (create-professional-profile (credential-hash (buff 32)))
  (let
    (
      (caller tx-sender)
      (existing-profile (map-get? professional-profiles caller))
    )
    (asserts! (is-none existing-profile) err-already-exists)
    
    (ok (map-set professional-profiles caller {
      reputation-score: u50, ;; Starting reputation
      total-endorsements: u0,
      verification-count: u0,
      credential-hash: credential-hash,
      last-updated: block-height,
      is-verified: false
    }))
  )
)

;; Request credential verification
(define-public (request-verification 
    (validator principal) 
    (credential-type (string-ascii 64))
    (verification-level uint)
    (fee uint))
  (let
    (
      (caller tx-sender)
      (request-id (var-get next-verification-id))
      (validator-info (unwrap! (map-get? validators validator) err-not-found))
      (profile (unwrap! (map-get? professional-profiles caller) err-not-found))
    )
    (asserts! (get is-active validator-info) err-unauthorized)
    
    ;; Transfer verification fee
    (try! (stx-transfer? fee caller validator))
    
    ;; Create verification request
    (map-set verification-requests request-id {
      professional: caller,
      validator: validator,
      credential-type: credential-type,
      verification-level: verification-level,
      status: "pending",
      timestamp: block-height,
      fee: fee
    })
    
    ;; Increment request counter
    (var-set next-verification-id (+ request-id u1))
    
    (ok request-id)
  )
)

;; Validator approves verification
(define-public (approve-verification (request-id uint))
  (let
    (
      (caller tx-sender)
      (request (unwrap! (map-get? verification-requests request-id) err-not-found))
      (professional (get professional request))
      (validator-info (unwrap! (map-get? validators caller) err-not-found))
      (profile (unwrap! (map-get? professional-profiles professional) err-not-found))
    )
    (asserts! (is-eq caller (get validator request)) err-unauthorized)
    (asserts! (is-eq (get status request) "pending") err-unauthorized)
    
    ;; Update verification request status
    (map-set verification-requests request-id
      (merge request { status: "approved" })
    )
    
    ;; Update professional profile
    (map-set professional-profiles professional
      (merge profile {
        verification-count: (+ (get verification-count profile) u1),
        reputation-score: (+ (get reputation-score profile) u20),
        is-verified: true,
        last-updated: block-height
      })
    )
    
    ;; Update validator stats
    (map-set validators caller
      (merge validator-info {
        total-verifications: (+ (get total-verifications validator-info) u1),
        successful-verifications: (+ (get successful-verifications validator-info) u1),
        reputation-score: (+ (get reputation-score validator-info) u5)
      })
    )
    
    (ok true)
  )
)

;; Endorse another professional
(define-public (endorse-professional 
    (endorsed principal)
    (skill-category (string-ascii 64))
    (weight uint))
  (let
    (
      (caller tx-sender)
      (endorser-profile (unwrap! (map-get? professional-profiles caller) err-not-found))
      (endorsed-profile (unwrap! (map-get? professional-profiles endorsed) err-not-found))
    )
    (asserts! (not (is-eq caller endorsed)) err-unauthorized)
    (asserts! (<= weight u100) err-invalid-reputation)
    
    ;; Record endorsement
    (map-set endorsements
      { endorser: caller, endorsed: endorsed }
      {
        skill-category: skill-category,
        weight: weight,
        timestamp: block-height
      }
    )
    
    ;; Update endorsed professional's profile
    (map-set professional-profiles endorsed
      (merge endorsed-profile {
        total-endorsements: (+ (get total-endorsements endorsed-profile) u1),
        reputation-score: (+ (get reputation-score endorsed-profile) weight),
        last-updated: block-height
      })
    )
    
    (ok true)
  )
)

;; Update credential hash (for credential evolution)
(define-public (update-credential-hash (new-credential-hash (buff 32)))
  (let
    (
      (caller tx-sender)
      (profile (unwrap! (map-get? professional-profiles caller) err-not-found))
    )
    (ok (map-set professional-profiles caller
      (merge profile {
        credential-hash: new-credential-hash,
        last-updated: block-height
      })
    ))
  )
)

;; Validator increases stake
(define-public (increase-validator-stake (additional-stake uint))
  (let
    (
      (caller tx-sender)
      (validator-info (unwrap! (map-get? validators caller) err-not-found))
    )
    (try! (stx-transfer? additional-stake caller (as-contract tx-sender)))
    
    (ok (map-set validators caller
      (merge validator-info {
        stake: (+ (get stake validator-info) additional-stake)
      })
    ))
  )
)

;; Admin functions

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u10) err-invalid-reputation) ;; Max 10% fee
    (ok (var-set platform-fee-percentage new-fee))
  )
)

;; Initialize contract
(begin
  (print "Shimmer Relay contract initialized")
)