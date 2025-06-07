(define-non-fungible-token disaster-report uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-input (err u104))

(define-data-var next-report-id uint u1)
(define-data-var contract-paused bool false)

(define-map disaster-reports
  uint
  {
    reporter: principal,
    disaster-type: (string-ascii 50),
    location: (string-ascii 100),
    severity: uint,
    timestamp: uint,
    description: (string-ascii 500),
    verified: bool,
    verifier: (optional principal),
    damage-estimate: uint,
    affected-population: uint,
    emergency-contact: (string-ascii 100),
    status: (string-ascii 20)
  }
)

(define-map reporter-stats
  principal
  {
    total-reports: uint,
    verified-reports: uint,
    reputation-score: uint
  }
)

(define-map location-incidents
  (string-ascii 100)
  {
    incident-count: uint,
    last-incident: uint,
    severity-total: uint
  }
)

(define-map disaster-type-stats
  (string-ascii 50)
  {
    total-incidents: uint,
    avg-severity: uint,
    total-damage: uint
  }
)

(define-map authorized-verifiers principal bool)

(define-public (submit-disaster-report
  (disaster-type (string-ascii 50))
  (location (string-ascii 100))
  (severity uint)
  (description (string-ascii 500))
  (damage-estimate uint)
  (affected-population uint)
  (emergency-contact (string-ascii 100))
)
  (let
    (
      (report-id (var-get next-report-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (not (var-get contract-paused)) err-unauthorized)
    (asserts! (and (> severity u0) (<= severity u10)) err-invalid-input)
    (asserts! (> (len disaster-type) u0) err-invalid-input)
    (asserts! (> (len location) u0) err-invalid-input)
    
    (try! (nft-mint? disaster-report report-id tx-sender))
    
    (map-set disaster-reports report-id
      {
        reporter: tx-sender,
        disaster-type: disaster-type,
        location: location,
        severity: severity,
        timestamp: current-time,
        description: description,
        verified: false,
        verifier: none,
        damage-estimate: damage-estimate,
        affected-population: affected-population,
        emergency-contact: emergency-contact,
        status: "pending"
      }
    )
    
    (update-reporter-stats tx-sender)
    (update-location-stats location severity)
    (update-disaster-type-stats disaster-type severity damage-estimate)
    
    (var-set next-report-id (+ report-id u1))
    (ok report-id)
  )
)

(define-public (verify-report (report-id uint))
  (let
    (
      (report (unwrap! (map-get? disaster-reports report-id) err-not-found))
    )
    (asserts! (default-to false (map-get? authorized-verifiers tx-sender)) err-unauthorized)
    (asserts! (not (get verified report)) err-already-exists)
    
    (map-set disaster-reports report-id
      (merge report {
        verified: true,
        verifier: (some tx-sender),
        status: "verified"
      })
    )
    
    (update-reporter-verification (get reporter report))
    (ok true)
  )
)

(define-public (update-report-status (report-id uint) (new-status (string-ascii 20)))
  (let
    (
      (report (unwrap! (map-get? disaster-reports report-id) err-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender (get reporter report))
      (default-to false (map-get? authorized-verifiers tx-sender))
      (is-eq tx-sender contract-owner)
    ) err-unauthorized)
    
    (map-set disaster-reports report-id
      (merge report { status: new-status })
    )
    (ok true)
  )
)

(define-public (transfer-report (report-id uint) (recipient principal))
  (let
    (
      (report (unwrap! (map-get? disaster-reports report-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get reporter report)) err-unauthorized)
    (try! (nft-transfer? disaster-report report-id tx-sender recipient))
    
    (map-set disaster-reports report-id
      (merge report { reporter: recipient })
    )
    (ok true)
  )
)

(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-read-only (get-disaster-report (report-id uint))
  (map-get? disaster-reports report-id)
)

(define-read-only (get-reporter-stats (reporter principal))
  (map-get? reporter-stats reporter)
)

(define-read-only (get-location-stats (location (string-ascii 100)))
  (map-get? location-incidents location)
)

(define-read-only (get-disaster-type-stats (disaster-type (string-ascii 50)))
  (map-get? disaster-type-stats disaster-type)
)

(define-read-only (get-report-owner (report-id uint))
  (ok (nft-get-owner? disaster-report report-id))
)

(define-read-only (get-next-report-id)
  (var-get next-report-id)
)

(define-read-only (is-verifier (address principal))
  (default-to false (map-get? authorized-verifiers address))
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-read-only (get-reports-by-location (location (string-ascii 100)))
  (map-get? location-incidents location)
)

(define-read-only (calculate-risk-score (location (string-ascii 100)))
  (match (map-get? location-incidents location)
    location-data
    (let
      (
        (incident-count (get incident-count location-data))
        (avg-severity (if (> incident-count u0) 
          (/ (get severity-total location-data) incident-count) 
          u0))
      )
      (ok (* incident-count avg-severity))
    )
    (ok u0)
  )
)

(define-private (update-reporter-stats (reporter principal))
  (let
    (
      (current-stats (default-to 
        { total-reports: u0, verified-reports: u0, reputation-score: u0 }
        (map-get? reporter-stats reporter)
      ))
    )
    (map-set reporter-stats reporter
      (merge current-stats {
        total-reports: (+ (get total-reports current-stats) u1),
        reputation-score: (+ (get reputation-score current-stats) u10)
      })
    )
  )
)

(define-private (update-reporter-verification (reporter principal))
  (let
    (
      (current-stats (default-to 
        { total-reports: u0, verified-reports: u0, reputation-score: u0 }
        (map-get? reporter-stats reporter)
      ))
    )
    (map-set reporter-stats reporter
      (merge current-stats {
        verified-reports: (+ (get verified-reports current-stats) u1),
        reputation-score: (+ (get reputation-score current-stats) u50)
      })
    )
  )
)

(define-private (update-location-stats (location (string-ascii 100)) (severity uint))
  (let
    (
      (current-stats (default-to 
        { incident-count: u0, last-incident: u0, severity-total: u0 }
        (map-get? location-incidents location)
      ))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (map-set location-incidents location
      (merge current-stats {
        incident-count: (+ (get incident-count current-stats) u1),
        last-incident: current-time,
        severity-total: (+ (get severity-total current-stats) severity)
      })
    )
  )
)

(define-private (update-disaster-type-stats (disaster-type (string-ascii 50)) (severity uint) (damage uint))
  (let
    (
      (current-stats (default-to 
        { total-incidents: u0, avg-severity: u0, total-damage: u0 }
        (map-get? disaster-type-stats disaster-type)
      ))
      (new-total (+ (get total-incidents current-stats) u1))
      (new-severity-total (+ (* (get avg-severity current-stats) (get total-incidents current-stats)) severity))
    )
    (map-set disaster-type-stats disaster-type {
      total-incidents: new-total,
      avg-severity: (/ new-severity-total new-total),
      total-damage: (+ (get total-damage current-stats) damage)
    })
  )
)
;; title: Aftershock
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

