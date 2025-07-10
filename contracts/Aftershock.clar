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

(define-data-var next-resource-id uint u1)
(define-data-var next-allocation-id uint u1)

(define-map registered-resources
  uint
  {
    resource-owner: principal,
    resource-type: (string-ascii 50),
    resource-name: (string-ascii 100),
    location: (string-ascii 100),
    quantity: uint,
    available-quantity: uint,
    contact-info: (string-ascii 100),
    last-updated: uint,
    active: bool
  }
)

(define-map resource-allocations
  uint
  {
    resource-id: uint,
    disaster-report-id: uint,
    allocated-quantity: uint,
    allocation-timestamp: uint,
    allocation-status: (string-ascii 20),
    priority-score: uint,
    allocator: principal,
    estimated-arrival: uint
  }
)

(define-map resource-types-registry
  (string-ascii 50)
  {
    total-registered: uint,
    total-available: uint,
    average-response-time: uint
  }
)

(define-map location-resource-index
  (string-ascii 100)
  {
    available-resources: uint,
    last-allocation: uint,
    resource-coverage-score: uint
  }
)

(define-map disaster-resource-requests
  uint
  {
    disaster-report-id: uint,
    requested-resources: (list 10 (string-ascii 50)),
    urgency-level: uint,
    estimated-duration: uint,
    request-timestamp: uint,
    fulfilled: bool
  }
)

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

(define-public (register-resource
  (resource-type (string-ascii 50))
  (resource-name (string-ascii 100))
  (location (string-ascii 100))
  (quantity uint)
  (contact-info (string-ascii 100))
)
  (let
    (
      (resource-id (var-get next-resource-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (not (var-get contract-paused)) err-unauthorized)
    (asserts! (> quantity u0) err-invalid-input)
    (asserts! (> (len resource-type) u0) err-invalid-input)
    (asserts! (> (len resource-name) u0) err-invalid-input)
    (asserts! (> (len location) u0) err-invalid-input)
    
    (map-set registered-resources resource-id
      {
        resource-owner: tx-sender,
        resource-type: resource-type,
        resource-name: resource-name,
        location: location,
        quantity: quantity,
        available-quantity: quantity,
        contact-info: contact-info,
        last-updated: current-time,
        active: true
      }
    )
    
    (update-resource-type-registry resource-type quantity)
    (update-location-resource-index location quantity)
    
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (update-resource-availability (resource-id uint) (new-available-quantity uint))
  (let
    (
      (resource (unwrap! (map-get? registered-resources resource-id) err-not-found))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender (get resource-owner resource)) err-unauthorized)
    (asserts! (<= new-available-quantity (get quantity resource)) err-invalid-input)
    
    (map-set registered-resources resource-id
      (merge resource {
        available-quantity: new-available-quantity,
        last-updated: current-time
      })
    )
    
    (let
      (
        (quantity-diff (- (get available-quantity resource) new-available-quantity))
      )
      (update-resource-type-availability (get resource-type resource) quantity-diff)
      (ok true)
    )
  )
)

(define-public (allocate-resource-to-disaster
  (resource-id uint)
  (disaster-report-id uint)
  (allocated-quantity uint)
  (estimated-arrival uint)
)
  (let
    (
      (resource (unwrap! (map-get? registered-resources resource-id) err-not-found))
      (disaster-data (unwrap! (map-get? disaster-reports disaster-report-id) err-not-found))
      (allocation-id (var-get next-allocation-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (priority-score (calculate-allocation-priority (get severity disaster-data) (get location disaster-data) (get location resource)))
    )
    (asserts! (or 
      (is-eq tx-sender (get resource-owner resource))
      (default-to false (map-get? authorized-verifiers tx-sender))
      (is-eq tx-sender contract-owner)
    ) err-unauthorized)
    (asserts! (>= (get available-quantity resource) allocated-quantity) err-invalid-input)
    (asserts! (> allocated-quantity u0) err-invalid-input)
    
    (map-set resource-allocations allocation-id
      {
        resource-id: resource-id,
        disaster-report-id: disaster-report-id,
        allocated-quantity: allocated-quantity,
        allocation-timestamp: current-time,
        allocation-status: "allocated",
        priority-score: priority-score,
        allocator: tx-sender,
        estimated-arrival: estimated-arrival
      }
    )
    
    (map-set registered-resources resource-id
      (merge resource {
        available-quantity: (- (get available-quantity resource) allocated-quantity),
        last-updated: current-time
      })
    )
    
    (var-set next-allocation-id (+ allocation-id u1))
    (ok allocation-id)
  )
)

(define-public (complete-resource-allocation (allocation-id uint))
  (let
    (
      (allocation (unwrap! (map-get? resource-allocations allocation-id) err-not-found))
      (resource (unwrap! (map-get? registered-resources (get resource-id allocation)) err-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender (get resource-owner resource))
      (is-eq tx-sender (get allocator allocation))
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (is-eq (get allocation-status allocation) "allocated") err-invalid-input)
    
    (map-set resource-allocations allocation-id
      (merge allocation { allocation-status: "completed" })
    )
    (ok true)
  )
)

(define-public (cancel-resource-allocation (allocation-id uint))
  (let
    (
      (allocation (unwrap! (map-get? resource-allocations allocation-id) err-not-found))
      (resource (unwrap! (map-get? registered-resources (get resource-id allocation)) err-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender (get resource-owner resource))
      (is-eq tx-sender (get allocator allocation))
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (is-eq (get allocation-status allocation) "allocated") err-invalid-input)
    
    (map-set resource-allocations allocation-id
      (merge allocation { allocation-status: "cancelled" })
    )
    
    (map-set registered-resources (get resource-id allocation)
      (merge resource {
        available-quantity: (+ (get available-quantity resource) (get allocated-quantity allocation))
      })
    )
    (ok true)
  )
)

(define-public (request-disaster-resources
  (disaster-report-id uint)
  (requested-resources (list 10 (string-ascii 50)))
  (urgency-level uint)
  (estimated-duration uint)
)
  (let
    (
      (disaster-info (unwrap! (map-get? disaster-reports disaster-report-id) err-not-found))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (or 
      (is-eq tx-sender (get reporter disaster-info))
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (and (> urgency-level u0) (<= urgency-level u10)) err-invalid-input)
    
    (map-set disaster-resource-requests disaster-report-id
      {
        disaster-report-id: disaster-report-id,
        requested-resources: requested-resources,
        urgency-level: urgency-level,
        estimated-duration: estimated-duration,
        request-timestamp: current-time,
        fulfilled: false
      }
    )
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

(define-read-only (get-registered-resource (resource-id uint))
  (map-get? registered-resources resource-id)
)

(define-read-only (get-resource-allocation (allocation-id uint))
  (map-get? resource-allocations allocation-id)
)

(define-read-only (get-resource-type-stats (resource-type (string-ascii 50)))
  (map-get? resource-types-registry resource-type)
)

(define-read-only (get-location-resource-coverage (location (string-ascii 100)))
  (map-get? location-resource-index location)
)

(define-read-only (get-disaster-resource-request (disaster-report-id uint))
  (map-get? disaster-resource-requests disaster-report-id)
)

(define-read-only (get-next-resource-id)
  (var-get next-resource-id)
)

(define-read-only (get-next-allocation-id)
  (var-get next-allocation-id)
)

(define-read-only (calculate-resource-distance-score (resource-location (string-ascii 100)) (disaster-location (string-ascii 100)))
  (if (is-eq resource-location disaster-location)
    u100
    (if (> (len resource-location) u0)
      (let
        (
          (resource-len (len resource-location))
          (disaster-len (len disaster-location))
          (location-hash-diff (if (> resource-len disaster-len) (- resource-len disaster-len) (- disaster-len resource-len)))
        )
        (if (> location-hash-diff u50)
          (- u100 location-hash-diff)
          (+ u50 (- u50 location-hash-diff))
        )
      )
      u0
    )
  )
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

(define-private (update-resource-type-registry (resource-type (string-ascii 50)) (quantity uint))
  (let
    (
      (current-stats (default-to 
        { total-registered: u0, total-available: u0, average-response-time: u0 }
        (map-get? resource-types-registry resource-type)
      ))
    )
    (map-set resource-types-registry resource-type {
      total-registered: (+ (get total-registered current-stats) quantity),
      total-available: (+ (get total-available current-stats) quantity),
      average-response-time: (get average-response-time current-stats)
    })
  )
)

(define-private (update-resource-type-availability (resource-type (string-ascii 50)) (quantity-change uint))
  (let
    (
      (current-stats (default-to 
        { total-registered: u0, total-available: u0, average-response-time: u0 }
        (map-get? resource-types-registry resource-type)
      ))
    )
    (map-set resource-types-registry resource-type
      (merge current-stats {
        total-available: (if (>= (get total-available current-stats) quantity-change)
          (- (get total-available current-stats) quantity-change)
          u0)
      })
    )
  )
)

(define-private (update-location-resource-index (location (string-ascii 100)) (quantity uint))
  (let
    (
      (current-stats (default-to 
        { available-resources: u0, last-allocation: u0, resource-coverage-score: u0 }
        (map-get? location-resource-index location)
      ))
      (new-available (+ (get available-resources current-stats) quantity))
      (coverage-score (if (> (+ (get resource-coverage-score current-stats) u10) u100) u100 (+ (get resource-coverage-score current-stats) u10)))
    )
    (map-set location-resource-index location {
      available-resources: new-available,
      last-allocation: (get last-allocation current-stats),
      resource-coverage-score: coverage-score
    })
  )
)

(define-private (calculate-allocation-priority (severity uint) (disaster-location (string-ascii 100)) (resource-location (string-ascii 100)))
  (let
    (
      (severity-weight (* severity u10))
      (distance-score (calculate-resource-distance-score resource-location disaster-location))
      (time-factor u20)
    )
    (+ severity-weight distance-score time-factor)
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

