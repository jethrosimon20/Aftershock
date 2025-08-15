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

;; Dynamic Risk Assessment & Alert System Maps
(define-map risk-assessments
  (string-ascii 100)
  {
    location: (string-ascii 100),
    current-risk-level: uint,
    historical-incidents: uint,
    last-assessment: uint,
    risk-trend: (string-ascii 20),
    contributing-factors: (list 5 (string-ascii 50)),
    predicted-disaster-types: (list 3 (string-ascii 50)),
    confidence-score: uint
  }
)

(define-map alert-configurations
  (string-ascii 50)
  {
    alert-type: (string-ascii 50),
    risk-threshold: uint,
    auto-trigger: bool,
    notification-radius: uint,
    priority-level: uint,
    resource-preposition-enabled: bool,
    alert-frequency-limit: uint
  }
)

(define-map active-alerts
  uint
  {
    alert-id: uint,
    location: (string-ascii 100),
    alert-type: (string-ascii 50),
    risk-level: uint,
    triggered-at: uint,
    status: (string-ascii 20),
    affected-radius: uint,
    recommended-actions: (list 5 (string-ascii 100)),
    auto-triggered: bool,
    acknowledged-by: (optional principal),
    escalation-level: uint
  }
)

(define-map risk-prediction-models
  (string-ascii 50)
  {
    disaster-type: (string-ascii 50),
    seasonal-factor: uint,
    frequency-pattern: uint,
    severity-multiplier: uint,
    location-vulnerability: uint,
    prediction-accuracy: uint,
    last-updated: uint
  }
)

(define-map location-risk-history
  (string-ascii 100)
  {
    location: (string-ascii 100),
    risk-timeline: (list 10 uint),
    assessment-dates: (list 10 uint),
    max-risk-recorded: uint,
    risk-volatility: uint,
    trend-direction: (string-ascii 20)
  }
)

(define-data-var next-alert-id uint u1)
(define-data-var global-risk-threshold uint u70)
(define-data-var assessment-update-interval uint u86400) ;; 24 hours in seconds
(define-data-var alert-cooldown-period uint u3600) ;; 1 hour cooldown

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

;; Dynamic Risk Assessment & Alert System Functions

(define-public (configure-alert-type
  (alert-type (string-ascii 50))
  (risk-threshold uint)
  (auto-trigger bool)
  (notification-radius uint)
  (priority-level uint)
  (resource-preposition-enabled bool)
  (alert-frequency-limit uint)
)
  (begin
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (and (> risk-threshold u0) (<= risk-threshold u100)) err-invalid-input)
    (asserts! (and (> priority-level u0) (<= priority-level u5)) err-invalid-input)
    
    (map-set alert-configurations alert-type
      {
        alert-type: alert-type,
        risk-threshold: risk-threshold,
        auto-trigger: auto-trigger,
        notification-radius: notification-radius,
        priority-level: priority-level,
        resource-preposition-enabled: resource-preposition-enabled,
        alert-frequency-limit: alert-frequency-limit
      }
    )
    (ok true)
  )
)

(define-public (update-risk-assessment
  (location (string-ascii 100))
  (contributing-factors (list 5 (string-ascii 50)))
  (predicted-disaster-types (list 3 (string-ascii 50)))
)
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (location-stats (map-get? location-incidents location))
      (calculated-risk (calculate-dynamic-risk-level location contributing-factors))
      (risk-trend (determine-risk-trend location calculated-risk))
      (confidence-score (calculate-confidence-score location predicted-disaster-types))
    )
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (> (len location) u0) err-invalid-input)
    
    (begin
      (map-set risk-assessments location
        {
          location: location,
          current-risk-level: calculated-risk,
          historical-incidents: (match location-stats stats (get incident-count stats) u0),
          last-assessment: current-time,
          risk-trend: risk-trend,
          contributing-factors: contributing-factors,
          predicted-disaster-types: predicted-disaster-types,
          confidence-score: confidence-score
        }
      )
      
      (update-risk-history location calculated-risk current-time)
      (unwrap! (check-and-trigger-alerts location calculated-risk) err-invalid-input)
      (ok calculated-risk)
    )
  )
)

(define-public (trigger-manual-alert
  (location (string-ascii 100))
  (alert-type (string-ascii 50))
  (risk-level uint)
  (recommended-actions (list 5 (string-ascii 100)))
)
  (let
    (
      (alert-id (var-get next-alert-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (alert-config (map-get? alert-configurations alert-type))
    )
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (and (> risk-level u0) (<= risk-level u100)) err-invalid-input)
    (asserts! (> (len location) u0) err-invalid-input)
    
    (map-set active-alerts alert-id
      {
        alert-id: alert-id,
        location: location,
        alert-type: alert-type,
        risk-level: risk-level,
        triggered-at: current-time,
        status: "active",
        affected-radius: (match alert-config config (get notification-radius config) u0),
        recommended-actions: recommended-actions,
        auto-triggered: false,
        acknowledged-by: none,
        escalation-level: u1
      }
    )
    
    (var-set next-alert-id (+ alert-id u1))
    (ok alert-id)
  )
)

(define-public (acknowledge-alert (alert-id uint))
  (let
    (
      (alert (unwrap! (map-get? active-alerts alert-id) err-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (is-eq (get status alert) "active") err-invalid-input)
    
    (map-set active-alerts alert-id
      (merge alert {
        status: "acknowledged",
        acknowledged-by: (some tx-sender)
      })
    )
    (ok true)
  )
)

(define-public (escalate-alert (alert-id uint))
  (let
    (
      (alert (unwrap! (map-get? active-alerts alert-id) err-not-found))
      (current-escalation (get escalation-level alert))
    )
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (is-eq (get status alert) "active") err-invalid-input)
    (asserts! (< current-escalation u5) err-invalid-input)
    
    (map-set active-alerts alert-id
      (merge alert {
        escalation-level: (+ current-escalation u1),
        status: "escalated"
      })
    )
    (ok true)
  )
)

(define-public (resolve-alert (alert-id uint))
  (let
    (
      (alert (unwrap! (map-get? active-alerts alert-id) err-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    
    (map-set active-alerts alert-id
      (merge alert { status: "resolved" })
    )
    (ok true)
  )
)

(define-public (update-prediction-model
  (disaster-type (string-ascii 50))
  (seasonal-factor uint)
  (frequency-pattern uint)
  (severity-multiplier uint)
  (location-vulnerability uint)
)
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (existing-model (map-get? risk-prediction-models disaster-type))
      (accuracy-score (calculate-model-accuracy disaster-type))
    )
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (default-to false (map-get? authorized-verifiers tx-sender))
    ) err-unauthorized)
    (asserts! (and (> seasonal-factor u0) (<= seasonal-factor u100)) err-invalid-input)
    (asserts! (and (> severity-multiplier u0) (<= severity-multiplier u200)) err-invalid-input)
    
    (map-set risk-prediction-models disaster-type
      {
        disaster-type: disaster-type,
        seasonal-factor: seasonal-factor,
        frequency-pattern: frequency-pattern,
        severity-multiplier: severity-multiplier,
        location-vulnerability: location-vulnerability,
        prediction-accuracy: accuracy-score,
        last-updated: current-time
      }
    )
    (ok true)
  )
)

(define-public (set-global-risk-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (> new-threshold u0) (<= new-threshold u100)) err-invalid-input)
    (var-set global-risk-threshold new-threshold)
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

;; Dynamic Risk Assessment & Alert System Read-Only Functions

(define-read-only (get-risk-assessment (location (string-ascii 100)))
  (map-get? risk-assessments location)
)

(define-read-only (get-alert-configuration (alert-type (string-ascii 50)))
  (map-get? alert-configurations alert-type)
)

(define-read-only (get-active-alert (alert-id uint))
  (map-get? active-alerts alert-id)
)

(define-read-only (get-prediction-model (disaster-type (string-ascii 50)))
  (map-get? risk-prediction-models disaster-type)
)

(define-read-only (get-location-risk-history (location (string-ascii 100)))
  (map-get? location-risk-history location)
)

(define-read-only (get-global-risk-threshold)
  (var-get global-risk-threshold)
)

(define-read-only (get-next-alert-id)
  (var-get next-alert-id)
)

(define-read-only (calculate-location-risk-level (location (string-ascii 100)))
  (let
    (
      (location-stats (map-get? location-incidents location))
      (risk-assessment (map-get? risk-assessments location))
    )
    (match location-stats
      stats
      (let
        (
          (incident-count (get incident-count stats))
          (avg-severity (if (> incident-count u0) 
            (/ (get severity-total stats) incident-count) 
            u0))
          (time-factor (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
          (recency-factor (if (> (get last-incident stats) u0)
            (if (< (- time-factor (get last-incident stats)) u2592000) u20 u0) ;; 30 days
            u0))
        )
        (ok (+ (* incident-count u15) (* avg-severity u10) recency-factor))
      )
      (ok u0)
    )
  )
)

(define-read-only (get-high-risk-locations)
  (let
    (
      (threshold (var-get global-risk-threshold))
    )
    (ok threshold)
  )
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

;; Dynamic Risk Assessment & Alert System Private Functions

(define-private (calculate-dynamic-risk-level (location (string-ascii 100)) (contributing-factors (list 5 (string-ascii 50))))
  (let
    (
      (location-stats (map-get? location-incidents location))
      (base-risk (match location-stats 
        stats 
        (let
          (
            (incident-count (get incident-count stats))
            (avg-severity (if (> incident-count u0) 
              (/ (get severity-total stats) incident-count) 
              u0))
          )
          (+ (* incident-count u5) (* avg-severity u8))
        )
        u0))
      (factor-multiplier (calculate-factor-weight contributing-factors))
    )
    (let
      (
        (calculated-risk (+ base-risk factor-multiplier))
      )
      (if (> calculated-risk u100) u100 calculated-risk)
    )
  )
)

(define-private (calculate-factor-weight (factors (list 5 (string-ascii 50))))
  (fold calculate-single-factor-weight factors u0)
)

(define-private (calculate-single-factor-weight (factor (string-ascii 50)) (current-weight uint))
  (let
    (
      (factor-value (if (is-eq factor "weather-extreme") u15
        (if (is-eq factor "seismic-activity") u20
        (if (is-eq factor "infrastructure-age") u10
        (if (is-eq factor "population-density") u12
        (if (is-eq factor "emergency-preparedness") u8
        u5))))))
    )
    (+ current-weight factor-value)
  )
)

(define-private (determine-risk-trend (location (string-ascii 100)) (current-risk uint))
  (let
    (
      (risk-history (map-get? location-risk-history location))
    )
    (match risk-history
      history
      (let
        (
          (risk-timeline (get risk-timeline history))
          (last-risk (default-to u0 (element-at? risk-timeline u0)))
        )
        (if (> current-risk (+ last-risk u10))
          "increasing"
          (if (< current-risk (- last-risk u10))
            "decreasing"
            "stable"))
      )
      "new"
    )
  )
)

(define-private (calculate-confidence-score (location (string-ascii 100)) (predicted-types (list 3 (string-ascii 50))))
  (let
    (
      (location-stats (map-get? location-incidents location))
      (base-confidence (match location-stats 
        stats 
        (let
          (
            (incident-count (get incident-count stats))
          )
          (if (> incident-count u10) u80
          (if (> incident-count u5) u60
          (if (> incident-count u1) u40
          u20)))
        )
        u10))
      (prediction-factor (len predicted-types))
    )
    (+ base-confidence (* prediction-factor u5))
  )
)

(define-private (update-risk-history (location (string-ascii 100)) (risk-level uint) (timestamp uint))
  (let
    (
      (existing-history (map-get? location-risk-history location))
    )
    (match existing-history
      history
      (let
        (
          (current-timeline (get risk-timeline history))
          (current-dates (get assessment-dates history))
          (new-timeline (unwrap-panic (as-max-len? (append current-timeline risk-level) u10)))
          (new-dates (unwrap-panic (as-max-len? (append current-dates timestamp) u10)))
          (max-risk (if (> risk-level (get max-risk-recorded history)) risk-level (get max-risk-recorded history)))
        )
        (map-set location-risk-history location {
          location: location,
          risk-timeline: new-timeline,
          assessment-dates: new-dates,
          max-risk-recorded: max-risk,
          risk-volatility: (calculate-risk-volatility new-timeline),
          trend-direction: (determine-risk-trend location risk-level)
        })
      )
      (map-set location-risk-history location {
        location: location,
        risk-timeline: (list risk-level),
        assessment-dates: (list timestamp),
        max-risk-recorded: risk-level,
        risk-volatility: u0,
        trend-direction: "new"
      })
    )
  )
)

(define-private (calculate-risk-volatility (risk-timeline (list 10 uint)))
  (if (> (len risk-timeline) u1)
    (let
      (
        (first-risk (default-to u0 (element-at? risk-timeline u0)))
        (last-risk (default-to u0 (element-at? risk-timeline (- (len risk-timeline) u1))))
        (volatility (if (> first-risk last-risk) (- first-risk last-risk) (- last-risk first-risk)))
      )
      volatility
    )
    u0
  )
)

(define-private (check-and-trigger-alerts (location (string-ascii 100)) (risk-level uint))
  (let
    (
      (threshold (var-get global-risk-threshold))
    )
    (if (>= risk-level threshold)
      (auto-trigger-alert location risk-level)
      (ok false)
    )
  )
)

(define-private (auto-trigger-alert (location (string-ascii 100)) (risk-level uint))
  (let
    (
      (alert-id (var-get next-alert-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (map-set active-alerts alert-id
      {
        alert-id: alert-id,
        location: location,
        alert-type: "high-risk-detected",
        risk-level: risk-level,
        triggered-at: current-time,
        status: "active",
        affected-radius: u0,
        recommended-actions: (list "assess-situation" "prepare-resources" "notify-authorities"),
        auto-triggered: true,
        acknowledged-by: none,
        escalation-level: u1
      }
    )
    (var-set next-alert-id (+ alert-id u1))
    (ok true)
  )
)

(define-private (calculate-model-accuracy (disaster-type (string-ascii 50)))
  (let
    (
      (disaster-stats (map-get? disaster-type-stats disaster-type))
    )
    (match disaster-stats
      stats
      (let
        (
          (total-incidents (get total-incidents stats))
        )
        (if (> total-incidents u20) u85
        (if (> total-incidents u10) u70
        (if (> total-incidents u5) u55
        u40)))
      )
      u30
    )
  )
)

