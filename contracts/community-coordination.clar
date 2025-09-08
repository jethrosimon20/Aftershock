;; Community Coordination Hub
;; Facilitates volunteer management and community response coordination

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INVALID-INPUT (err u422))
(define-constant ERR-CAPACITY-EXCEEDED (err u507))
(define-constant ERR-INACTIVE-STATUS (err u410))
(define-constant ERR-TIME-EXPIRED (err u408))

;; Data variables
(define-data-var next-volunteer-id uint u1)
(define-data-var next-team-id uint u1)
(define-data-var next-task-id uint u1)
(define-data-var next-coordination-event-id uint u1)
(define-data-var system-active bool true)
(define-data-var max-team-size uint u20)

;; Volunteer registry
(define-map registered-volunteers
    uint
    {
        volunteer-principal: principal,
        name: (string-ascii 100),
        contact-info: (string-ascii 150),
        location: (string-ascii 100),
        skills: (list 7 (string-ascii 50)),
        availability-status: (string-ascii 20),
        experience-level: uint,
        languages: (list 5 (string-ascii 30)),
        registration-timestamp: uint,
        last-activity: uint,
        reputation-score: uint,
        total-hours-served: uint,
        active: bool
    }
)

(define-map volunteer-principal-to-id
    principal
    uint
)

;; Task management system
(define-map disaster-response-tasks
    uint
    {
        task-creator: principal,
        related-disaster-id: uint,
        task-title: (string-ascii 100),
        task-description: (string-ascii 500),
        location: (string-ascii 100),
        required-skills: (list 5 (string-ascii 50)),
        priority-level: uint,
        estimated-duration: uint,
        max-volunteers-needed: uint,
        current-volunteers-assigned: uint,
        task-status: (string-ascii 20),
        created-timestamp: uint,
        deadline: uint,
        completion-timestamp: (optional uint),
        safety-requirements: (list 5 (string-ascii 100)),
        equipment-needed: (list 10 (string-ascii 50)),
        task-coordinator: (optional principal)
    }
)

;; Team coordination
(define-map response-teams
    uint
    {
        team-leader: principal,
        team-name: (string-ascii 100),
        team-specialization: (string-ascii 50),
        team-location: (string-ascii 100),
        team-members: (list 20 uint),
        team-status: (string-ascii 20),
        formation-timestamp: uint,
        team-rating: uint,
        communication-channel: (string-ascii 100),
        active: bool
    }
)

;; Communication and coordination events
(define-map coordination-events
    uint
    {
        event-organizer: principal,
        event-type: (string-ascii 50),
        event-title: (string-ascii 100),
        event-description: (string-ascii 400),
        location: (string-ascii 100),
        scheduled-timestamp: uint,
        duration-minutes: uint,
        max-attendees: uint,
        current-attendees: uint,
        event-status: (string-ascii 20),
        priority-level: uint
    }
)

;; Volunteer task assignments
(define-map task-assignments
    { task-id: uint, volunteer-id: uint }
    {
        assignment-timestamp: uint,
        assignment-status: (string-ascii 20),
        check-in-timestamp: (optional uint),
        check-out-timestamp: (optional uint),
        hours-logged: uint,
        assigned-by: principal
    }
)

;; Event attendance tracking
(define-map event-attendees
    { event-id: uint, volunteer-id: uint }
    {
        registration-timestamp: uint,
        attendance-confirmed: bool,
        attendance-timestamp: (optional uint)
    }
)

;; Community feedback system
(define-map response-feedback
    uint
    {
        feedback-provider: uint,
        feedback-target-type: (string-ascii 20),
        target-id: uint,
        feedback-category: (string-ascii 50),
        rating: uint,
        feedback-text: (string-ascii 300),
        timestamp: uint,
        anonymous: bool
    }
)

(define-data-var next-feedback-id uint u1)

;; Volunteer registration and management
(define-public (register-volunteer
    (name (string-ascii 100))
    (contact-info (string-ascii 150))
    (location (string-ascii 100))
    (skills (list 7 (string-ascii 50)))
    (experience-level uint)
    (languages (list 5 (string-ascii 30)))
)
    (let ((volunteer-id (var-get next-volunteer-id))
          (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
        
        ;; Validate inputs
        (asserts! (var-get system-active) ERR-INACTIVE-STATUS)
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (> (len contact-info) u0) ERR-INVALID-INPUT)
        (asserts! (> (len location) u0) ERR-INVALID-INPUT)
        (asserts! (and (>= experience-level u1) (<= experience-level u10)) ERR-INVALID-INPUT)
        
        ;; Check if volunteer already registered
        (asserts! (is-none (map-get? volunteer-principal-to-id tx-sender)) ERR-ALREADY-EXISTS)
        
        ;; Register volunteer
        (map-set registered-volunteers volunteer-id
            {
                volunteer-principal: tx-sender,
                name: name,
                contact-info: contact-info,
                location: location,
                skills: skills,
                availability-status: "available",
                experience-level: experience-level,
                languages: languages,
                registration-timestamp: current-time,
                last-activity: current-time,
                reputation-score: u100,
                total-hours-served: u0,
                active: true
            }
        )
        
        ;; Map principal to volunteer ID
        (map-set volunteer-principal-to-id tx-sender volunteer-id)
        
        ;; Increment volunteer counter
        (var-set next-volunteer-id (+ volunteer-id u1))
        
        (ok volunteer-id)
    )
)

(define-public (create-disaster-response-task
    (related-disaster-id uint)
    (task-title (string-ascii 100))
    (task-description (string-ascii 500))
    (location (string-ascii 100))
    (required-skills (list 5 (string-ascii 50)))
    (priority-level uint)
    (estimated-duration uint)
    (max-volunteers-needed uint)
    (deadline-hours uint)
    (safety-requirements (list 5 (string-ascii 100)))
    (equipment-needed (list 10 (string-ascii 50)))
)
    (let ((task-id (var-get next-task-id))
          (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
        
        ;; Validate inputs
        (asserts! (var-get system-active) ERR-INACTIVE-STATUS)
        (asserts! (> (len task-title) u0) ERR-INVALID-INPUT)
        (asserts! (> (len task-description) u0) ERR-INVALID-INPUT)
        (asserts! (> (len location) u0) ERR-INVALID-INPUT)
        (asserts! (and (>= priority-level u1) (<= priority-level u5)) ERR-INVALID-INPUT)
        (asserts! (> max-volunteers-needed u0) ERR-INVALID-INPUT)
        (asserts! (> deadline-hours u0) ERR-INVALID-INPUT)
        
        ;; Ensure creator is registered volunteer
        (asserts! (is-some (map-get? volunteer-principal-to-id tx-sender)) ERR-NOT-AUTHORIZED)
        
        ;; Create task
        (map-set disaster-response-tasks task-id
            {
                task-creator: tx-sender,
                related-disaster-id: related-disaster-id,
                task-title: task-title,
                task-description: task-description,
                location: location,
                required-skills: required-skills,
                priority-level: priority-level,
                estimated-duration: estimated-duration,
                max-volunteers-needed: max-volunteers-needed,
                current-volunteers-assigned: u0,
                task-status: "open",
                created-timestamp: current-time,
                deadline: (+ current-time (* deadline-hours u3600)),
                completion-timestamp: none,
                safety-requirements: safety-requirements,
                equipment-needed: equipment-needed,
                task-coordinator: (some tx-sender)
            }
        )
        
        ;; Increment task counter
        (var-set next-task-id (+ task-id u1))
        
        (ok task-id)
    )
)

(define-public (volunteer-for-task (task-id uint))
    (let ((task-data (unwrap! (map-get? disaster-response-tasks task-id) ERR-NOT-FOUND))
          (volunteer-id (unwrap! (map-get? volunteer-principal-to-id tx-sender) ERR-NOT-AUTHORIZED))
          (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
        
        ;; Check task is still open and accepting volunteers
        (asserts! (is-eq (get task-status task-data) "open") ERR-INVALID-INPUT)
        (asserts! (< (get current-volunteers-assigned task-data) (get max-volunteers-needed task-data)) ERR-CAPACITY-EXCEEDED)
        (asserts! (< current-time (get deadline task-data)) ERR-TIME-EXPIRED)
        
        ;; Check if volunteer already assigned
        (asserts! (is-none (map-get? task-assignments {task-id: task-id, volunteer-id: volunteer-id})) ERR-ALREADY-EXISTS)
        
        ;; Create assignment
        (map-set task-assignments {task-id: task-id, volunteer-id: volunteer-id}
            {
                assignment-timestamp: current-time,
                assignment-status: "assigned",
                check-in-timestamp: none,
                check-out-timestamp: none,
                hours-logged: u0,
                assigned-by: tx-sender
            }
        )
        
        ;; Update task volunteer count
        (map-set disaster-response-tasks task-id
            (merge task-data {
                current-volunteers-assigned: (+ (get current-volunteers-assigned task-data) u1)
            })
        )
        
        (ok true)
    )
)

(define-public (form-response-team
    (team-name (string-ascii 100))
    (team-specialization (string-ascii 50))
    (team-location (string-ascii 100))
    (initial-members (list 20 uint))
    (communication-channel (string-ascii 100))
)
    (let ((team-id (var-get next-team-id))
          (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
        
        ;; Validate inputs
        (asserts! (var-get system-active) ERR-INACTIVE-STATUS)
        (asserts! (> (len team-name) u0) ERR-INVALID-INPUT)
        (asserts! (> (len team-specialization) u0) ERR-INVALID-INPUT)
        (asserts! (> (len team-location) u0) ERR-INVALID-INPUT)
        (asserts! (<= (len initial-members) (var-get max-team-size)) ERR-CAPACITY-EXCEEDED)
        
        ;; Validate team leader is registered volunteer
        (asserts! (is-some (map-get? volunteer-principal-to-id tx-sender)) ERR-NOT-AUTHORIZED)
        
        ;; Create team
        (map-set response-teams team-id
            {
                team-leader: tx-sender,
                team-name: team-name,
                team-specialization: team-specialization,
                team-location: team-location,
                team-members: initial-members,
                team-status: "forming",
                formation-timestamp: current-time,
                team-rating: u100,
                communication-channel: communication-channel,
                active: true
            }
        )
        
        ;; Increment team counter
        (var-set next-team-id (+ team-id u1))
        
        (ok team-id)
    )
)

(define-public (schedule-coordination-event
    (event-type (string-ascii 50))
    (event-title (string-ascii 100))
    (event-description (string-ascii 400))
    (location (string-ascii 100))
    (scheduled-timestamp uint)
    (duration-minutes uint)
    (max-attendees uint)
    (priority-level uint)
)
    (let ((event-id (var-get next-coordination-event-id))
          (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
        
        ;; Validate inputs
        (asserts! (var-get system-active) ERR-INACTIVE-STATUS)
        (asserts! (> (len event-title) u0) ERR-INVALID-INPUT)
        (asserts! (> (len event-description) u0) ERR-INVALID-INPUT)
        (asserts! (> scheduled-timestamp current-time) ERR-INVALID-INPUT)
        (asserts! (> duration-minutes u0) ERR-INVALID-INPUT)
        (asserts! (> max-attendees u0) ERR-INVALID-INPUT)
        (asserts! (and (>= priority-level u1) (<= priority-level u5)) ERR-INVALID-INPUT)
        
        ;; Ensure organizer is registered volunteer
        (asserts! (is-some (map-get? volunteer-principal-to-id tx-sender)) ERR-NOT-AUTHORIZED)
        
        ;; Create event
        (map-set coordination-events event-id
            {
                event-organizer: tx-sender,
                event-type: event-type,
                event-title: event-title,
                event-description: event-description,
                location: location,
                scheduled-timestamp: scheduled-timestamp,
                duration-minutes: duration-minutes,
                max-attendees: max-attendees,
                current-attendees: u0,
                event-status: "scheduled",
                priority-level: priority-level
            }
        )
        
        ;; Increment event counter
        (var-set next-coordination-event-id (+ event-id u1))
        
        (ok event-id)
    )
)

(define-public (submit-feedback
    (target-type (string-ascii 20))
    (target-id uint)
    (feedback-category (string-ascii 50))
    (rating uint)
    (feedback-text (string-ascii 300))
    (anonymous bool)
)
    (let ((feedback-id (var-get next-feedback-id))
          (volunteer-id (unwrap! (map-get? volunteer-principal-to-id tx-sender) ERR-NOT-AUTHORIZED))
          (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
        
        ;; Validate inputs
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-INPUT)
        (asserts! (> (len feedback-text) u0) ERR-INVALID-INPUT)
        (asserts! (> (len target-type) u0) ERR-INVALID-INPUT)
        
        ;; Create feedback entry
        (map-set response-feedback feedback-id
            {
                feedback-provider: volunteer-id,
                feedback-target-type: target-type,
                target-id: target-id,
                feedback-category: feedback-category,
                rating: rating,
                feedback-text: feedback-text,
                timestamp: current-time,
                anonymous: anonymous
            }
        )
        
        ;; Increment feedback counter
        (var-set next-feedback-id (+ feedback-id u1))
        
        (ok feedback-id)
    )
)

;; Read-only functions
(define-read-only (get-volunteer-info (volunteer-id uint))
    (map-get? registered-volunteers volunteer-id)
)

(define-read-only (get-volunteer-by-address (volunteer-principal principal))
    (match (map-get? volunteer-principal-to-id volunteer-principal)
        volunteer-id (map-get? registered-volunteers volunteer-id)
        none
    )
)

(define-read-only (get-task-info (task-id uint))
    (map-get? disaster-response-tasks task-id)
)

(define-read-only (get-team-info (team-id uint))
    (map-get? response-teams team-id)
)

(define-read-only (get-event-info (event-id uint))
    (map-get? coordination-events event-id)
)

(define-read-only (get-task-assignment (task-id uint) (volunteer-id uint))
    (map-get? task-assignments {task-id: task-id, volunteer-id: volunteer-id})
)

(define-read-only (get-event-attendance (event-id uint) (volunteer-id uint))
    (map-get? event-attendees {event-id: event-id, volunteer-id: volunteer-id})
)

(define-read-only (get-feedback (feedback-id uint))
    (map-get? response-feedback feedback-id)
)

(define-read-only (get-next-volunteer-id)
    (var-get next-volunteer-id)
)

(define-read-only (get-next-task-id)
    (var-get next-task-id)
)

(define-read-only (get-next-team-id)
    (var-get next-team-id)
)

(define-read-only (get-next-event-id)
    (var-get next-coordination-event-id)
)

(define-read-only (is-system-active)
    (var-get system-active)
)

;; Admin functions
(define-public (toggle-system-status)
    (begin
        (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
        (var-set system-active (not (var-get system-active)))
        (ok (var-get system-active))
    )
)

(define-public (set-max-team-size (new-size uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (> new-size u0) ERR-INVALID-INPUT)
        (var-set max-team-size new-size)
        (ok new-size)
    )
)

;; Import contract owner from main Aftershock contract
(define-constant contract-owner tx-sender)
