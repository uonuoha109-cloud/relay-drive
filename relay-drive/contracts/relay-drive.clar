;; RelayDrive - Hierarchical Delegation System for DAO Governance
;; Core governance contract with delegation, committee formation, and reputation tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-delegation (err u104))
(define-constant err-committee-full (err u105))
(define-constant err-insufficient-reputation (err u106))

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var committee-nonce uint u0)
(define-data-var min-reputation-threshold uint u100)
(define-data-var max-committee-size uint u10)
(define-data-var rotation-period uint u144) ;; approximately 1 day in blocks

;; Data Maps

;; Member profiles with skills and reputation
(define-map members
    principal
    {
        reputation: uint,
        total-delegated-power: uint,
        active-since: uint,
        last-activity: uint
    }
)

;; Member skills (composite key: member + skill-id)
(define-map member-skills
    { member: principal, skill-id: uint }
    { proficiency: uint, verified: bool, verification-block: uint }
)

;; Delegation records
(define-map delegations
    { delegator: principal, committee-id: uint }
    { 
        voting-power: uint,
        delegated-at: uint,
        expires-at: uint,
        active: bool
    }
)

;; Committee details
(define-map committees
    uint
    {
        name: (string-ascii 50),
        required-skills: (list 5 uint),
        chair: (optional principal),
        member-count: uint,
        created-at: uint,
        rotation-due: uint,
        active: bool
    }
)

;; Committee membership
(define-map committee-members
    { committee-id: uint, member: principal }
    { 
        role: (string-ascii 20),
        joined-at: uint,
        voting-weight: uint
    }
)

;; Proposals
(define-map proposals
    uint
    {
        title: (string-ascii 100),
        creator: principal,
        committee-id: uint,
        required-skills: (list 5 uint),
        votes-for: uint,
        votes-against: uint,
        created-at: uint,
        voting-ends: uint,
        executed: bool
    }
)

;; Votes cast
(define-map votes
    { proposal-id: uint, voter: principal }
    { weight: uint, in-favor: bool, voted-at: uint }
)

;; Read-only functions

(define-read-only (get-member (member principal))
    (map-get? members member)
)

(define-read-only (get-member-skill (member principal) (skill-id uint))
    (map-get? member-skills { member: member, skill-id: skill-id })
)

(define-read-only (get-delegation (delegator principal) (committee-id uint))
    (map-get? delegations { delegator: delegator, committee-id: committee-id })
)

(define-read-only (get-committee (committee-id uint))
    (map-get? committees committee-id)
)

(define-read-only (get-committee-member (committee-id uint) (member principal))
    (map-get? committee-members { committee-id: committee-id, member: member })
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (calculate-voting-power (member principal) (committee-id uint))
    (let
        (
            (member-data (unwrap! (get-member member) u0))
            (delegation (get-delegation member committee-id))
        )
        (+ (get reputation member-data)
           (match delegation
               some-delegation (if (get active some-delegation) (get voting-power some-delegation) u0)
               u0
           )
        )
    )
)

;; Public functions

;; Register as a member
(define-public (register-member)
    (let
        (
            (existing (get-member tx-sender))
        )
        (asserts! (is-none existing) err-already-exists)
        (ok (map-set members tx-sender {
            reputation: u100,
            total-delegated-power: u0,
            active-since: block-height,
            last-activity: block-height
        }))
    )
)

;; Add or update skill for member
(define-public (add-skill (skill-id uint) (proficiency uint))
    (let
        (
            (member-data (unwrap! (get-member tx-sender) err-not-found))
        )
        (ok (map-set member-skills 
            { member: tx-sender, skill-id: skill-id }
            { 
                proficiency: proficiency,
                verified: false,
                verification-block: u0
            }
        ))
    )
)

;; Create a new committee
(define-public (create-committee (name (string-ascii 50)) (required-skills (list 5 uint)))
    (let
        (
            (committee-id (var-get committee-nonce))
            (member-data (unwrap! (get-member tx-sender) err-not-found))
        )
        (asserts! (>= (get reputation member-data) (var-get min-reputation-threshold)) err-insufficient-reputation)
        (map-set committees committee-id {
            name: name,
            required-skills: required-skills,
            chair: (some tx-sender),
            member-count: u1,
            created-at: block-height,
            rotation-due: (+ block-height (var-get rotation-period)),
            active: true
        })
        (map-set committee-members
            { committee-id: committee-id, member: tx-sender }
            { role: "chair", joined-at: block-height, voting-weight: u100 }
        )
        (var-set committee-nonce (+ committee-id u1))
        (ok committee-id)
    )
)

;; Join a committee
(define-public (join-committee (committee-id uint))
    (let
        (
            (committee (unwrap! (get-committee committee-id) err-not-found))
            (member-data (unwrap! (get-member tx-sender) err-not-found))
            (existing-membership (get-committee-member committee-id tx-sender))
        )
        (asserts! (is-none existing-membership) err-already-exists)
        (asserts! (get active committee) err-unauthorized)
        (asserts! (< (get member-count committee) (var-get max-committee-size)) err-committee-full)
        (map-set committee-members
            { committee-id: committee-id, member: tx-sender }
            { role: "member", joined-at: block-height, voting-weight: (get reputation member-data) }
        )
        (map-set committees committee-id
            (merge committee { member-count: (+ (get member-count committee) u1) })
        )
        (ok true)
    )
)

;; Delegate voting power to a committee
(define-public (delegate-to-committee (committee-id uint) (voting-power uint))
    (let
        (
            (committee (unwrap! (get-committee committee-id) err-not-found))
            (member-data (unwrap! (get-member tx-sender) err-not-found))
        )
        (asserts! (get active committee) err-unauthorized)
        (asserts! (<= voting-power (get reputation member-data)) err-invalid-delegation)
        (map-set delegations
            { delegator: tx-sender, committee-id: committee-id }
            {
                voting-power: voting-power,
                delegated-at: block-height,
                expires-at: (+ block-height (var-get rotation-period)),
                active: true
            }
        )
        (ok true)
    )
)

;; Create a proposal
(define-public (create-proposal 
    (title (string-ascii 100))
    (committee-id uint)
    (required-skills (list 5 uint))
    (voting-duration uint))
    (let
        (
            (proposal-id (var-get proposal-nonce))
            (committee (unwrap! (get-committee committee-id) err-not-found))
            (membership (unwrap! (get-committee-member committee-id tx-sender) err-unauthorized))
        )
        (map-set proposals proposal-id {
            title: title,
            creator: tx-sender,
            committee-id: committee-id,
            required-skills: required-skills,
            votes-for: u0,
            votes-against: u0,
            created-at: block-height,
            voting-ends: (+ block-height voting-duration),
            executed: false
        })
        (var-set proposal-nonce (+ proposal-id u1))
        (ok proposal-id)
    )
)

;; Cast a vote on a proposal
(define-public (cast-vote (proposal-id uint) (in-favor bool))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) err-not-found))
            (existing-vote (get-vote proposal-id tx-sender))
            (voting-power (calculate-voting-power tx-sender (get committee-id proposal)))
        )
        (asserts! (is-none existing-vote) err-already-exists)
        (asserts! (< block-height (get voting-ends proposal)) err-unauthorized)
        (asserts! (not (get executed proposal)) err-unauthorized)
        (map-set votes
            { proposal-id: proposal-id, voter: tx-sender }
            { weight: voting-power, in-favor: in-favor, voted-at: block-height }
        )
        (if in-favor
            (map-set proposals proposal-id
                (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) })
            )
            (map-set proposals proposal-id
                (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) })
            )
        )
        (ok true)
    )
)

;; Update member reputation (contract owner only)
(define-public (update-reputation (member principal) (new-reputation uint))
    (let
        (
            (member-data (unwrap! (get-member member) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set members member
            (merge member-data { reputation: new-reputation, last-activity: block-height })
        ))
    )
)

;; Rotate committee chair
(define-public (rotate-committee-chair (committee-id uint) (new-chair principal))
    (let
        (
            (committee (unwrap! (get-committee committee-id) err-not-found))
            (caller-membership (unwrap! (get-committee-member committee-id tx-sender) err-unauthorized))
            (new-chair-membership (unwrap! (get-committee-member committee-id new-chair) err-not-found))
        )
        (asserts! (is-eq (get role caller-membership) "chair") err-unauthorized)
        (asserts! (>= block-height (get rotation-due committee)) err-unauthorized)
        (map-set committees committee-id
            (merge committee { 
                chair: (some new-chair),
                rotation-due: (+ block-height (var-get rotation-period))
            })
        )
        (map-set committee-members
            { committee-id: committee-id, member: new-chair }
            (merge new-chair-membership { role: "chair" })
        )
        (map-set committee-members
            { committee-id: committee-id, member: tx-sender }
            (merge caller-membership { role: "member" })
        )
        (ok true)
    )
)