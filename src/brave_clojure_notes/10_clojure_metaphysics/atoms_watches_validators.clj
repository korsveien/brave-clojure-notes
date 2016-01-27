(ns atoms-watches-validators)

;;---------------------- Atoms ------------------------
;; A reference type for shared, synchronous, independent state.
;; Atoms are free of race conditions.

;; we create an atom (a reference type), give it an identity (fred),
;; and set its initial state
(def fred (atom {:cuddle-hunger-level 0
                 :percent-detoriated  0}))

;; to get an atom's state you dereference it
;; atoms are 'synchronous', meaning we get the value immediately
;; as opposed to getting some time in the future when the value is 'known'
@fred
;; => {:cuddle-hunger-level 0, :percent-detoriated 0}

;; the atom returns an immutable value, which is inherently thread-safe
(let [zombie-state @fred]
  (if (>= (:percent-detoriated zombie-state) 50)
    (future (println (:percent-detoriated zombie-state)))))


(swap! fred
       (fn [current-state]
         (merge-with + current-state {:cuddle-hunger-level 1})))
;; => {:cuddle-hunger-level 1, :percent-detoriated 0}


(defn increase-cuddle-hunger-level
  [zombie-state increase-by]
  (merge-with + zombie-state {:cuddle-hunger-level increase-by}))

;; does not update the atom, because swap! is not called
(increase-cuddle-hunger-level @fred 10)

;; does update the atom, because we are calling swap!
(swap! fred increase-cuddle-hunger-level 10)

;; update-in is a neat little function
;; (update-in <map i want to update> <key path> <update func>)
(update-in {:a {:b 3}} [:a :b] inc)

;; update-in can be used to update the atom
(swap! fred update-in [:cuddle-hunger-level] + 10)

;; we can always retain previous atom states
(let [num (atom 1)
      s1 @num]
  (swap! num inc)
  (println "State 1: " s1)
  (println "Current state:" @num))

;; swap! implements compare-and-set semantics:
;; 1. It reads the current state of the atom.
;; 2. It then applies the update function to that state.
;; 3. It checks whether the previous state has changed meanwhile
;; 4. If not, it updates the atom to refer to the new value
;; 5. If it has, then swap! retries the process, starting over from step 1
;;
;; these semantics are meant to avoid any race conditions
;;
;; updates happen synchronously, so update functions do have the capability of
;; blocking their thread


;; we can also update the atom without checking its current value (overwrite)
(reset! fred {:cuddle-hunger-level 0
              :percent-detoriated  0})


;;---------------------- Watches ------------------------
;; Watches allows you to run a func when the reference reaches a given state

(defn shuffle-speed
  [zombie]
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-detoriated zombie))))

;; this is the function to run when a reference is updated
(defn shuffle-alert
  [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do
        (println "Run, you fool!")
        (println "The zombie's SPH is now " sph)
        (println "This message brought to your courtesy of " key))
      (do
        (println "All's well with " key)
        (println "Cuddle hunger: " (:cuddle-hunger-level new-state))
        (println "Percent deteriorated: " (:percent-deteriorated new-state))
        (println "SPH" sph)))))


(reset! fred {:cuddle-hunger-level 22
              :percent-detoriated  2})

;; add a watch to fred, the key param can be used to remove the watch later
(add-watch fred :fred-shuffle-alert shuffle-alert)

;; lets update the atom so the watch fn is called
(swap! fred update-in [:percent-detoriated] + 1)
(swap! fred update-in [:cuddle-hunger-level] + 30)

;;---------------------- Validators ------------------------
;; Validators let you specify the legal states of a ref

;; The exception would be thrown anyway, but we add a more descriptive message
(defn percent-detoriated-validator
  [{:keys [percent-detoriated]}]
  (or (and (>= percent-detoriated 0)
           (<= percent-detoriated 100))
      (throw (IllegalStateException. "That's not mathy!"))))

(def bobby
  (atom
    {:cuddle-hunger-level 0 :percent-detoriated 0}
    :validator percent-detoriated-validator))

;; This will throw an IllegalStateException
(swap! bobby update-in [:percent-detoriated] + 200)


;;---------------------- Refs ------------------------
;; Refs are capable of handling synchronization between different 'identities'












