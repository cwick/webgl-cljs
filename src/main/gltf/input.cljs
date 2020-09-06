(ns gltf.input (:require
                [gltf.math.utils :as math]))

(defn create-axis [name]
  {:name name :value 0 :previous-raw-value 0})

(defn create-controller [axes]
  (into {} (map (fn [a] [(keyword (:name a)) a]) axes)))

(defn map-axis
  ([name map-fn selector]
   (map-axis name map-fn nil selector))

  ([name map-fn negative-selector positive-selector]
   {:name name
    :value 0
    :previous-raw-value 0
    :positive-selector positive-selector
    :negative-selector negative-selector
    :map-fn map-fn}))

(defn- get-raw-input-value [input-state selector]
  (if selector
    (let [raw-value (get-in input-state selector)]
      (if (= (second selector) :buttons)
        (if raw-value 1 0)
        raw-value))
    0))

(defn update-axis [axis input-state time-delta]
  (let [positive-value (get-raw-input-value input-state (:positive-selector axis))
        negative-value (get-raw-input-value input-state (:negative-selector axis))
        raw-value (- positive-value negative-value)]
    (assoc axis
           :value ((:map-fn axis) axis raw-value time-delta)
           :previous-raw-value raw-value)))

(defn update-controller [controller input-state time-delta]
  (persistent!
   (loop [new-controller (transient controller)
          axes controller]
     (if-let [[key axis] (first axes)]
       (recur (assoc! new-controller key (update-axis axis input-state time-delta))
              (rest axes))
       new-controller))))

(defn button-axis [_ raw-value]
  (math/round (math/clamp raw-value 0 1)))

(defn leading-edge-button-axis [axis raw-value]
  (let [previous-raw-value (:previous-raw-value axis)]
    (if (and (zero? previous-raw-value) (> raw-value previous-raw-value))
      1
      0)))

(defn absolute-axis [_ raw-value]
  (math/clamp raw-value -1 1))

(defn raw-axis [_ raw-value] raw-value)

(defn sensitivity [value]
  (fn [_ raw-value] (* raw-value value)))

(defn digital-to-absolute-axis [force counter-force]
  (fn [axis raw-value time-delta]
    (let [previous-value
          (:value axis)

         ; Pick a direction to apply an input force.
         ; If there's no user input on this axis we'll apply a force in the opposite
         ; direction to start returning the input position to neutral
          impulse
          (if (zero? raw-value)
            (- (js/Math.sign previous-value))
            raw-value)

        ; Pick between a counter-force and normal force
        ; Normal: Current position is zero, or in the same direction the user is already applying force
        ; Counter: User is applying force in the opposite direction from current input, or 
        ;   has let go of the controls on this axis.
        ; Having two different values here allows for better fine tuning. Having the counter force
        ;   higher than the normal force feels more responsive.
          acceleration
          (if (or (zero? raw-value)
                  (and
                   (not (zero? previous-value))
                   (not= (js/Math.sign raw-value) (js/Math.sign previous-value))))
            counter-force
            force)

          new-value
          (+ previous-value (* acceleration impulse time-delta))

        ; We need to detect if the force we're applying would cause the input value to flip
        ; to the other side of the axis. If that happens, we return the input to neutral
        ; so it doesn't go flying off in the other direction.
          new-value
          (if (and (zero? raw-value) (not= (js/Math.sign new-value) (js/Math.sign previous-value)))
            0
            new-value)]
      (math/clamp new-value -1 1))))