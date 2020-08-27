(ns gltf.controllers (:require
                      [gltf.math.vec3 :as vec3]))

(defn handle-mouse-input [dx dy]
  (let [sensitivity (* 0.1 (/ js/Math.PI 180))]
    {:yaw-delta (* dx sensitivity)
     :pitch-delta (* dy sensitivity)}))

(def FORCE 2)
(def COUNTER-FORCE 4)

(defn- get-input-axis-value [current user-impulse time]
  (let [; Pick a direction to apply an input force.
        ; If there's no user input on this axis we'll apply a force in the opposite
        ; direction to start returning the input position to neutral
        impulse
        (if (zero? user-impulse)
          (- (js/Math.sign current))
          user-impulse)

        ; Pick between a counter-force and normal force
        ; Normal: Current position is zero, or in the same direction the user is already applying force
        ; Counter: User is applying force in the opposite direction from current input, or 
        ;   has let go of the controls on this axis.
        ; Having two different values here allows for better fine tuning. Having the counter force
        ;   higher than the normal force feels more responsive.
        acceleration
        (if (or (zero? user-impulse)
                (and
                 (not (zero? current))
                 (not= (js/Math.sign user-impulse) (js/Math.sign current))))
          COUNTER-FORCE
          FORCE)

        clamp #(max -1 (min 1 %))

        new-value
        (+ current (* acceleration impulse time))]
    (clamp
     ; We need to detect if the force we're applying would cause the input value to flip
     ; to the other side of the axis. If that happens, we return the input to neutral
     ; so it doesn't go flying off in the other direction.
     (if (and (zero? user-impulse) (not= (js/Math.sign new-value) (js/Math.sign current)))
       0
       new-value))))

(defn handle-keyboard-input [state time]
  (let [pressed-buttons
        (:pressed-buttons state)

        impulse
        (cond-> {:x-delta 0 :y-delta 0 :z-delta 0}
          (pressed-buttons "KeyW")
          (update :z-delta inc)

          (pressed-buttons "KeyS")
          (update :z-delta dec)

          (pressed-buttons "KeyD")
          (update :x-delta inc)

          (pressed-buttons "KeyA")
          (update :x-delta dec)

          (pressed-buttons "KeyE")
          (update :y-delta inc)

          (pressed-buttons "KeyQ")
          (update :y-delta dec))

        {:keys [x-delta y-delta z-delta]}
        impulse

        old-impulse (:impulse state)

        impulse-vector
        (vec3/create (get-input-axis-value (nth old-impulse 0) x-delta time)
                     (get-input-axis-value (nth old-impulse 1) y-delta time)
                     (get-input-axis-value (nth old-impulse 2) z-delta time))]
    (assoc state :impulse impulse-vector)))