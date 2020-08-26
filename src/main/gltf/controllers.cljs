(ns gltf.controllers (:require
                      [gltf.math.vec3 :as vec3]))

(defn handle-mouse-input [dx dy]
  (let [sensitivity (* 0.1 (/ js/Math.PI 180))]
    {:yaw-delta (* dx sensitivity)
     :pitch-delta (* dy sensitivity)}))

(def SENSITIVITY 2)
(def DRAG 3)

(defn- get-input-axis-value [current user-impulse time]
  (let [impulse
        (if (zero? user-impulse)
          (- (js/Math.sign current))
          user-impulse)

        acceleration
        (if (zero? user-impulse)
          DRAG
          SENSITIVITY)

        clamp #(max -1 (min 1 %))

        new-value
        (+ current (* acceleration impulse time))]
    (clamp
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