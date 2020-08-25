(ns gltf.controllers (:require
                      [gltf.math.vec3 :as vec3]))

(defn handle-mouse-input [dx dy]
  (let [sensitivity (* 0.1 (/ js/Math.PI 180))]
    {:yaw-delta (* dx sensitivity)
     :pitch-delta (* dy sensitivity)}))

(#{"KeyS" "ShiftLeft"} "ShiftLeft")

(defn handle-keyboard-input [pressed-buttons]
  (let [speed (if (pressed-buttons "ShiftLeft") 3 1)
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
        {:keys [x-delta y-delta z-delta]} impulse
        impulse-vector (vec3/create x-delta y-delta z-delta)]
    (if (== x-delta y-delta z-delta 0)
      impulse-vector
      (-> impulse-vector
          (vec3/normalize)
          (vec3/scale speed)))))

