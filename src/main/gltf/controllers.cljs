(ns gltf.controllers (:require
                      [gltf.math.vec3 :as vec3]))

(defn handle-mouse-input [dx dy]
  (let [sensitivity (* 0.1 (/ js/Math.PI 180))]
    {:yaw-delta (* dx sensitivity)
     :pitch-delta (* dy sensitivity)}))

(defn handle-keyboard-input [pressed-buttons]
  (let [impulse
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
        {:keys [x-delta y-delta z-delta]} impulse]
    (vec3/create x-delta y-delta z-delta)))

