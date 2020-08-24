(ns gltf.controllers (:require
                      [goog.vec.vec3f :as vec3]))

(defn handle-mouse-input [dx dy]
  (let [sensitivity (* 0.1 (/ js/Math.PI 180))]
    {:yaw-delta (* dx sensitivity)
     :pitch-delta (* dy sensitivity)}))

(defn handle-keyboard-input [pressed-buttons]
  (let [impulse
        (cond-> {:x-delta 0 :y-delta 0 :z-delta 0}
          (contains? pressed-buttons "KeyW")
          (update :z-delta inc)
          (contains? pressed-buttons "KeyS")
          (update :z-delta dec)
          (contains? pressed-buttons "KeyD")
          (update :x-delta inc)
          (contains? pressed-buttons "KeyA")
          (update :x-delta dec)
          (contains? pressed-buttons "KeyQ")
          (update :y-delta inc)
          (contains? pressed-buttons "KeyE")
          (update :y-delta dec))
        impulse-vector [(:x-delta impulse) (:y-delta impulse) (:z-delta impulse)]]
    (if (= [0 0 0] impulse-vector)
      [0 0 0]
      (js->clj (vec3/normalize (clj->js impulse-vector) #js[])))))

