(ns gltf.camera
  (:require
   [gltf.math.mat4 :as mat4]
   [gltf.math.vec3 :as vec3]
   [gltf.ui :as ui]))

(defn- update-velocity [camera]
  (let [max-speed 10
        orientation (:orientation camera)
        [impulse-x impulse-y impulse-z] (:impulse camera)
        ; Extract forward and right vectors from the rotation matrix
        ; https://community.khronos.org/t/get-direction-from-transformation-matrix-or-quat/65502/2
        forward-velocity (-> (mat4/get-column orientation 2)
                             (vec3/negate!)
                             (vec3/scale! impulse-z))
        right-velocity (-> (mat4/get-column orientation 0)
                           (vec3/scale! impulse-x))
        up-velocity (-> (vec3/world-up)
                        (vec3/scale! impulse-y))
        velocity (-> (vec3/add forward-velocity right-velocity up-velocity)
                     (vec3/scale! max-speed)
                     (vec3/clamp! max-speed))]
    (assoc camera
           :velocity velocity)))

(defn- move-camera [camera time]
  (let [camera (update-velocity camera)
        velocity (:velocity camera)
        position (:position camera)]
    (ui/debug (str "V: " (.toFixed (vec3/magnitude velocity) 2)))
    (assoc camera
           :position (vec3/scale-and-add position velocity time))))

(defn- update-pitch-yaw
  "Adjust yaw/pitch based on desired change"
  [camera]
  (letfn [(clamp [x] (max (- (/ js/Math.PI 2))
                          (min (/ js/Math.PI 2) x)))]
    (cond-> camera
      (some? (:yaw-delta camera))
      (-> (update :yaw #(mod
                         (+ % (:yaw-delta camera))
                         (* 2 js/Math.PI)))
          ; TODO: cancel pitch and yaw somewhere else?
          (dissoc :yaw-delta))

      (some? (:pitch-delta camera))
      (-> (update :pitch #(clamp (+ % (:pitch-delta camera))))
          (dissoc :pitch-delta)))))

(defn- update-orientation [camera]
  (-> (update-pitch-yaw camera)
      (assoc :orientation
             (-> (mat4/create-identity)
                 (mat4/rotate-y! (- (:yaw camera)))
                 (mat4/rotate-x! (:pitch camera))))))

(defn update-camera [camera time]
  (let [camera (-> (update-orientation camera)
                   (move-camera time))
        [x y z] (:position camera)
        ; Inverse of pure rotation matrix is its transpose
        view-matrix (-> (mat4/transpose (:orientation camera))
                        (mat4/translate! (- x) (- y) (- z)))]
    (assoc camera :view-matrix view-matrix)))