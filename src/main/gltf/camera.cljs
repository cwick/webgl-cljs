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
    (assoc camera
           :position (vec3/scale-and-add position velocity time))))

(defn- update-pitch-yaw
  "Adjust yaw/pitch based on desired change"
  [camera]
  (letfn [(clamp [x] (max (- (/ js/Math.PI 2))
                          (min (/ js/Math.PI 2) x)))]
    (-> camera
        (update :yaw #(mod
                       (+ % (:yaw-delta camera))
                       (* 2 js/Math.PI)))
        (update :pitch #(clamp (+ % (:pitch-delta camera)))))))

(defn- update-orientation [camera]
  (-> camera
      (update-pitch-yaw)
      (as-> camera
            (assoc camera :orientation
                   (-> (mat4/create-identity)
                       (mat4/rotate-y! (- (:yaw camera)))
                       (mat4/rotate-x! (:pitch camera)))))))

(defn- update-fly-movement [camera time]
  (-> camera
      (update-orientation)
      (move-camera time)))

(defn- update-orbit-movement [camera]
  (let [yaw-delta (:yaw-delta camera)]
    (-> camera
        (update :position
                #(-> (mat4/create-identity)
                     (mat4/rotate-y! yaw-delta)
                     (mat4/mult-vec3 %)))
        (assoc :yaw-delta (- yaw-delta))
        (update-orientation))))

(defn update-camera [camera time]
  (let [camera (if (:orbit? camera)
                 (update-orbit-movement camera)
                 (update-fly-movement camera time))
        [x y z] (:position camera)
        ; Inverse of pure rotation matrix is its transpose
        view-matrix (-> (mat4/transpose (:orientation camera))
                        (mat4/translate! (- x) (- y) (- z)))]
    (-> camera
        (assoc :view-matrix view-matrix)
        (assoc :yaw-delta 0 :pitch-delta 0))))

(defn create []
  {:yaw 0
   :pitch 0
   :position (vec3/create 0 1 3.5)
   :velocity (vec3/zero)
   :orientation (mat4/create-identity)})