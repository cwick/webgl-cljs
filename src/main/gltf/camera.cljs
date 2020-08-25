(ns gltf.camera
  (:require
   [gltf.math.mat4 :as mat4]
   [gltf.math.vec3 :as vec3]))

(defn- move-camera [camera orientation time]
  (let [camera-speed 10
        [impulse-x impulse-y impulse-z] (map #(* camera-speed %) (:impulse camera))
        ; Extract forward and right vectors from the rotation matrix
        ; https://community.khronos.org/t/get-direction-from-transformation-matrix-or-quat/65502/2
        forward (-> (mat4/get-column orientation 2) (vec3/negate))
        right (mat4/get-column orientation 0)
        forward-velocity (vec3/scale forward impulse-z)
        right-velocity (vec3/scale right impulse-x)
        up-velocity (vec3/scale (vec3/create 0 1 0) impulse-y)
        velocity (vec3/add  forward-velocity right-velocity up-velocity)]
    (assoc camera
           :look forward
           :velocity velocity
           :position (vec3/scale-and-add (:position camera) velocity time))))

(defn- update-orientation
  "Adjust yaw/pitch based on desired change"
  [camera]
  (letfn [(clamp [x] (max (- (/ js/Math.PI 2))
                          (min (/ js/Math.PI 2) x)))]
    (cond-> camera
      (some? (:yaw-delta camera))
      (-> (update :yaw #(mod
                         (+ % (:yaw-delta camera))
                         (* 2 js/Math.PI)))
          (dissoc :yaw-delta))

      (some? (:pitch-delta camera))
      (-> (update :pitch #(clamp (+ % (:pitch-delta camera))))
          (dissoc :pitch-delta)))))

(defn update-camera [camera time]
  (let [camera (update-orientation camera)
        orientation (-> (mat4/create-identity)
                        (mat4/rotate-y! (- (:yaw camera)))
                        (mat4/rotate-x! (:pitch camera)))
        camera (move-camera camera orientation time)
        [x y z] (:position camera)
        ; Inverse of pure rotation matrix is its transpose
        view-matrix (-> (mat4/transpose orientation)
                        (mat4/translate! (- x) (- y) (- z)))]
    (assoc camera :view-matrix view-matrix)))