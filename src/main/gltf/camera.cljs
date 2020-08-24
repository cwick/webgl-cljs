(ns gltf.camera
  (:require
   [goog.vec.vec3f :as vec3]
   [goog.vec.mat4f :as mat4]))

(defn- apply-velocity [pos velocity time]
  (let [[x y z] pos]
    #js[(+ x (* (aget velocity 0) time))
        (+ y (* (aget velocity 1) time))
        (+ z (* (aget velocity 2) time))]))

(defn- move-camera [camera orientation time]
  (let [camera-speed 10
        [impulse-x impulse-y impulse-z] (map #(* camera-speed %) (:impulse camera))
        ; Extract forward and right vectors from the rotation matrix
        ; https://community.khronos.org/t/get-direction-from-transformation-matrix-or-quat/65502/2
        forward (as-> (mat4/getColumn orientation 2 (vec3/create)) v (vec3/negate v v))
        right (mat4/getColumn orientation 0 (vec3/create))
        forward-velocity (vec3/scale forward impulse-z (vec3/create))
        right-velocity (vec3/scale right impulse-x (vec3/create))
        up-velocity (vec3/scale #js[0 1 0] impulse-y (vec3/create))
        velocity (as-> (vec3/create) velocity
                   (vec3/add forward-velocity right-velocity velocity)
                   (vec3/add velocity up-velocity velocity))]
    (assoc camera
           :look forward
           :velocity velocity
           :position (apply-velocity (:position camera) velocity time))))

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
        orientation (as-> (mat4/createIdentity) m
                      (mat4/rotateY m (- (:yaw camera)))
                      (mat4/rotateX m (:pitch camera)))
        camera (move-camera camera orientation time)
        [x y z] (:position camera)
        ; Inverse of pure rotation matrix is its transpose
        view-matrix (as-> (mat4/transpose orientation (mat4/create)) m
                      (mat4/translate m (- x) (- y) (- z)))]
    (assoc camera :view-matrix view-matrix)))