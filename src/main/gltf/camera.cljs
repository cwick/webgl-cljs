(ns gltf.camera
  (:require
   [goog.math :refer [toRadians]]
   [goog.vec.vec3f :as vec3]
   [goog.vec.mat4f :as mat4]))

(defn- apply-velocity [pos velocity time]
  (let [[x y z] pos]
    #js[(+ x (* (aget velocity 0) time))
        (+ y (* (aget velocity 1) time))
        (+ z (* (aget velocity 2) time))]))

(defn- move-camera [camera orientation time]
  (let [impulse (:impulse camera)
        ; Extract forward and right vectors from the rotation matrix
        ; https://community.khronos.org/t/get-direction-from-transformation-matrix-or-quat/65502/2
        forward (as-> (mat4/getColumn orientation 2 (vec3/create)) v (vec3/negate v v))
        right (mat4/getColumn orientation 0 (vec3/create))
        forward-velocity (vec3/scale forward (impulse 0) (vec3/create))
        right-velocity (vec3/scale right (impulse 1) (vec3/create))
        velocity (vec3/add forward-velocity right-velocity (vec3/create))]

    (assoc camera
           :look forward
           :velocity velocity
           :position (apply-velocity (:position camera) velocity time))))

(defn update-camera [camera time]
  (let [orientation (as-> (mat4/createIdentity) m
                      (mat4/rotateY m (toRadians (- (:yaw camera))))
                      (mat4/rotateX m (toRadians (:pitch camera))))
        camera (move-camera camera orientation time)
        [x y z] (:position camera)
        ; Inverse of pure rotation matrix is its transpose
        view-matrix (as-> (mat4/transpose orientation (mat4/create)) m
                      (mat4/translate m (- x) (- y) (- z)))]
    (assoc camera :view-matrix view-matrix)))