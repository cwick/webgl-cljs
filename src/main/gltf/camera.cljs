(ns gltf.camera
  (:require
   [goog.math :refer [toRadians]]
   [goog.vec.vec3f :as vec3]
   [goog.vec.Quaternion :as quat]
   [goog.vec.mat4f :as mat4]))

(defn- apply-velocity [pos velocity time]
  (let [[x y z] pos]
    #js[(+ x (* (aget velocity 0) time))
        (+ y (* (aget velocity 1) time))
        (+ z (* (aget velocity 2) time))]))

(defn- move-camera [position forward impulse time]
  (let [right (vec3/cross forward #js[0 1 0] (vec3/create))
        forward-velocity (vec3/scale forward (impulse 0) (vec3/create))
        right-velocity (vec3/scale right (impulse 1) (vec3/create))
        velocity (vec3/add forward-velocity right-velocity (vec3/create))]
    [(apply-velocity position velocity time) velocity]))

(defn update-camera [camera time]
  (let [orientation (as-> (quat/createIdentityFloat32) q
                      (quat/rotateY q (toRadians (- (:yaw camera))) q)
                      (quat/rotateX q (toRadians (:pitch camera)) q))
        forward (quat/transformVec #js[0 0 -1] orientation (vec3/create))
        [position velocity] (move-camera (:position camera) forward (:impulse camera) time)
        camera-matrix (mat4/makeRotationTranslation (mat4/create) orientation position)
        view-matrix (as-> (mat4/createIdentity) q
                      (do (mat4/invert camera-matrix q) q))]
    (-> camera
        (assoc :look forward
               :velocity velocity
               :position position
               :view-matrix view-matrix))))