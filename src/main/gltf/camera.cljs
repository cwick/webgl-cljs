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

(defn- move-camera [camera orientation time]
  (let [impulse (:impulse camera)
        forward (quat/transformVec #js[0 0 -1] orientation (vec3/create))
        right (quat/transformVec #js[1 0 0] orientation (vec3/create))
        forward-velocity (vec3/scale forward (impulse 0) (vec3/create))
        right-velocity (vec3/scale right (impulse 1) (vec3/create))
        velocity (vec3/add forward-velocity right-velocity (vec3/create))]

    (assoc camera
           :look forward
           :velocity velocity
           :position (apply-velocity (:position camera) velocity time))))

(defn update-camera [camera time]
  (let [orientation (as-> (quat/createIdentityFloat32) q
                      (quat/rotateY q (toRadians (- (:yaw camera))) q)
                      (quat/rotateX q (toRadians (:pitch camera)) q))
        camera (move-camera camera orientation time)
        camera-matrix (mat4/makeRotationTranslation (mat4/create) orientation (:position camera))
        view-matrix (as-> (mat4/createIdentity) q
                      (do (mat4/invert camera-matrix q) q))]
    (assoc camera :view-matrix view-matrix)))