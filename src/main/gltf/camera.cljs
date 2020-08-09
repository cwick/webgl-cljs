(ns gltf.camera (:require ["gl-matrix/vec3" :as vec3]
                          ["gl-matrix/mat4" :as mat4]
                          ["gl-matrix/quat" :as quat]))

(defn- apply-velocity [pos velocity time]
  (let [[x y z] pos]
    #js[(+ x (* (aget velocity 0) time))
        (+ y (* (aget velocity 1) time))
        (+ z (* (aget velocity 2) time))]))

(defn- move-camera [position forward impulse time]
  (let [right (vec3/cross (vec3/create) forward #js[0 1 0])
        forward-velocity (vec3/scale (vec3/create) forward (impulse 0))
        velocity (vec3/scaleAndAdd (vec3/create) forward-velocity right (impulse 1))]
    (vec3/normalize velocity velocity)
    [(apply-velocity position velocity time) velocity]))

(defn update-camera [camera time]
  (let [orientation (quat/fromEuler (quat/create) (:pitch camera) (- (:yaw camera)) 0)
        forward (vec3/transformQuat (vec3/create) #js[0 0 -1] orientation)
        [position velocity] (move-camera (:position camera) forward (:impulse camera) time)
        camera-matrix (mat4/fromRotationTranslation (mat4/create) orientation position)
        view-matrix (mat4/invert (mat4/create) camera-matrix)]
    (-> camera
        (assoc :look forward
               :velocity velocity
               :position position
               :view-matrix view-matrix))))