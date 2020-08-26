(ns gltf.math.mat4 (:require
                    [goog.vec.mat4f :as gmat4]
                    [goog.vec.vec3f :as gvec3]
                    [gltf.math.vec3 :as vec3]))

(deftype Mat4 [data]
  IPrintWithWriter
  (-pr-writer [o writer _]
    (-write writer (str "["
                        (aget data 0) " "
                        (aget data 1) " "
                        (aget data 2) " "
                        (aget data 3) " "
                        (aget data 4) " "
                        (aget data 5) " "
                        (aget data 6) " "
                        (aget data 7) " "
                        (aget data 8) " "
                        (aget data 9) " "
                        (aget data 10) " "
                        (aget data 11) " "
                        (aget data 12) " "
                        (aget data 13) " "
                        (aget data 14) " "
                        (aget data 15) " "
                        (aget data 16) "]"))))

(defn create
  ([] (Mat4. (gmat4/create)))
  ([v00 v10 v20 v30
    v01 v11 v21 v31
    v02 v12 v22 v32
    v03 v13 v23 v33]
   (Mat4.
    (-> (gmat4/create)
        (gmat4/setFromValues
         v00 v10 v20 v30
         v01 v11 v21 v31
         v02 v12 v22 v32
         v03 v13 v23 v33)))))

(defn create-identity []
  (Mat4. (gmat4/createIdentity)))

(defn create-rotation-translation-scale [r t s]
  (Mat4.
   (gmat4/makeRotationTranslationScale
    (gmat4/create) r (.-data t) (.-data s))))

(defn create-perspective [fov-y aspect near far]
  (Mat4.
   (gmat4/makePerspective
    (gmat4/create) fov-y aspect near far)))

(defn get-column [m i]
  (->> (gvec3/create)
       (gmat4/getColumn (.-data m) i)
       (vec3/Vec3.)))

(defn transpose [m]
  (Mat4. (gmat4/transpose (.-data m) (gmat4/create))))

(defn mult-mat [m1 m2]
  (Mat4. (gmat4/multMat (.-data m1) (.-data m2) (gmat4/create))))

(defn rotate-x! [m angle]
  (gmat4/rotateX (.-data m) angle)
  m)

(defn rotate-y! [m angle]
  (gmat4/rotateY (.-data m) angle)
  m)

(defn translate! [m x y z]
  (gmat4/translate (.-data m) x y z)
  m)

(defn scale! [m x y z]
  (gmat4/scale (.-data m) x y z)
  m)

(defn data [m] (.-data m))