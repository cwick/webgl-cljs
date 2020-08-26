(ns gltf.math.vec3
  (:refer-clojure :exclude [zero?])
  (:require
   [goog.vec.vec3f :as gvec3]))

(deftype Vec3 [data]
  Object
  (toString [_] (str "["
                     (aget data 0) " "
                     (aget data 1) " "
                     (aget data 2) "]"))

  IPrintWithWriter
  (-pr-writer [this writer _]
    (-write writer (.toString this)))

  IIndexed
  (-nth [_ n] (aget data n))
  (-nth [_ n not-found] (or (aget data n) not-found))

  ISeqable
  (-seq [_] (IndexedSeq. data 0 nil)))

(defn create
  ([] (Vec3. (gvec3/create)))
  ([x y z] (Vec3. (gvec3/createFromValues x y z))))

(defn add
  ([v1 v2]
   (Vec3.
    (gvec3/add (.-data v1) (.-data v2) (gvec3/create))))
  ([v1 v2 v3]
   (Vec3. (as-> (gvec3/create) temp
            (gvec3/add (.-data v1) (.-data v2) temp)
            (gvec3/add (.-data v3) temp temp)))))

(defn negate [v]
  (Vec3.
   (gvec3/negate (.-data v) (gvec3/create))))

(defn scale [v scalar]
  (Vec3.
   (gvec3/scale (.-data v) scalar (gvec3/create))))

(defn scale-and-add [v1 v2 scalar]
  (Vec3. (as-> (gvec3/create) temp
           (gvec3/scale (.-data v2) scalar temp)
           (gvec3/add (.-data v1) temp temp))))

(declare zero?)
(declare zero)

(defn normalize [v]
  (if (zero? v) zero
      (Vec3.
       (gvec3/normalize (.-data v) (gvec3/create)))))

(defn magnitude-squared [v]
  (gvec3/magnitudeSquared (.-data v)))

(defn magnitude [v]
  (gvec3/magnitude (.-data v)))

(defn zero? [v]
  (let [data (.-data v)]
    (== (aget data 0)
        (aget data 1)
        (aget data 2)
        0)))

(def zero (create))
(def world-up (create 0 1 0))