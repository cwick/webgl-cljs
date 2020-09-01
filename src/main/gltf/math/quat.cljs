(ns gltf.math.quat (:require
                    [goog.vec.Quaternion :as gquat]))

(deftype Quat [data]
  Object
  (toString [_] (str "["
                     (aget data 0) " "
                     (aget data 1) " "
                     (aget data 2) " "
                     (aget data 3) "]"))

  IPrintWithWriter
  (-pr-writer [o writer _]
    (-write writer (str "["
                        (aget data 0) " "
                        (aget data 1) " "
                        (aget data 2) " "
                        (aget data 3) " ")))

  IIndexed
  (-nth [_ n] (aget data n))
  (-nth [_ n not-found] (or (aget data n) not-found))

  ISeqable
  (-seq [_] (IndexedSeq. data 0 nil))

  ICloneable
  (-clone [_] (Quat. (gquat/cloneFloat32 data))))

(defn create
  ([v0 v1 v2 v3]
   (Quat.
    (gquat/createFloat32FromValues v0 v1 v2 v3))))

(defn create-identity []
  (Quat. (gquat/createIdentityFloat32)))