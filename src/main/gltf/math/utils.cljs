(ns gltf.math.utils)

(defn clamp [x low high]
  (max low (min high x)))

(def round js/Math.round)