(ns gltf.webgl.debug (:require
                      [gltf.webgl.utils :as gl-utils]
                      [shadow.resource :as rc]
                      [gltf.math.mat4 :as mat4]))

(defonce debug-state (atom nil))
(def shader-sources
  {:vertex (rc/inline "./vertex-shader.vert")
   :fragment (rc/inline "./debug-fragment-shader.frag")})

(defn- get-debug-buffer [gl]
  (if-let [gl-buffer (:debug-buffer @debug-state)]
    gl-buffer
    (let [gl-buffer (.createBuffer gl)]
      (swap! debug-state assoc :debug-buffer gl-buffer)
      gl-buffer)))

(defn- initialize [gl]
  (when-not (:program @debug-state)
    (swap! debug-state assoc :program (gl-utils/create-program gl))))

(defn recompile-shaders [gl]
  (initialize gl)
  (swap! debug-state update :program #(gl-utils/recompile-program! gl % shader-sources)))

(def identity-matrix (mat4/create-identity))

(defn set-camera [gl camera]
  (let [program (:program @debug-state)
        projection (gl-utils/get-uniform-location gl program "u_projection")
        view (gl-utils/get-uniform-location gl program "u_view")
        transform-location (gl-utils/get-uniform-location gl program "u_transform")]
    (gl-utils/use-program gl (:program @debug-state))
    (.uniformMatrix4fv gl view false (mat4/data (:view-matrix camera)))
    (.uniformMatrix4fv gl projection false (mat4/data (:projection-matrix camera)))
    (.uniformMatrix4fv gl transform-location false (mat4/data identity-matrix))))

(defn point [gl x y z color]
  (.disable gl (.-DEPTH_TEST gl))
  (.bindVertexArray gl nil)
  (gl-utils/use-program gl (:program @debug-state))
  (let [gl-buffer (get-debug-buffer gl)
        data (.-buffer (js/Float32Array.from #js[x y z]))
        position-attribute (.getAttribLocation
                            gl
                            (gl-utils/get-gl-program (:program @debug-state))
                            "POSITION")]
    (.bindBuffer gl (.-ARRAY_BUFFER gl) gl-buffer)
    (.bufferData gl
                 (.-ARRAY_BUFFER gl)
                 (js/Uint8Array. data)
                 (.-DYNAMIC_DRAW gl)
                 0
                 (.-length data))
    (.enableVertexAttribArray gl position-attribute)
    (.vertexAttribPointer
     gl
     position-attribute
     3 ; number of components per vertex attribute
     (.-FLOAT gl) ; data type of each component
     false ; whether integer data values should be normalized into a certain range
     0 ; stride
     0 ; offset into the buffer
     )
    (.drawArrays gl (.-POINTS gl) 0 1))
  (.enable gl (.-DEPTH_TEST gl)))