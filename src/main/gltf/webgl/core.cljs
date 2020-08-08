(ns gltf.webgl.core (:require
                     [gltf.webgl.utils :as gl-utils]
                     ["gl-matrix/mat4" :as mat4]))

(defonce gl-state (atom nil))

(defn- init-gl-state [gl]
  (reset! gl-state {:gl gl}))

(defn- bind-gl-buffer [gl buffer-view target]
  (if-let [gl-buffer (get-in @gl-state [:buffers buffer-view])]
    (.bindBuffer gl target gl-buffer)
    (let [gl-buffer (.createBuffer gl)]
      (.bindBuffer gl target gl-buffer)
      (.bufferData gl
                   target
                   (js/Uint8Array. (-> buffer-view :buffer :data))
                   (.-STATIC_DRAW gl)
                   (:byteOffset buffer-view)
                   (:byteLength buffer-view))
      (swap! gl-state assoc-in [:buffers buffer-view] gl-buffer))))

(defn init-webgl [canvas]
  (let [gl (.getContext canvas "webgl2")]
    (init-gl-state gl)
    (when-not gl
      (throw (js/Error. "WebGL 2.0 not available")))

    (.viewport gl
               0
               0
               (-> gl .-canvas .-width)
               (-> gl .-canvas .-height))

    (.clearColor gl 0 0 0 1)
    ; Cull back facing triangles. The default winding order is counter-clockwise,
    ; meaning that the "front" face is the one where vertices are drawn in a counter-clockwise
    ; order.
    (.enable gl (.-CULL_FACE gl))
    (.enable gl (.-DEPTH_TEST gl))
    (gl-utils/recompile-shaders gl gl-state)))

(defn- bind-vertex-attribute [gl accessor attribute]
  (let [component-counts {:SCALAR 1
                          :VEC2 2
                          :VEC3 3
                          :VEC4 4
                          :MAT2 4
                          :MAT3 9
                          :MAT4 16}
        buffer-view (:bufferView accessor)]
    (.vertexAttribPointer
     gl
     attribute
     ((:type accessor) component-counts) ; number of components per vertex attribute
     (goog.object/get gl (name (:componentType accessor))) ; data type of each component
     (:normalized accessor) ; whether integer data values should be normalized into a certain range
     (:byteStride buffer-view) ; stride
     (:byteOffset accessor) ; offset into the buffer
     )))

(defn- bind-vertex-array [gl primitive]
  (if-let [gl-vao (get-in @gl-state [:vertex-arrays primitive])]
    (.bindVertexArray gl gl-vao)
    (let [gl-vao (.createVertexArray gl)
          program (gl-utils/get-gl-program gl gl-state)
          position-attr (.getAttribLocation gl program "POSITION")]
      (.bindVertexArray gl gl-vao)
      (.enableVertexAttribArray gl position-attr)
      (bind-vertex-attribute gl (-> primitive :attributes :POSITION) position-attr)
      (swap! gl-state assoc-in [:vertex-arrays primitive] gl-vao))))

(defn- bind-uniforms [gl]
  (let [program (gl-utils/get-gl-program gl gl-state)
        transform (.getUniformLocation gl program "u_transform")
        projection (.getUniformLocation gl program "u_projection")
        view (.getUniformLocation gl program "u_view")]
    (.uniformMatrix4fv gl transform false (mat4/create))
    (.uniformMatrix4fv gl view false (:view-matrix @gl-state (mat4/create)))
    (.uniformMatrix4fv gl projection false (mat4/create))
    (.uniformMatrix4fv gl projection false
                       (mat4/perspective
                        (mat4/create)
                        (* 50 (/ js/Math.PI 180))
                        1
                        0.1
                        100))))

(defn- draw-elements [gl primitive]
  (let [indices (:indices primitive)]
    (bind-gl-buffer gl (:bufferView indices) (.-ELEMENT_ARRAY_BUFFER gl))
    (.drawElements gl
                   (goog.object/get gl (name (:mode primitive)))
                   (:count indices)
                   (goog.object/get gl (name (:componentType indices)))
                   (:byteOffset indices))))

(defn- draw-primitive [gl primitive]
  (when-let [position-attribute (get-in primitive [:attributes :POSITION])]
    (bind-gl-buffer gl (:bufferView position-attribute) (.-ARRAY_BUFFER gl))
    (bind-vertex-array gl primitive)
    (if (:indices primitive)
      (draw-elements gl primitive)
      (.drawArrays gl (.-TRIANGLES gl) 0 (* 3 100)))))

(defn- draw-mesh [gl mesh]
  (bind-uniforms gl)
  (doseq [p (:primitives mesh)]
    (draw-primitive gl p)))

(defn- draw-node [gl node]
  (when-let [mesh (:mesh node)]
    (draw-mesh gl mesh))
  (doseq [child (:children node)] (draw-node gl child)))

(defn- setup-new-scene
  "Free all GL resources and set new scene"
  [old-state gl scene]

  (doseq [[_ gl-buffer] (:buffers old-state)]
    (.deleteBuffer gl gl-buffer))

  (doseq [[_ gl-vao] (:vertex-arrays old-state)]
    (.deleteVertexArray gl gl-vao))

  (-> old-state
      (dissoc :buffers)
      (dissoc :vertex-arrays)
      (assoc :scene scene)))

(defn draw [scene]
  (when-let [gl (:gl @gl-state)]
    (when-not (identical? scene (:scene @gl-state))
      (swap! gl-state setup-new-scene gl scene))
    (.clear gl (bit-or (.-DEPTH_BUFFER_BIT gl) (.-COLOR_BUFFER_BIT gl)))
    (doseq [node (:nodes scene)]
      (draw-node gl node))))

(defn recompile-shaders []
  (when-let [gl (:gl @gl-state)]
    (gl-utils/recompile-shaders gl gl-state)))

(defn set-view-matrix! [m]
  (swap! gl-state assoc :view-matrix m))