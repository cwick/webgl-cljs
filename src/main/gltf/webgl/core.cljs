(ns gltf.webgl.core (:require
                     [gltf.webgl.utils :as gl-utils]
                     [gltf.ui :as ui]
                     [goog.vec.mat4f :as mat4]))

(defonce gl-state (atom nil))

(def identity-matrix (mat4/createIdentity))

(defn- init-gl-state [gl]
  (reset! gl-state {:gl gl}))

(defn- get-gl-buffer [gl buffer-view target]
  (if-let [gl-buffer (get-in @gl-state [:buffers buffer-view])]
    gl-buffer
    (let [gl-buffer (.createBuffer gl)]
      (.bindBuffer gl target gl-buffer)
      (.bufferData gl
                   target
                   (js/Uint8Array. (-> buffer-view :buffer :data))
                   (.-STATIC_DRAW gl)
                   (:byteOffset buffer-view)
                   (:byteLength buffer-view))
      (.bindBuffer gl target nil)
      (swap! gl-state assoc-in [:buffers buffer-view] gl-buffer)
      gl-buffer)))

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

(defn- bind-vertex-attribute [gl accessor attribute-name]
  (let [component-counts {:SCALAR 1
                          :VEC2 2
                          :VEC3 3
                          :VEC4 4
                          :MAT2 4
                          :MAT3 9
                          :MAT4 16}
        buffer-view (:bufferView accessor)
        program (gl-utils/get-gl-program gl gl-state)
        attribute-location (.getAttribLocation gl program attribute-name)]
    (.enableVertexAttribArray gl attribute-location)
    (.vertexAttribPointer
     gl
     attribute-location
     ((:type accessor) component-counts) ; number of components per vertex attribute
     (goog.object/get gl (name (:componentType accessor))) ; data type of each component
     (:normalized accessor) ; whether integer data values should be normalized into a certain range
     (:byteStride buffer-view) ; stride
     (:byteOffset accessor) ; offset into the buffer
     )))

(defn- set-transform-matrix! [gl matrix]
  (let [transform-location (gl-utils/get-uniform-location gl gl-state "u_transform")]
    (.uniformMatrix4fv gl transform-location false matrix)))

(defn- bind-uniforms [gl]
  (let [projection (gl-utils/get-uniform-location gl gl-state "u_projection")
        view (gl-utils/get-uniform-location gl gl-state "u_view")]
    (.uniformMatrix4fv gl view false (:view-matrix @gl-state identity-matrix))
    (.uniformMatrix4fv gl projection false
                       (mat4/makePerspective
                        (mat4/create)
                        (* 50 (/ js/Math.PI 180))
                        1
                        0.1
                        10000))))

(defn- draw-elements [gl primitive]
  (let [indices (:indices primitive)]
    (.drawElements gl
                   (goog.object/get gl (name (:mode primitive)))
                   (:count indices)
                   (goog.object/get gl (name (:componentType indices)))
                   (:byteOffset indices))))

(defn- bind-vertex-array [gl primitive]
  (if-let [gl-vao (get-in @gl-state [:vertex-arrays primitive])]
    (.bindVertexArray gl gl-vao)
    (let [gl-vao (.createVertexArray gl)
          position-accessor (-> primitive :attributes :POSITION)
          vertex-buffer (get-gl-buffer gl (:bufferView position-accessor) (.-ARRAY_BUFFER gl))]
      (.bindVertexArray gl gl-vao)
      (.bindBuffer gl (.-ARRAY_BUFFER gl) vertex-buffer)
      (when-let [indices (:indices primitive)]
        (let [element-buffer (get-gl-buffer gl (:bufferView indices) (.-ELEMENT_ARRAY_BUFFER gl))]
          (.bindBuffer gl (.-ELEMENT_ARRAY_BUFFER gl) element-buffer)))
      (bind-vertex-attribute gl position-accessor "POSITION")
      (swap! gl-state assoc-in [:vertex-arrays primitive] gl-vao))))

(defn- draw-primitive [gl primitive]
  (swap! gl-state update-in [:stats :primitive-count] inc)
  (bind-vertex-array gl primitive)
  (if (:indices primitive)
    (draw-elements gl primitive)
    (.drawArrays gl
                 (goog.object/get gl (name (:mode primitive)))
                 0
                 (-> primitive :attributes :POSITION :count))))

(defn- draw-mesh [gl mesh]
  (swap! gl-state update-in [:stats :mesh-count] inc)
  (doseq [p (:primitives mesh)]
    (draw-primitive gl p)))

(defn- draw-node
  ([gl node]
   (draw-node gl node identity-matrix))

  ([gl node parent-transform]
   (let [local-transform (or (:matrix node) identity-matrix)
         global-transform (mat4/multMat parent-transform local-transform (mat4/create))]
     (swap! gl-state update-in [:stats :node-count] inc)
     (when-let [mesh (:mesh node)]
       (set-transform-matrix! gl global-transform)
       (draw-mesh gl mesh))
     (doseq [child (:children node)] (draw-node gl child global-transform)))))

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
    (let [start (js/performance.now)]
      (when-not (identical? scene (:scene @gl-state))
        (swap! gl-state setup-new-scene gl scene))
      (swap! gl-state dissoc :stats)
      (.clear gl (bit-or (.-DEPTH_BUFFER_BIT gl) (.-COLOR_BUFFER_BIT gl)))
      (bind-uniforms gl)
      (doseq [node (:nodes scene)]
        (draw-node gl node))

      (ui/draw-text
       (str "Meshes: " (-> @gl-state :stats :mesh-count)
            " Primitives: " (-> @gl-state :stats :primitive-count)
            " Nodes: " (-> @gl-state :stats :node-count)
            " Buffers: " (count (-> @gl-state :buffers))
            " VAO: " (count (-> @gl-state :vertex-arrays))) 80)

      (ui/draw-text (str "Draw time: " (.toFixed (- (js/performance.now) start) 2) "ms") 100))))

(defn recompile-shaders []
  (when-let [gl (:gl @gl-state)]
    (gl-utils/recompile-shaders gl gl-state)))

(defn set-view-matrix! [m]
  (swap! gl-state assoc :view-matrix m))