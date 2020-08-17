(ns gltf.webgl.core (:require
                     [gltf.webgl.utils :as gl-utils]
                     [gltf.ui :as ui]
                     [goog.vec.mat4f :as mat4]))

(defonce gl-state (atom nil))

(def identity-matrix (mat4/createIdentity))

(defn- init-gl-state [gl]
  (reset! gl-state {:gl gl}))

(defn- get-gl-texture [gl texture]
  (if-let [gl-texture (get-in @gl-state [:texture-buffers texture])]
    gl-texture
    (let [gl-texture (.createTexture gl)
          sampler (:sampler texture)]
      (.bindTexture gl (.-TEXTURE_2D gl) gl-texture)
      (.texParameteri gl (.-TEXTURE_2D gl) (.-TEXTURE_WRAP_S gl) (or (:wrapS sampler) (.-REPEAT gl)))
      (.texParameteri gl (.-TEXTURE_2D gl) (.-TEXTURE_WRAP_T gl) (or (:wrapT sampler) (.-REPEAT gl)))
      (.texParameteri gl (.-TEXTURE_2D gl) (.-TEXTURE_MIN_FILTER gl) (or (:minFilter sampler) (.-LINEAR gl)))
      (.texParameteri gl (.-TEXTURE_2D gl) (.-TEXTURE_MAG_FILTER gl) (or (:magFilter sampler) (.-LINEAR gl)))
      ; See https://github.com/KhronosGroup/glTF/tree/master/specification/2.0#images
      (.pixelStorei gl (.-UNPACK_COLORSPACE_CONVERSION_WEBGL gl) (.-NONE gl))
      (swap! gl-state assoc-in [:texture-buffers texture] gl-texture)
      ; Images must be converted from sRGB to linear space
      ; See https://www.khronos.org/registry/OpenGL-Refpages/es3.0/html/glTexImage2D.xhtml
      ; See https://github.com/KhronosGroup/glTF/tree/master/specification/2.0#metallic-roughness-material
      (.texImage2D gl
                   (.-TEXTURE_2D gl)
                   0 ; mip level
                   (.-SRGB8_ALPHA8 gl) ; internal format
                   (.-RGBA gl) ; format
                   (.-UNSIGNED_BYTE gl) ; type
                   (-> texture :source :data))
      (when (contains? #{(.-LINEAR_MIPMAP_NEAREST gl)
                         (.-NEAREST_MIPMAP_LINEAR gl)
                         (.-NEAREST_MIPMAP_NEAREST gl)
                         (.-LINEAR_MIPMAP_LINEAR gl)}
                       (:minFilter sampler))
        (.generateMipmap gl (.-TEXTURE_2D gl)))
      gl-texture)))

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

(defn- setup-new-scene
  "Free all GL resources and set new scene"
  [old-state gl scene]

  (doseq [[_ gl-buffer] (:buffers old-state)]
    (.deleteBuffer gl gl-buffer))

  (doseq [[_ gl-texture] (:texture-buffers old-state)]
    (.deleteTexture gl gl-texture))

  (doseq [[_ gl-vao] (:vertex-arrays old-state)]
    (.deleteVertexArray gl gl-vao))

  (-> old-state
      (dissoc :buffers)
      (dissoc :texture-buffers)
      (dissoc :vertex-arrays)
      (assoc :scene scene)))

(defn- print-debug-info [start-time]
  (ui/debug
   (str "Meshes: " (-> @gl-state :stats :mesh-count)
        " Primitives: " (-> @gl-state :stats :primitive-count)
        " Nodes: " (-> @gl-state :stats :node-count)
        " Buffers: " (count (-> @gl-state :buffers))
        " Textures: " (count (-> @gl-state :texture-buffers))
        " VAO: " (count (-> @gl-state :vertex-arrays))))
  (ui/debug (str "Draw time: " (.toFixed (- (js/performance.now) start-time) 2) "ms")))

(defn- vertex-attrib-pointer [gl accessor attribute-name]
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

(defn- bind-vertex-attribute [gl primitive attribute]
  (if-let [accessor (get-in primitive [:attributes attribute])]
    (let [vertex-buffer (get-gl-buffer gl (:bufferView accessor) (.-ARRAY_BUFFER gl))]
      (.bindBuffer gl (.-ARRAY_BUFFER gl) vertex-buffer)
      (vertex-attrib-pointer gl accessor (name attribute)))))

(defn- set-transform-matrix! [gl matrix]
  (let [transform-location (gl-utils/get-uniform-location gl gl-state "u_transform")]
    (.uniformMatrix4fv gl transform-location false matrix)))

(defn- bind-uniforms [gl]
  (let [projection (gl-utils/get-uniform-location gl gl-state "u_projection")
        view (gl-utils/get-uniform-location gl gl-state "u_view")
        texcoord-0 (gl-utils/get-uniform-location gl gl-state "u_texture0")]
    (.uniform1i gl texcoord-0 0)
    (.uniformMatrix4fv gl view false (:view-matrix @gl-state identity-matrix))
    (.uniformMatrix4fv gl projection false (:projection-matrix @gl-state identity-matrix))))

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
    (let [gl-vao (.createVertexArray gl)]
      (.bindVertexArray gl gl-vao)
      (when-let [indices (:indices primitive)]
        (let [element-buffer (get-gl-buffer gl (:bufferView indices) (.-ELEMENT_ARRAY_BUFFER gl))]
          (.bindBuffer gl (.-ELEMENT_ARRAY_BUFFER gl) element-buffer)))
      (bind-vertex-attribute gl primitive :POSITION)
      (bind-vertex-attribute gl primitive :TEXCOORD_0)
      (swap! gl-state assoc-in [:vertex-arrays primitive] gl-vao))))

(defn- bind-textures [gl primitive]
  (when-let [texture (get-in primitive [:material :pbrMetallicRoughness :baseColorTexture])]
    (let [gl-texture (get-gl-texture gl texture)]
      (.activeTexture gl (.-TEXTURE0 gl))
      (.bindTexture gl (.-TEXTURE_2D gl) gl-texture))))

(defn- draw-primitive [gl primitive]
  (swap! gl-state update-in [:stats :primitive-count] inc)
  (bind-vertex-array gl primitive)
  (bind-textures gl primitive)

  (if (-> primitive :material :doubleSided)
    (.disable gl (.-CULL_FACE gl))
    (.enable gl (.-CULL_FACE gl)))

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

(defn draw [scene]
  (when-let [gl (:gl @gl-state)]
    (let [start-time (js/performance.now)]
      (when-not (identical? scene (:scene @gl-state))
        (swap! gl-state setup-new-scene gl scene))
      (swap! gl-state dissoc :stats)
      (.clear gl (bit-or (.-DEPTH_BUFFER_BIT gl) (.-COLOR_BUFFER_BIT gl)))
      (bind-uniforms gl)
      (doseq [node (:nodes scene)]
        (draw-node gl node))
      (print-debug-info start-time))))

(defn recompile-shaders []
  (when-let [gl (:gl @gl-state)]
    (gl-utils/recompile-shaders gl gl-state)))

(defn set-view-matrix! [m]
  (swap! gl-state assoc :view-matrix m))

(defn set-projection-matrix! [m]
  (swap! gl-state assoc :projection-matrix m))

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
    (.enable gl (.-DEPTH_TEST gl))
    (gl-utils/recompile-shaders gl gl-state)))

