(ns gltf.webgl.core (:require
                     [gltf.webgl.utils :as gl-utils]
                     [gltf.webgl.debug :as gl-debug]
                     [gltf.ui :as ui]
                     [gltf.scene :as scene]
                     [gltf.math.mat4 :as mat4]
                     [shadow.resource :as rc]))

(defonce gl-state (atom nil))
(def CANVAS-WIDTH 1920)
(def CANVAS-HEIGHT 1200)

(def identity-matrix (mat4/create-identity))

(def shader-sources
  {:vertex (rc/inline "./vertex-shader.vert")
   :fragment (rc/inline "./fragment-shader.frag")})

(defn- upload-texture-image [gl image]
  ; Images must be converted from sRGB to linear space
  ; See https://www.khronos.org/registry/OpenGL-Refpages/es3.0/html/glTexImage2D.xhtml
  ; See https://github.com/KhronosGroup/glTF/tree/master/specification/2.0#metallic-roughness-material
  (.texImage2D gl
               (.-TEXTURE_2D gl)
               0 ; mip level
               (.-SRGB8_ALPHA8 gl) ; internal format
               (:width image) ; width
               (:height image) ; height
               0 ; border. Must be 0
               (.-RGBA gl) ; format
               (.-UNSIGNED_BYTE gl) ; type
               (:data image)))

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
      (upload-texture-image gl (:source texture))
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

; TODO free individual resources, not the whole scene
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

(defn- print-debug-info []
  (ui/debug (str "Meshes: " (-> @gl-state :stats :mesh-count)
                 " Primitives: " (-> @gl-state :stats :primitive-count)
                 " Nodes: " (-> @gl-state :stats :node-count)
                 " Buffers: " (count (-> @gl-state :buffers))
                 " Textures: " (count (-> @gl-state :texture-buffers))
                 " VAO: " (count (-> @gl-state :vertex-arrays)))))

(defn- vertex-attrib-pointer [gl accessor attribute-name]
  (let [component-counts {:SCALAR 1
                          :VEC2 2
                          :VEC3 3
                          :VEC4 4
                          :MAT2 4
                          :MAT3 9
                          :MAT4 16}
        buffer-view (:bufferView accessor)
        program (gl-utils/get-gl-program (:program @gl-state))
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
  (let [program (:program @gl-state)
        transform-location (gl-utils/get-uniform-location gl program "u_transform")]
    (.uniformMatrix4fv gl transform-location false (mat4/data matrix))))

(defn- bind-scene-uniforms [gl scene]
  (let [program (:program @gl-state)
        projection (gl-utils/get-uniform-location gl program "u_projection")
        view (gl-utils/get-uniform-location gl program "u_view")
        texcoord-0 (gl-utils/get-uniform-location gl program "u_texture0")
        #_#_eyePosition (gl-utils/get-uniform-location gl program "u_eyePosition")
        camera (:camera scene)]
    (.uniform1i gl texcoord-0 0)
    (.uniformMatrix4fv gl view false (mat4/data (:view-matrix camera)))
    (.uniformMatrix4fv gl projection false (mat4/data (:projection-matrix camera)))
    #_(.uniform3fv gl eyePosition (vec3/data (:position camera)))))

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

(defn- get-default-texture
  "Generate a 1x1 white texture, used when material doesn't have a base color texture"
  [gl]
  (if-let [gl-texture (:default-texture @gl-state)]
    gl-texture
    (let [gl-texture (.createTexture gl)]
      (.bindTexture gl (.-TEXTURE_2D gl) gl-texture)
      (swap! gl-state assoc :default-texture gl-texture)
      (upload-texture-image gl {:width 1 :height 1 :data (js/Uint8Array.from #js[255 255 255 255])})
      gl-texture)))

(defn- bind-textures [gl primitive]
  (let [texture (get-in primitive [:material :pbrMetallicRoughness :baseColorTexture])
        gl-texture (if texture (get-gl-texture gl texture) (get-default-texture gl))]
    (.activeTexture gl (.-TEXTURE0 gl))
    (.bindTexture gl (.-TEXTURE_2D gl) gl-texture)))

(defn- bind-material [gl primitive]
  (bind-textures gl primitive)
  (let [program (:program @gl-state)
        default-base-color #js[1 1 1 1]
        base-color (get-in primitive
                           [:material :pbrMetallicRoughness :baseColorFactor]
                           default-base-color)
        base-color-location (gl-utils/get-uniform-location gl program "u_baseColor")]
    (.uniform4fv gl base-color-location base-color)))

(defn- draw-primitive [gl primitive]
  (swap! gl-state update-in [:stats :primitive-count] inc)
  (bind-vertex-array gl primitive)
  (bind-material gl primitive)
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

(defn- draw-node [gl scene node]
  (swap! gl-state update-in [:stats :node-count] inc)
  (when-let [mesh (:mesh node)]
    (set-transform-matrix! gl (:global-transform node))
    (draw-mesh gl mesh))
  (doseq [child (scene/children scene node)]
    (draw-node gl scene child)))

(defn- create-framebuffer [gl]
  (let [framebuffer (.createFramebuffer gl)
        color-buffer (.createRenderbuffer gl)
        packed-depth-buffer (.createRenderbuffer gl)
        depth-buffer (.createRenderbuffer gl)]
    (.bindFramebuffer gl (.-FRAMEBUFFER gl) framebuffer)
    ; Attach depth buffer
    (.bindRenderbuffer gl (.-RENDERBUFFER gl) depth-buffer)
    (.renderbufferStorage gl (.-RENDERBUFFER gl) (.-DEPTH_COMPONENT24 gl) CANVAS-WIDTH CANVAS-HEIGHT)
    (.framebufferRenderbuffer gl (.-FRAMEBUFFER gl) (.-DEPTH_ATTACHMENT gl) (.-RENDERBUFFER gl) depth-buffer)
    ; Attach color buffer
    (.bindRenderbuffer gl (.-RENDERBUFFER gl) color-buffer)
    (.renderbufferStorage gl (.-RENDERBUFFER gl) (.-RGBA8 gl) CANVAS-WIDTH CANVAS-HEIGHT)
    (.framebufferRenderbuffer gl (.-FRAMEBUFFER gl) (.-COLOR_ATTACHMENT0 gl) (.-RENDERBUFFER gl) color-buffer)
    ; Attach second color buffer (for reading back depth values)
    (.bindRenderbuffer gl (.-RENDERBUFFER gl) packed-depth-buffer)
    (.renderbufferStorage gl (.-RENDERBUFFER gl) (.-RGB8 gl) CANVAS-WIDTH CANVAS-HEIGHT)
    (.framebufferRenderbuffer gl (.-FRAMEBUFFER gl) (.-COLOR_ATTACHMENT1 gl) (.-RENDERBUFFER gl) packed-depth-buffer)
    ; Set fragment shader output locations
    (.drawBuffers gl #js[(.-COLOR_ATTACHMENT0 gl) (.-COLOR_ATTACHMENT1 gl)])
    ; Restore default framebuffer
    (.bindFramebuffer gl (.-FRAMEBUFFER gl) nil)
    framebuffer))

(defn- unpack-rgb-to-depth [v]
  (/ (+ (* (aget v 0) 256 256) (* (aget v 1) 256) (aget v 2))
     (- (* 256 256 256) 1)))

(defn- blit-framebuffer [gl]
  ; Draw into the default canvas framebuffer
  (.bindFramebuffer gl (.-DRAW_FRAMEBUFFER gl) nil)
  ; Read from the framebuffer we rendered the scene into
  (.bindFramebuffer gl (.-READ_FRAMEBUFFER gl) (:framebuffer @gl-state))
  (.blitFramebuffer gl
                    0 0 CANVAS-WIDTH CANVAS-HEIGHT
                    0 0 CANVAS-WIDTH CANVAS-HEIGHT
                    (.-COLOR_BUFFER_BIT gl) (.-NEAREST gl))
  (let [pixels (js/Uint8Array. 4)]
    (.readBuffer gl (.-COLOR_ATTACHMENT1 gl))
    ; TODO: This is what reading back from the packed depth buffer would look like for picking
    #_(.readPixels gl 0 0 1 1 (.-RGBA gl) (.-UNSIGNED_BYTE gl) pixels 0)
    #_(ui/debug (unpack-rgb-to-depth pixels))
    (.readBuffer gl (.-COLOR_ATTACHMENT0 gl))))

(defn- clear-buffers [gl]
  ; Normal color buffer
  (.clearBufferfv gl (.-COLOR gl) 0 #js[0 0 0 1])
  ; Depth buffer encoded as RGB values for picking operations
  (.clearBufferfv gl (.-COLOR gl) 1 #js[1 1 1 1])
  ; Real depth buffer
  (.clearBufferfi gl (.-DEPTH_STENCIL gl) 0 1 1))

(defn draw [gl scene]
  (ui/draw-benchmark
   "Draw time"
   (fn []
       ; TODO free GL resources when scene nodes destroyed
     #_(when-not (identical? scene (:scene @gl-state))
         (swap! gl-state setup-new-scene gl scene))
     (swap! gl-state dissoc :stats)
     (.bindFramebuffer gl (.-FRAMEBUFFER gl) (:framebuffer @gl-state))
     (clear-buffers gl)
     (gl-utils/use-program gl (:program @gl-state))
     (bind-scene-uniforms gl scene)
     (draw-node gl scene (scene/root scene))
     (blit-framebuffer gl)))
  (print-debug-info))

(defn recompile-shaders [gl]
  (swap! gl-state update :program #(gl-utils/recompile-program! gl % shader-sources))
  (gl-debug/recompile-shaders gl))

(defn init-webgl [canvas]
  (let [gl (.getContext canvas "webgl2" #js{:antialias false})]
    (when-not gl
      (throw (js/Error. "WebGL 2.0 not available")))
    (set! (.-width canvas) CANVAS-WIDTH)
    (set! (.-height canvas) CANVAS-HEIGHT)
    (.viewport gl
               0
               0
               (-> gl .-canvas .-width)
               (-> gl .-canvas .-height))
    (.enable gl (.-DEPTH_TEST gl))
    (let [framebuffer (create-framebuffer gl)]
      (swap! gl-state assoc
             :program (gl-utils/create-program gl)
             :framebuffer framebuffer)
      (recompile-shaders gl))
    gl))

