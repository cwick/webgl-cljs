(ns gltf.webgl.utils)

(defn- create-shader [gl shader-type]
  (let [gl-shader (.createShader gl shader-type)]
    (when (zero? gl-shader)
      (throw (js/Error. "Error creating shader")))
    gl-shader))

(defn- get-gl-shader [gl program shader-type]
  (if-let [shader (get-in @(:state program) [:shaders shader-type])]
    shader
    (let [shader (create-shader gl shader-type)]
      (swap! (:state program) assoc-in [:shaders shader-type] shader)
      shader)))

(defn- create-gl-program [gl]
  (let [program (.createProgram gl)]
    (when (zero? program)
      (throw (js/Error. "Error creating program")))
    program))

(defn- validate-shader [gl shader]
  (let [success (.getShaderParameter gl shader (.-COMPILE_STATUS gl))]
    (when-not success
      (let [message (.getShaderInfoLog gl shader)]
        (throw (js/Error. (str "Error while compiling shader: " message)))))))

(defn- validate-program [gl program]
  (.validateProgram gl program)

  (let [success (.getProgramParameter gl program (.-LINK_STATUS gl))]
    (when-not success
      (let [message (.getProgramInfoLog gl program)]
        (throw (js/Error. (str "Error while linking program: " message)))))))

(defn- compile-shader [gl gl-shader shader-source]
  (.shaderSource gl gl-shader shader-source)
  (.compileShader gl gl-shader)
  (validate-shader gl gl-shader)
  gl-shader)

(defn- recompile-vertex-shader
  "Recompile vertex shader if source has changed due to hot reload"
  [gl program source]
  (let [gl-shader (get-gl-shader gl program (.-VERTEX_SHADER gl))]
    (if (identical? (:vertex-shader-source @(:state program)) source)
      [gl-shader false]
      (do
        (swap! (:state program) assoc :vertex-shader-source source)
        [(compile-shader gl gl-shader source) true]))))

(defn- recompile-fragment-shader
  "Recompile fragment shader if source has changed due to hot reload"
  [gl program source]
  (let [gl-shader (get-gl-shader gl program (.-FRAGMENT_SHADER gl))]
    (if (identical? (:fragment-shader-source @(:state program)) source)
      [gl-shader false]
      (do
        (swap! (:state program) assoc :fragment-shader-source source)
        [(compile-shader gl gl-shader source) true]))))

(defn recompile-program! [gl program sources]
  (let [gl-program (-> program :state deref :gl-program)
        [vertex-shader vertex-shader-dirty?] (recompile-vertex-shader gl program (:vertex sources))
        [fragment-shader fragment-shader-dirty?] (recompile-fragment-shader gl program (:fragment sources))]
    (when (empty? (.getAttachedShaders gl gl-program))
      (.attachShader gl gl-program vertex-shader)
      (.attachShader gl gl-program fragment-shader))
    (when (or vertex-shader-dirty? fragment-shader-dirty?)
      (.linkProgram gl gl-program)
      (validate-program gl gl-program)
      (swap! (:state program) dissoc :uniforms)))
  program)

(declare get-gl-program)
(defn get-uniform-location [gl program uniform]
  (if-let [location (get-in (-> program :state deref) [:uniforms uniform])]
    location
    (let [gl-program (get-gl-program program)
          location (.getUniformLocation gl gl-program uniform)]
      (when-not location
        (throw (js/Error. (str "Invalid uniform " uniform))))
      (swap! (:state program) assoc-in [:uniforms uniform] location)
      location)))

(defn create-program [gl]
  {:state (atom {:gl-program (create-gl-program gl)})})

(defn get-gl-program [program] (-> program :state deref :gl-program))
(defn use-program [gl program] (.useProgram gl (get-gl-program program)))
