(ns gltf.webgl.utils (:require [shadow.resource :as rc]))

(def vertex-shader-source-inline (rc/inline "./vertex-shader.vert"))
(def fragment-shader-source-inline (rc/inline "./fragment-shader.frag"))

(defn- create-shader [gl shader-type]
  (let [gl-shader (.createShader gl shader-type)]
    (when (zero? gl-shader)
      (throw (js/Error. "Error creating shader")))

    gl-shader))

(defn- get-gl-shader [gl gl-state shader-type]
  (if-let [shader (get-in @gl-state [:shaders shader-type])]
    shader
    (let [shader (create-shader gl shader-type)]
      (swap! gl-state assoc-in [:shaders shader-type] shader)
      shader)))

(defn- create-program [gl]
  (let [program (.createProgram gl)]
    (when (zero? program)
      (throw (js/Error. "Error creating program")))

    program))

(defn- get-gl-program [gl gl-state]
  (if-let [program (:program @gl-state)]
    program
    (let [program (create-program gl)]
      (swap! gl-state assoc :program program)
      program)))

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

(defn- compile-shader [gl gl-state shader-type shader-source]
  (let [gl-shader (get-gl-shader gl gl-state shader-type)]
    (.shaderSource gl gl-shader shader-source)
    (.compileShader gl gl-shader)
    (validate-shader gl gl-shader)
    gl-shader))

(defn- compile-vertex-shader [gl gl-state]
  (compile-shader gl gl-state (.-VERTEX_SHADER gl) (:vertex-shader-source @gl-state)))

(defn- compile-fragment-shader [gl gl-state]
  (compile-shader gl gl-state (.-FRAGMENT_SHADER gl) (:fragment-shader-source @gl-state)))

(defn- recompile-vertex-shader
  "Recompile vertex shader if source has changed due to hot reload"
  [gl gl-state]
  (if (identical? (:vertex-shader-source @gl-state) vertex-shader-source-inline)
    [(get-gl-shader gl gl-state (.-VERTEX_SHADER gl)) false]
    (do
      (swap! gl-state assoc :vertex-shader-source vertex-shader-source-inline)
      [(compile-vertex-shader gl gl-state) true])))

(defn- recompile-fragment-shader
  "Recompile fragment shader if source has changed due to hot reload"
  [gl gl-state]
  (if (identical? (:fragment-shader-source @gl-state) fragment-shader-source-inline)
    [(get-gl-shader gl gl-state (.-FRAGMENT_SHADER gl)) false]
    (do
      (swap! gl-state assoc :fragment-shader-source fragment-shader-source-inline)
      [(compile-fragment-shader gl gl-state) true])))

(defn recompile-shaders [gl gl-state]
  (let [program (get-gl-program gl gl-state)
        [vertex-shader vertex-shader-dirty?] (recompile-vertex-shader gl gl-state)
        [fragment-shader fragment-shader-dirty?] (recompile-fragment-shader gl gl-state)]
    (when (empty? (.getAttachedShaders gl program))
      (.attachShader gl program vertex-shader)
      (.attachShader gl program fragment-shader))
    (when (or vertex-shader-dirty? fragment-shader-dirty?)
      (.linkProgram gl program)
      (validate-program gl program))
    (.useProgram gl program)))
