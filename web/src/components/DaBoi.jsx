import React, { Suspense, useRef, useEffect } from "react";
import { Canvas, useLoader, useThree } from "@react-three/fiber";
import { OrbitControls, Environment, useTexture } from "@react-three/drei";
import { FBXLoader } from "three/examples/jsm/loaders/FBXLoader";
import { Box3, Vector3 } from "three";

function Model() {
  const fbx = useLoader(FBXLoader, "/boi/Models/bee_low.fbx");
  const [baseColorMap, heightMap, metallicMap, normalMap, roughnessMap] =
    useTexture([
      "/boi/Textures/bee_low_body_low_BaseColor.png",
      "/boi/Textures/bee_low_body_low_Height.png",
      "/boi/Textures/bee_low_body_low_Metallic.png",
      "/boi/Textures/bee_low_body_low_Normal.png",
      "/boi/Textures/bee_low_body_low_Roughness.png",
    ]);

  fbx.scale.set(0.6, 0.6, 0.6);
  fbx.position.y = 1;
  fbx.position.x = 0;
  fbx.rotation.z = -0.1;

  fbx.traverse((o) => {
    if (o.isMesh) {
      o.material.map = baseColorMap;
      o.material.displacementMap = heightMap;
      o.material.metalnessMap = metallicMap;
      o.material.normalMap = normalMap;
      o.material.roughnessMap = roughnessMap;
      o.castShadow = true;
      o.receiveShadow = true;
    }
  });

  return (
    <>
      <primitive object={fbx} />
      {fbx && <Controls model={fbx} />}
    </>
  );
}

function Lights() {
  return (
    <>
      <directionalLight position={[8, 8, 2]} intensity={2} castShadow />
      <hemisphereLight
        color={"#ffffff"}
        groundColor={"#b3858c"}
        intensity={2}
      />
      <spotLight
        color={0xffffff}
        intensity={100}
        position={[2.5, 5, 2.5]}
        angle={Math.PI / 6}
        penumbra={1}
        decay={2}
        distance={0}
        castShadow
        shadow-mapSize-width={1024}
        shadow-mapSize-height={1024}
        shadow-camera-near={1}
        shadow-camera-far={10}
        shadow-focus={1}
      />
    </>
  );
}

function Controls({ model }) {
  const { camera, gl } = useThree();

  const controls = useRef();

  useEffect(() => {
    if (!controls.current) {
      return;
    }
    if (!model) {
      return;
    }
    const box = new Box3().setFromObject(model);
    const center = box.getCenter(new Vector3());
    controls.current.target.copy(center);
  }, [model]);

  return (
    <OrbitControls
      ref={controls}
      args={[camera, gl.domElement]}
      enablePan={false}
      enableZoom={false}
      enableDamping={true}
      dampingFactor={0.1}
      autoRotate={true}
      autoRotateSpeed={2}
      minPolarAngle={Math.PI / 2}
      maxPolarAngle={Math.PI / 2}
      minAzimuthAngle={-Math.PI / 2}
    />
  );
}

export default function DaBoi({ height = 200, width = 200 }) {
  return (
    <Canvas
      style={{
        height,
        width,
        background: "transparent",
      }}
      camera={{ position: [0, 0, 245], fov: 50, near: 0.1, far: 1000 }}
    >
      <Suspense fallback={null}>
        <Model />
        <Environment
          files={[
            "/boi/pisa/px.png",
            "/boi/pisa/nx.png",
            "/boi/pisa/py.png",
            "/boi/pisa/ny.png",
            "/boi/pisa/pz.png",
            "/boi/pisa/nz.png",
          ]}
        />
      </Suspense>
      <Lights />
    </Canvas>
  );
}
