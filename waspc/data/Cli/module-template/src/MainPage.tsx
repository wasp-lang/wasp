export function MainPage() {
  return (
    <main style={{ padding: "3rem", fontFamily: "system-ui, sans-serif" }}>
      <p style={{ margin: 0, color: "#666", textTransform: "uppercase" }}>
        Wasp module
      </p>
      <h1 style={{ margin: "0.5rem 0" }}>Hello from a Wasp module</h1>
      <p style={{ maxWidth: "36rem", lineHeight: 1.6 }}>
        This page is exported from a reusable module package.
      </p>
    </main>
  );
}
