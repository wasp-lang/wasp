import { getModuleContent, useQuery } from "wasp/client/operations";

export function MainPage() {
  const { data: content, isLoading, error } = useQuery(getModuleContent);

  return (
    <main style={{ padding: "3rem", fontFamily: "system-ui, sans-serif" }}>
      <p style={{ margin: 0, color: "#666", textTransform: "uppercase" }}>
        Wasp module
      </p>
      <h1 style={{ margin: "0.5rem 0" }}>Hello from a Wasp module</h1>
      <p style={{ maxWidth: "36rem", lineHeight: 1.6 }}>
        This page is exported from a reusable module package.
      </p>
      <section
        style={{
          marginTop: "2rem",
          padding: "1rem",
          border: "1px solid #ddd",
          borderRadius: "0.75rem",
          maxWidth: "36rem",
        }}
      >
        <h2 style={{ margin: "0 0 0.5rem" }}>Server content</h2>
        {isLoading && <p>Loading content from the server...</p>}
        {error && <p>Failed to load server content: {error.message}</p>}
        {content && (
          <>
            <p style={{ margin: "0 0 0.5rem" }}>{content.message}</p>
            <small>Served at {content.servedAt}</small>
          </>
        )}
      </section>
    </main>
  );
}
