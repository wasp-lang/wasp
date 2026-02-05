import { FeatureContainer } from "../components/FeatureContainer";

export const head = () => ({
  title: "Home",
  meta: [
    { name: "description", content: "Landing page" },
    { property: "og:title", content: "Home" },
  ],
  link: [
    { rel: "canonical", href: "https://example.com" },
  ],
});

export function HomePage() {
  return (
    <FeatureContainer>
      <div className="card">
        Welcome to the Wasp kitchen sink app. Explore the features by clicking
        on the links in the sidebar.
      </div>
    </FeatureContainer>
  );
}
