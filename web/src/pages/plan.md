# Landing Page Redesign Plan (Stripe.dev Inspired)

**Goal:** Redesign the landing page (`web/src/pages/index.js`) to adopt layout and structural elements inspired by `stripe.dev`, while preserving the existing color scheme, typography, and unique stylistic elements (e.g., background lights).

**Chosen Strategy:** Component-by-Component Refactoring using LLM assistance.

**Steps:**

1.  (Skip this, as we are on a new git branch already) -- **Backup:** Create a backup copy of `web/src/pages/index.js` and any related CSS files (e.g., `web/src/pages/styles.module.css`, `web/src/pages/index.css`).
2.  **Analyze `stripe.dev`:** Browse `stripe.dev` and take screenshots or notes on key layout patterns for typical sections (Hero, Features, How It Works, Testimonials, CTA/Newsletter, Footer). Pay attention to spacing, alignment, use of cards, grids, visual hierarchy, typography, and responsive behavior.
  - Analysis: 
    - **Developer-Centric Aesthetic:** `stripe.dev` has a distinct look geared towards developers. Clean, functional, with subtle nods to coding culture.
    - **Layout Patterns:**
        - Clear Hero Section: Bold headline, concise text, prominent CTAs.
        - Distinct Sections: Clear visual separation using spacing, background variations.
        - Visuals + Text: Often uses side-by-side layouts or incorporates diagrams/code snippets effectively.
        - Card Layouts: Used for organizing lists of features, examples, etc.
        - Spacious & Clean: Ample whitespace, avoids clutter.
        - Structured Footer: Organized links.
    - **Specific Elements:**
        - **Navbar:** Simple, minimal navigation. Includes keyboard shortcut hints (e.g., `[B] Blog`, `[D] Docs`) which adds to the developer feel.
        - **Hero Headline:** Uses mixed typography, notably featuring "DEV" in a distinct, pixelated font style.
        - **Floating Terminal:** Incorporates a stylized, non-functional floating terminal window showcasing code or commands, enhancing the developer theme.
3.  **Iterate Through Components:** Address each component imported in `index.js` one by one. For each component:
    *   Identify the corresponding component file (e.g., `web/src/components/Hero/index.js`).
    *   Analyze the target layout from the `stripe.dev` analysis (Step 2).
    *   Use LLM assistance to refactor the component's JSX structure and apply/update Tailwind classes to match the Stripe layout.
    *   **Crucially:** Instruct the LLM to preserve existing text content, props, functionality, color scheme (using existing CSS variables or Tailwind theme colors), and any specific stylistic elements (like background lights).
    *   **Components to Refactor (in approximate order):**
        *   `Hero`: Refactor layout to match `stripe.dev`'s hero structure. Incorporate the pixelated font style for a portion of the headline (e.g., "Develop"). Integrate a floating terminal component displaying the `main.wasp` example code snippet currently present on the landing page. Ensure text and CTAs are preserved and styled appropriately.
        *   `Features`
        *   `HowItWorks`
        *   `ExampleWaspApps`
        *   `Testimonials`
        *   `ShowcaseGallery`
        *   `Newsletter` (Consider merging with a CTA pattern if appropriate)
        *   `Roadmap`
        *   `Faq`
        *   `Nav`: Minor adjustments for consistency. Consider adding keyboard navigation hints inspired by `stripe.dev`.
        *   `Footer`: Minor adjustments for consistency.
4.  **Global Styles & Layout Review:** After refactoring components, review the main `web/src/pages/index.js` file and global CSS (`styles.module.css`, `index.css`).
    *   Adjust overall page padding and spacing between sections for consistency.
    *   Verify that the background light effects (`styles.leftLights`, `styles.lightsTwo`) are still positioned correctly and visually appealing with the new layouts. Make adjustments if necessary.
5.  **Responsiveness Check:** Thoroughly test the redesigned page across different screen sizes (desktop, tablet, mobile) using browser developer tools. Ensure all components reflow correctly and maintain usability. Fix any layout issues.
6.  **Review and Refine:** Perform a final visual review. Compare the result against the `stripe.dev` inspiration and the original design goals. Make fine-tuning adjustments to spacing, alignment, typography, and styling as needed for a polished look and feel.
7.  **Clean Up:** Remove any unused CSS or commented-out code.
