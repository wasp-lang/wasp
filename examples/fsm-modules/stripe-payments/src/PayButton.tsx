import React, { useState } from "react";
import { generateCheckoutSession } from "wasp/client/operations";

export default function PayButton() {
  const [isLoading, setIsLoading] = useState(false);

  const handleClick = async () => {
    setIsLoading(true);
    try {
      const { sessionUrl } = await generateCheckoutSession();
      if (sessionUrl) {
        window.location.href = sessionUrl;
      }
    } catch (error) {
      console.error("Checkout failed:", error);
      setIsLoading(false);
    }
  };

  return (
    <button
      onClick={handleClick}
      disabled={isLoading}
      style={{
        backgroundColor: "#333",
        color: "white",
        border: "none",
        borderRadius: 6,
        padding: "12px 24px",
        fontSize: 14,
        fontWeight: 600,
        cursor: isLoading ? "not-allowed" : "pointer",
        opacity: isLoading ? 0.7 : 1,
      }}
    >
      {isLoading ? "Redirecting..." : "Buy Premium"}
    </button>
  );
}
