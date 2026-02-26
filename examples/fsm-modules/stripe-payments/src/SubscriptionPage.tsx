import React from "react";
import {
  useQuery,
  getSubscriptionStatus,
  cancelSubscription,
  getCustomerPortalUrl,
} from "wasp/client/operations";
import PayButton from "./PayButton.js";

export default function SubscriptionPage() {
  const { data, isLoading, error } = useQuery(getSubscriptionStatus);

  const handleManageBilling = async () => {
    try {
      const { portalUrl } = await getCustomerPortalUrl();
      if (portalUrl) {
        window.location.href = portalUrl;
      }
    } catch (err) {
      console.error("Failed to open billing portal:", err);
    }
  };

  const handleCancel = async () => {
    if (!confirm("Are you sure you want to cancel your subscription?")) return;
    try {
      await cancelSubscription();
    } catch (err) {
      console.error("Failed to cancel subscription:", err);
    }
  };

  if (isLoading) return <div style={styles.container}>Loading...</div>;
  if (error)
    return (
      <div style={styles.container}>
        <p style={{ color: "red" }}>Error: {error.message}</p>
      </div>
    );

  const isActive =
    data?.subscriptionStatus === "active" ||
    data?.subscriptionStatus === "cancel_at_period_end";

  return (
    <div style={styles.container}>
      <h1>Subscription</h1>

      <div style={styles.card}>
        <h2 style={{ margin: "0 0 8px" }}>
          {isActive ? "Premium Plan" : "Free Plan"}
        </h2>
        <p style={{ color: "#666", margin: "0 0 16px" }}>
          {data?.subscriptionStatus === "active" && "Your subscription is active."}
          {data?.subscriptionStatus === "cancel_at_period_end" &&
            "Your subscription will cancel at the end of the billing period."}
          {data?.subscriptionStatus === "past_due" &&
            "Your payment is past due. Please update your billing info."}
          {!data?.subscriptionStatus &&
            "Upgrade to Premium for unlimited columns."}
          {data?.subscriptionStatus === "deleted" &&
            "Your subscription has been cancelled."}
        </p>

        {data?.datePaid && (
          <p style={{ color: "#888", fontSize: 14, margin: "0 0 16px" }}>
            Last payment: {new Date(data.datePaid).toLocaleDateString()}
          </p>
        )}

        {!isActive && <PayButton />}

        {isActive && (
          <div style={{ display: "flex", gap: 12 }}>
            <button onClick={handleManageBilling} style={styles.secondaryButton}>
              Manage Billing
            </button>
            {data?.subscriptionStatus === "active" && (
              <button onClick={handleCancel} style={styles.dangerButton}>
                Cancel Subscription
              </button>
            )}
          </div>
        )}
      </div>
    </div>
  );
}

const styles: Record<string, React.CSSProperties> = {
  container: {
    maxWidth: 600,
    margin: "2rem auto",
    fontFamily: "sans-serif",
    padding: "0 16px",
  },
  card: {
    border: "1px solid #e0e0e0",
    borderRadius: 8,
    padding: 24,
    background: "#fafafa",
  },
  secondaryButton: {
    backgroundColor: "#f0f0f0",
    color: "#333",
    border: "1px solid #ddd",
    borderRadius: 6,
    padding: "10px 20px",
    fontSize: 14,
    fontWeight: 600,
    cursor: "pointer",
  },
  dangerButton: {
    backgroundColor: "#fff",
    color: "#dc3545",
    border: "1px solid #dc3545",
    borderRadius: 6,
    padding: "10px 20px",
    fontSize: 14,
    fontWeight: 600,
    cursor: "pointer",
  },
};
