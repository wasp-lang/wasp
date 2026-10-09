import useBrokenLinks from "@docusaurus/useBrokenLinks";
import classNames from "classnames";
import { useState } from "react";
import { track } from "../lib/analytics";

const createNewEmailSubscriberApiEndpoint =
  "https://app.loops.so/api/newsletter-form/clg0zndc9000ajn0f8a1bhgmu";

const NEWSLETTER_INPUT_ID = "newsletter-input";

// Do not rename these values: Loops stores them on each new contact.
const LOOPS_SOURCE_BY_PLACEMENT = {
  body: "web-homepage",
  footer: "web-footer",
  post_card: "web-post",
};

const SUCCESS_MESSAGE = "Thank you for subscribing! 🙏";
const ERROR_MESSAGE = "🛑 Oops! Something went wrong. Please try again.";

const buttonVariantClasses = {
  yellow: "bg-wasp-yellow text-wasp-black hover:bg-wasp-yellow-dark",
  black: "bg-wasp-black text-wasp-yellow hover:bg-wasp-g7",
};

const SubscribeForm = ({
  className,
  inputBgColor = "bg-wasp-white",
  buttonVariant = "yellow",
  placement = "body",
}) => {
  const [email, setEmail] = useState("");
  const [message, setMessage] = useState("");
  useBrokenLinks().collectAnchor(NEWSLETTER_INPUT_ID);

  const handleSubmit = async (event) => {
    // NOTE(matija): without this, the whole page reloads on form submission.
    event.preventDefault();

    const body = new URLSearchParams({
      userGroup: "",
      email,
      source: LOOPS_SOURCE_BY_PLACEMENT[placement] ?? "web",
    });

    let response;
    try {
      response = await fetch(createNewEmailSubscriberApiEndpoint, {
        method: "POST",
        body: body.toString(),
        headers: {
          "Content-Type": "application/x-www-form-urlencoded",
        },
      });
    } catch (error) {
      track("Newsletter: Error", { placement, status: "network" });
      setMessage(ERROR_MESSAGE);
      return;
    }

    const result = await response.json().catch(() => null);
    if (response.ok && result?.success) {
      track("Newsletter: Signup", { placement });
      setMessage(SUCCESS_MESSAGE);
    } else {
      track("Newsletter: Error", {
        placement,
        status: getErrorStatus(response.status),
      });
      setMessage(ERROR_MESSAGE);
    }
  };

  return (
    <>
      {message ? (
        <p className="border-2 border-wasp-black bg-wasp-yellow-light px-4 py-3 font-mono text-sm font-bold text-wasp-black">
          {message}
        </p>
      ) : (
        <form
          onSubmit={handleSubmit}
          // `ph-no-capture` keeps the form out of PostHog autocapture.
          className={classNames("ph-no-capture sm:flex", className)}
        >
          <input
            aria-label="Email address"
            type="email"
            name="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            id={NEWSLETTER_INPUT_ID}
            required
            autoComplete="email"
            placeholder="you@awesomedev.com"
            className={classNames(
              "w-full appearance-none border-2 border-wasp-black px-4 py-2.5 font-mono text-sm text-wasp-black placeholder:text-wasp-g4 focus:outline-none focus:ring-2 focus:ring-wasp-yellow-dark",
              inputBgColor,
            )}
          />
          <div className="mt-3 sm:ml-3 sm:mt-0">
            <button
              type="submit"
              className={classNames(
                "w-full whitespace-nowrap border-2 border-wasp-black px-5 py-2.5 font-mono text-sm font-bold uppercase tracking-wide transition duration-150 ease-out",
                buttonVariantClasses[buttonVariant],
              )}
            >
              Subscribe
            </button>
          </div>
        </form>
      )}
    </>
  );
};

function getErrorStatus(httpStatus) {
  if (httpStatus === 429) {
    return "429";
  }
  if (httpStatus >= 500) {
    return "500";
  }
  return "400";
}

export default SubscribeForm;
