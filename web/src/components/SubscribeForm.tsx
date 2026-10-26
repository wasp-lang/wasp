import classNames from "classnames";
import { useState, type FormEvent } from "react";
import {
  getPlacement,
  track,
  type NewsletterErrorStatus,
  type Placement,
} from "../lib/analytics";

const createNewEmailSubscriberApiEndpoint =
  "https://app.loops.so/api/newsletter-form/clg0zndc9000ajn0f8a1bhgmu";

// Do not rename these values: Loops stores them on each new contact.
const LOOPS_SOURCE_BY_PLACEMENT: Partial<Record<Placement, string>> = {
  newsletter: "web-homepage",
  footer: "web-footer",
  post_card: "web-post",
};

const SUCCESS_MESSAGE = "Thank you for subscribing! 🙏";
const ERROR_MESSAGE = "🛑 Oops! Something went wrong. Please try again.";

const buttonVariantClasses = {
  yellow: "bg-wasp-yellow text-wasp-black hover:bg-wasp-yellow-dark",
  black: "bg-wasp-black text-wasp-yellow hover:bg-wasp-g7",
};

type SubscribeFormProps = {
  className?: string;
  inputBgColor?: string;
  buttonVariant?: keyof typeof buttonVariantClasses;
  inputId?: string;
  // Replaces the Loops source that the placement gives, for a page that needs
  // its own source.
  source?: string;
  // Also adds the contact to this Loops mailing list.
  mailingListId?: string;
};

const SubscribeForm = ({
  className,
  inputBgColor = "bg-wasp-white",
  buttonVariant = "yellow",
  inputId,
  source,
  mailingListId,
}: SubscribeFormProps) => {
  const [email, setEmail] = useState("");
  const [message, setMessage] = useState("");

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    // NOTE(matija): without this, the whole page reloads on form submission.
    event.preventDefault();
    // React clears `currentTarget` after the first `await`.
    const form = event.currentTarget;

    const body = new URLSearchParams({
      userGroup: "",
      email,
      source: source ?? LOOPS_SOURCE_BY_PLACEMENT[getPlacement(form)] ?? "web",
    });
    if (mailingListId) {
      body.set("mailingLists", mailingListId);
    }

    let response: Response;
    try {
      response = await fetch(createNewEmailSubscriberApiEndpoint, {
        method: "POST",
        body: body.toString(),
        headers: {
          "Content-Type": "application/x-www-form-urlencoded",
        },
      });
    } catch (error) {
      track(form, "Newsletter: Error", { status: "network" });
      setMessage(ERROR_MESSAGE);
      return;
    }

    const result = await response.json().catch(() => null);
    if (response.ok && result?.success) {
      track(form, "Newsletter: Signup", {});
      setMessage(SUCCESS_MESSAGE);
    } else {
      track(form, "Newsletter: Error", {
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
            id={inputId}
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

function getErrorStatus(httpStatus: number): NewsletterErrorStatus {
  if (httpStatus === 429) {
    return "429";
  }
  if (httpStatus >= 500 && httpStatus < 600) {
    return "5xx";
  }
  return "other";
}

export default SubscribeForm;
