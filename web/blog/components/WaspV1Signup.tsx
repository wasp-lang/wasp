import SubscribeForm from "@site/src/components/SubscribeForm";
import { trackPlacementAttributes } from "@site/src/lib/analytics";

// The Loops mailing list for the email when Wasp 1.0 lands.
const WASP_V1_MAILING_LIST_ID = "cmvfc4mns050y0kzb72vw51px";

export const WaspV1Signup = ({ source }: { source: string }) => (
  <div
    {...trackPlacementAttributes("wasp_v1_signup")}
    className="mb-6 flex flex-col items-start space-y-3 rounded-lg border border-yellow-500/25 bg-yellow-500/20 p-8"
  >
    <p className="text-xl font-semibold">Get an email when Wasp 1.0 lands!</p>
    <SubscribeForm
      className="self-stretch"
      source={source}
      mailingListId={WASP_V1_MAILING_LIST_ID}
    />
  </div>
);
