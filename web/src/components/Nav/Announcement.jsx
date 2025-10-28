import { useHistory } from "@docusaurus/router";
import classNames from "classnames";

import styles from "../../pages/styles.module.css";

const Announcement = () => {
  let history = useHistory();

  const handleLink = () => {
    history.push("/blog/2025/10/08/design-ai-thon");
  };

  return (
    <div
      onClick={handleLink}
      className={classNames(
        styles.gradientBackground,
        `cursor-pointer flex-row space-x-3 overflow-hidden text-white`,
      )}
    >
      <div
        className={`mx-auto flex items-center justify-center divide-white p-3 text-sm font-medium lg:container lg:divide-x lg:px-16 xl:px-20`}
      >
        <span className="item-center flex gap-2 px-3">
          <span>
            <b className="text-yellow-300">🎨 Wasp Design-AI-Thon is live!</b>{" "}
            <span className="font-medium underline">Oct 10 - Oct 19</span>
          </span>
        </span>

        <span className="hidden items-center space-x-2 px-3 lg:flex">
          <span
            className={`cursor-pointer rounded-full bg-neutral-700 px-2.5 py-1 text-xs hover:bg-neutral-600`}
          >
            Join now <span className="text-yellow-300">→</span>
          </span>
        </span>
      </div>
    </div>
  );
};

export default Announcement;
