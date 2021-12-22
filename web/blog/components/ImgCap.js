import React from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";

const ImgCap = (props) => {
  return (
    <div>
      <p align="center">
        <figure>
          <img
            // alt="Waspello - no optimistic UI update"
            alt={props.alt}
            // src={useBaseUrl("img/waspello-no-opt-UI-updates.gif")}
            src={useBaseUrl(props.source)}
          />
          <figcaption class="image-caption">
            {/* Without an optimistic UI update, there is a delay */}
            {props.caption}
          </figcaption>
        </figure>
      </p>
    </div>
  );
};

export default ImgCap;
