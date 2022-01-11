import React from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";

const ImgWithCaption = (props) => {
  return (
    <div>
      <p align="center">
        <figure>
          <img alt={props.alt} src={useBaseUrl(props.source)} />
          <figcaption class="image-caption">{props.caption}</figcaption>
        </figure>
      </p>
    </div>
  );
};

export default ImgWithCaption;
