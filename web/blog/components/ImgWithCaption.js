import React from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";

const ImgWithCaption = (props) => {
  return (
    <div>
      <p align='center'>
        <figure>
          <img style={{ width: props.width }} alt={props.alt} src={useBaseUrl(props.source)} />
          <figcaption class='image-caption' style={{ fontStyle: 'italic', opacity: 0.6, fontSize: '0.9rem'  }}>
            {props.caption}
          </figcaption>
        </figure>
      </p>
    </div>
  );
};

export default ImgWithCaption;
