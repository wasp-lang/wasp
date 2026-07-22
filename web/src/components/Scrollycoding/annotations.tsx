import type {
  AnnotationHandler,
  CustomPreProps,
  TokenTransitionsSnapshot,
} from "codehike/code";
import { getPreRef, InnerLine, InnerPre, InnerToken } from "codehike/code";
import {
  calculateTransitions,
  getStartingSnapshot,
} from "codehike/utils/token-transitions";
import React from "react";
import styles from "./Scrollycoding.module.css";

const MAX_TRANSITION_DURATION = 900; // milliseconds

// Class component on purpose: token transitions need getSnapshotBeforeUpdate,
// which has no hook equivalent. Adapted from the official CodeHike Docusaurus
// example (code-hike/examples/with-docusaurus).
class SmoothPre extends React.Component<CustomPreProps> {
  ref: React.RefObject<HTMLPreElement>;

  constructor(props: CustomPreProps) {
    super(props);
    this.ref = getPreRef(this.props);
  }

  render() {
    return <InnerPre merge={this.props} style={{ position: "relative" }} />;
  }

  getSnapshotBeforeUpdate() {
    return getStartingSnapshot(this.ref.current!);
  }

  componentDidUpdate(
    prevProps: never,
    prevState: never,
    snapshot: TokenTransitionsSnapshot,
  ) {
    const transitions = calculateTransitions(this.ref.current!, snapshot);
    transitions.forEach(({ element, keyframes, options }) => {
      const { translateX, translateY, ...kf } = keyframes as any;
      if (translateX && translateY) {
        kf.translate = [
          `${translateX[0]}px ${translateY[0]}px`,
          `${translateX[1]}px ${translateY[1]}px`,
        ];
      }
      element.animate(kf, {
        duration: options.duration * MAX_TRANSITION_DURATION,
        delay: options.delay * MAX_TRANSITION_DURATION,
        easing: options.easing,
        fill: "both",
      });
    });
  }
}

export const tokenTransitions: AnnotationHandler = {
  name: "token-transitions",
  PreWithRef: SmoothPre,
  Token: (props) => (
    <InnerToken merge={props} style={{ display: "inline-block" }} />
  ),
};

export const wordWrap: AnnotationHandler = {
  name: "word-wrap",
  Pre: (props) => <InnerPre merge={props} style={{ whiteSpace: "pre-wrap" }} />,
  Line: (props) => (
    <InnerLine
      merge={props}
      style={{
        textIndent: `${-props.indentation}ch`,
        marginLeft: `${props.indentation}ch`,
      }}
    />
  ),
  Token: (props) => <InnerToken merge={props} style={{ textIndent: 0 }} />,
};

// Replacement for Docusaurus' `// highlight-next-line` magic comments inside
// Scrollycoding step code: `// !mark` highlights the next line, `// !mark(1:3)`
// a range, `// !mark[/regex/]` tokens within the next line.
export const mark: AnnotationHandler = {
  name: "mark",
  Line: ({ annotation, ...props }) => (
    <InnerLine
      merge={props}
      className={annotation ? styles.markedLine : undefined}
    />
  ),
  Inline: ({ children }) => (
    <span className={styles.markedInline}>{children}</span>
  ),
};
