import { Block, HighlightedCodeBlock, parseProps } from "codehike/blocks";
import type { HighlightedCode } from "codehike/code";
import { Pre } from "codehike/code";
import {
  Selectable,
  Selection,
  SelectionProvider,
} from "codehike/utils/selection";
import { z } from "zod";
import { mark, tokenTransitions, wordWrap } from "./annotations";
import styles from "./Scrollycoding.module.css";

const Schema = Block.extend({
  steps: z.array(Block.extend({ code: HighlightedCodeBlock })),
});

/**
 * CodeHike scrollycoding layout: prose steps on the left, a sticky code panel
 * on the right that follows the reader's scroll position and animates between
 * the steps' code states.
 *
 * Authored in MDX as:
 *
 *   <Scrollycoding>
 *     ## !!steps Step title
 *     Step prose...
 *     ```ts ! main.wasp.ts
 *     // full file state at this step
 *     ```
 *   </Scrollycoding>
 */
export function Scrollycoding(props: unknown) {
  const { steps } = parseProps(props, Schema);
  return (
    <SelectionProvider className={styles.container}>
      <div className={styles.stepsContainer}>
        {steps.map((step, i) => (
          <Selectable
            key={i}
            index={i}
            selectOn={["click", "scroll"]}
            className={styles.step}
          >
            <div className={styles.stepTitle}>{step.title}</div>
            <div>{step.children}</div>
          </Selectable>
        ))}
      </div>
      <div className={styles.sticker}>
        <Selection
          from={steps.map((step, i) => (
            <CodeSticker key={i} codeblock={step.code} />
          ))}
        />
      </div>
    </SelectionProvider>
  );
}

function CodeSticker({ codeblock }: { codeblock: HighlightedCode }) {
  return (
    <div className={styles.stickyCode}>
      {codeblock.meta && (
        <div className={styles.filename}>{codeblock.meta}</div>
      )}
      <Pre
        className={styles.pre}
        code={codeblock}
        handlers={[tokenTransitions, wordWrap, mark]}
      />
    </div>
  );
}
