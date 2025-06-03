// From: https://gist.github.com/adamwathan/3b9f3ad1a285a2d1b482769aeb862467

import { createContext, useContext, useEffect, useRef } from "react";
import { CSSTransition as ReactCSSTransition } from "react-transition-group";

const TransitionContext = createContext({
  parent: {},
});

function useIsInitialRender() {
  const isInitialRender = useRef(true);
  useEffect(() => {
    isInitialRender.current = false;
  }, []);
  return isInitialRender.current;
}

function CSSTransition({
  show,
  enter = "",
  enterFrom = "",
  enterTo = "",
  leave = "",
  leaveFrom = "",
  leaveTo = "",
  appear,
  children,
  nodeRef,
}) {
  const enterClasses = enter.split(" ").filter((s) => s.length);
  const enterFromClasses = enterFrom.split(" ").filter((s) => s.length);
  const enterToClasses = enterTo.split(" ").filter((s) => s.length);
  const leaveClasses = leave.split(" ").filter((s) => s.length);
  const leaveFromClasses = leaveFrom.split(" ").filter((s) => s.length);
  const leaveToClasses = leaveTo.split(" ").filter((s) => s.length);

  function addClasses(classes) {
    classes.length && nodeRef.current?.classList?.add(...classes);
  }

  function removeClasses(classes) {
    classes.length && nodeRef.current?.classList?.remove(...classes);
  }

  return (
    <ReactCSSTransition
      nodeRef={nodeRef}
      appear={appear}
      unmountOnExit
      in={show}
      addEndListener={(done) => {
        nodeRef.current?.addEventListener("transitionend", done, false);
      }}
      onEnter={() => {
        addClasses([...enterClasses, ...enterFromClasses]);
      }}
      onEntering={() => {
        removeClasses(enterFromClasses);
        addClasses(enterToClasses);
      }}
      onEntered={() => {
        removeClasses([...enterToClasses, ...enterClasses]);
      }}
      onExit={() => {
        addClasses([...leaveClasses, ...leaveFromClasses]);
      }}
      onExiting={() => {
        removeClasses(leaveFromClasses);
        addClasses(leaveToClasses);
      }}
      onExited={() => {
        removeClasses([...leaveToClasses, ...leaveClasses]);
      }}
    >
      {children}
    </ReactCSSTransition>
  );
}

function Transition({ show, appear, ...rest }) {
  const { parent } = useContext(TransitionContext);
  const isInitialRender = useIsInitialRender();
  const isChild = show === undefined;

  if (isChild) {
    return (
      <CSSTransition
        appear={parent.appear || !parent.isInitialRender}
        show={parent.show}
        {...rest}
      />
    );
  }

  return (
    <TransitionContext.Provider
      value={{
        parent: {
          show,
          isInitialRender,
          appear,
        },
      }}
    >
      <CSSTransition appear={appear} show={show} {...rest} />
    </TransitionContext.Provider>
  );
}

export default Transition;
