import { useState, useEffect } from 'react'
import { Droppable as RBDDroppable } from 'react-beautiful-dnd'


export const Droppable = ({ children, ...props }) => {
    const [enabled, setEnabled] = useState(false);
    useEffect(() => {
      const animation = requestAnimationFrame(() => setEnabled(true));
      return () => {
        cancelAnimationFrame(animation);
        setEnabled(false);
      };
    }, []);
    if (!enabled) {
      return null;
    }
    return <RBDDroppable {...props}>{children}</RBDDroppable>;
  };
  