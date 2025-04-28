import React, { useState, useEffect, useRef, useCallback } from 'react'
import Link from '@docusaurus/Link'

import CodeHighlight from './CodeHighlight'

import {
  Terminal,
  ArrowUpRight,
  Play,
  BookOpen,
  Grid,
  Layout,
  Trello,
  X as IconX,
} from 'react-feather'

// Terminal, BookOpen, Grid, Layout, Trello, FileText

import InstallCmd from './InstallCmd'
import SectionContainer from './Layouts/SectionContainer'

const StartIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="16"
    height="16"
    viewBox="0 0 24 24"
    fill="none"
    stroke="currentColor"
    strokeWidth="2"
    strokeLinecap="round"
    strokeLinejoin="round"
    opacity="0.5"
  >
    <polyline points="13 17 18 12 13 7"></polyline>
    <polyline points="6 17 11 12 6 7"></polyline>
  </svg>
)

const PHBadge = () => (
  <a
    href="https://www.producthunt.com/posts/wasp-lang-beta"
    target="_blank"
    rel="noreferrer"
  >
    <img
      className="w-32 md:w-[180px]"
      src="https://api.producthunt.com/widgets/embed-image/v1/top-post-badge.svg?post_id=277135&theme=light&period=daily"
      alt="Wasp&#0045;lang&#0032;Alpha - Develop&#0032;web&#0032;apps&#0032;in&#0032;React&#0032;&#0038;&#0032;Node&#0046;js&#0032;with&#0032;no&#0032;boilerplate | Product Hunt"
    />
  </a>
)

const Hero = () => {
  const [isCodeViewerVisible, setIsCodeViewerVisible] = useState(false);

  // Keyboard listener for 'c' key
  useEffect(() => {
    const handleKeyDown = (event) => {
      // Ignore if modifier keys are pressed or if inside an input element
      if (event.metaKey || event.ctrlKey || event.altKey || event.shiftKey || 
          event.target.tagName === 'INPUT' || event.target.tagName === 'TEXTAREA') {
        return;
      }
      if (event.key.toLowerCase() === 'c') {
        setIsCodeViewerVisible(prev => !prev);
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    // Cleanup listener
    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, []); // Empty dependency array ensures this runs once on mount

  const waspFileSourceCode = `app todoApp {
  title: "ToDo App",  // visible in the browser tab
  auth: { // full-stack auth out-of-the-box
    userEntity: User, 
    methods: { google: {}, gitHub: {}, email: {...} }
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  authRequired: true, // Limit access to logged in users.
  component: import Main from "@client/Main.tsx" // Your React code.
}

query getTasks {
  fn: import { getTasks } from "@server/tasks.js", // Your Node.js code.
  entities: [Task] // Automatic cache invalidation.
}`

  const prismaFileSourceCode = `model Task { ... } // Your Prisma data model`

  const handleShowCodeClick = () => {
      setIsCodeViewerVisible(prev => !prev);
  };

  const handleCloseCodeViewer = () => {
      setIsCodeViewerVisible(false);
  };

  return (
    <SectionContainer className="relative pb-5 pt-24">

        {/* Conditionally render the main FileViewer */}
        {isCodeViewerVisible && (
          <FileViewer
            fileName="todoApp.wasp"
            fileExplanation="Wasp config file"
            link="https://github.com/wasp-lang/wasp/blob/release/examples/todo-typescript/main.wasp"
            initialPosition={{ x: 0, y: 0 }}
            onClose={handleCloseCodeViewer} // Pass close handler
          >
            <CodeHighlight language="wasp" source={waspFileSourceCode} />
          </FileViewer>
        )}
        {/* <FileViewer
            fileName="schema.prisma"
            fileExplanation="Wasp entities schema"
            link="https://github.com/wasp-lang/wasp/blob/release/examples/todo-typescript/schema.prisma"
            initialPosition={{ x: 40, y: 300 }}
          >
            <CodeHighlight language="prisma" source={prismaFileSourceCode} />
          </FileViewer> */}

      <div className="lg:grid lg:grid-cols-12 lg:gap-16">
        <div className="z-10 space-y-12 lg:col-span-10">
          {/* Hero title and subtitle */}
          <div>
            <h1
              className={`
                text-4xl font-bold text-neutral-700
                lg:text-7xl lg:leading-tight
              `}
            >
              Full-stack{' '}
              <span className="text-yellow-500 lg:text-8xl font-pixelated mx-3">Everything</span>
              <br />
              in a few lines of code.
            </h1>

            <p className="mt-4 text-xl text-neutral-500 sm:mt-5 lg:text-xl">
              Rails-like framework for React, Node.js and Prisma. Build your app
              in a day and deploy it with a single CLI command.
            </p>
          </div>{' '}
          {/* EOF Hero title and subtitle */}
          <div className="flex items-center gap-2">
            <Link to="/docs/quick-start">
              <button
                className={`
                   inline-flex items-center space-x-2
                   rounded border border-yellow-500
                   bg-yellow-500 px-3 py-2 text-sm
                   leading-4 text-white transition
                   duration-200
                   ease-out hover:border-yellow-400 hover:bg-yellow-400
                   `}
              >
                <Terminal size={16} />
                <span>{'Get Started'}</span>
              </button>
            </Link>

            <Link to="/docs">
              <button
                className={`
                   inline-flex items-center space-x-2
                   rounded border border-neutral-500
                   px-3 py-2
                   text-sm leading-4
                   text-neutral-700
                   transition duration-200
                   ease-out hover:border-neutral-400 hover:text-neutral-400
                   `}
              >
                <BookOpen size={16} />
                <span>Documentation</span>
              </button>
            </Link>

            {/* New Show Code Button */}
            <button
              onClick={handleShowCodeClick}
              className={`
                 inline-flex items-center space-x-2
                 rounded border border-neutral-500
                 px-3 py-2
                 text-sm leading-4
                 text-neutral-700
                 transition duration-200
                 ease-out hover:border-neutral-400 hover:text-neutral-400
                 `}
              title="Press 'c' to toggle"
            >
              {/* Consider adding an icon here too if desired */}
              <span>{isCodeViewerVisible ? 'Hide Code' : 'Show Code'}</span>
            </button>
          </div>
          <div className="flex flex-col gap-4">
            <small className="text-xs text-neutral-500">Works with</small>

            <div className="flex">
              <img
                className="h-8 pr-5 md:h-10 md:pr-10"
                src="img/lp/react-logo-gray.svg"
                alt="React"
              />
              <img
                className="h-8 pr-5 md:h-10 md:pr-10"
                src="img/lp/nodejs-logo-gray.svg"
                alt="Node"
              />
              <img
                className="h-8 pr-5 md:h-10 md:pr-10"
                src="img/lp/prisma-logo-gray.svg"
                alt="Prisma"
              />
            </div>

            <span className="mt-6 flex items-center">
              <small className="text-xs text-neutral-500">Backed by</small>
              <img
                className="ml-2 w-24"
                src="img/lp/yc-logo-rounded.webp"
                alt="YC"
              />
            </span>
          </div>
        </div>
      </div>

      {/* 1-min video */}
      {/*
      <div className='flex justify-center mt-20'>
        <div className='w-full lg:w-2/3 xl:w-3/5'>
          <div
            className="relative w-full rounded-md shadow-lg"
            style={{ padding: '56.25% 0 0 0' }}
          >
            <iframe
              title="Demo video showcasing Wasp"
              className="absolute h-full w-full rounded-md"
              src="https://www.youtube-nocookie.com/embed/YaaTJOhx68I?playlist=YaaTJOhx68I&autoplay=0&loop=1&controls=0&showinfo=1&modestbranding=0&rel=0&disablekb=0&mute=1"
              style={{ top: 0, left: 0 }}
              frameBorder="0"
              allow="autoplay; modestbranding; encrypted-media"
            />
          </div>
        </div>
      </div>
      */}

      {/* PH & YC badges */}
      {/*
      <div className='flex justify-center items-center space-x-4 mt-20 mb-10 md:mt-28 md:mb-0'>
        <PHBadge />
        <div
          className={`
            h-11 border border-transparent border-l-neutral-400/50
          `}
        />
        <img
          className='w-32 md:w-[180px]'
          src='img/lp/yc-logo.webp'
          alt='YC'
        />
      </div>
      */}
    </SectionContainer>
  )
}

function FileViewer({ fileName, fileExplanation, link, children, initialPosition = { x: 0, y: 0 }, onClose }) {
  const [position, setPosition] = useState(initialPosition);
  // Default width based on Tailwind max-w-lg (approx 512px)
  // Default height - auto or a fixed value? Let's start with a fixed value for simplicity.
  const [size, setSize] = useState({ width: 512, height: 300 }); 
  const [isDragging, setIsDragging] = useState(false);
  const [isResizing, setIsResizing] = useState(false); // New state for resizing
  const dragStartInfo = useRef({ initialMouseX: 0, initialMouseY: 0, initialPosX: 0, initialPosY: 0 });
  // Ref for resize start info (mouse pos and initial size)
  const resizeStartInfo = useRef({ initialMouseX: 0, initialMouseY: 0, initialWidth: 0, initialHeight: 0 }); 
  const viewerRef = useRef(null);
  const resizeHandleRef = useRef(null); // Ref for the resize handle

  // --- Dragging Logic --- 
  const handleMouseDownDrag = useCallback((e) => {
    // Prevent drag if clicking the resize handle or a link
    if (e.button !== 0 || e.target.closest('a') || e.target === resizeHandleRef.current) return;

    setIsDragging(true);
    dragStartInfo.current = {
      initialMouseX: e.pageX,
      initialMouseY: e.pageY,
      initialPosX: position.x,
      initialPosY: position.y,
    };
    document.body.style.cursor = 'grabbing';
    e.preventDefault();
  }, [position.x, position.y]);

  const handleMouseMoveDrag = useCallback((e) => {
    if (!isDragging) return;

    const deltaX = e.pageX - dragStartInfo.current.initialMouseX;
    const deltaY = e.pageY - dragStartInfo.current.initialMouseY;

    let newX = dragStartInfo.current.initialPosX + deltaX;
    let newY = dragStartInfo.current.initialPosY + deltaY;

    setPosition({ x: newX, y: newY });
  }, [isDragging]);

  const handleMouseUpDrag = useCallback(() => {
    if (isDragging) {
      setIsDragging(false);
      document.body.style.cursor = 'default';
    }
  }, [isDragging]);

  // --- Resizing Logic --- 
  const handleMouseDownResize = useCallback((e) => {
    // Only left clicks
    if (e.button !== 0) return;

    setIsResizing(true);
    resizeStartInfo.current = {
      initialMouseX: e.pageX,
      initialMouseY: e.pageY,
      initialWidth: size.width,
      initialHeight: size.height,
    };
    // No cursor change needed here as the handle itself has the resize cursor
    e.preventDefault(); // Prevent text selection etc.
    e.stopPropagation(); // Stop the event from bubbling up to the main drag handler

  }, [size.width, size.height]);

  const handleMouseMoveResize = useCallback((e) => {
    if (!isResizing) return;

    const deltaX = e.pageX - resizeStartInfo.current.initialMouseX;
    const deltaY = e.pageY - resizeStartInfo.current.initialMouseY;

    // Min dimensions (e.g., 200x100)
    const minWidth = 200;
    const minHeight = 100;

    const newWidth = Math.max(minWidth, resizeStartInfo.current.initialWidth + deltaX);
    const newHeight = Math.max(minHeight, resizeStartInfo.current.initialHeight + deltaY);

    setSize({ width: newWidth, height: newHeight });

  }, [isResizing]);

  const handleMouseUpResize = useCallback(() => {
    if (isResizing) {
      setIsResizing(false);
      // No cursor reset needed here as the body cursor wasn't changed
    }
  }, [isResizing]);


  // --- Combined useEffect for Listeners --- 
  useEffect(() => {
    const handleGlobalMouseMove = (e) => {
        handleMouseMoveDrag(e);
        handleMouseMoveResize(e);
    };
    const handleGlobalMouseUp = (e) => {
        handleMouseUpDrag(e);
        handleMouseUpResize(e);
    };

    if (isDragging || isResizing) {
      window.addEventListener('mousemove', handleGlobalMouseMove);
      window.addEventListener('mouseup', handleGlobalMouseUp);
    }

    return () => {
      window.removeEventListener('mousemove', handleGlobalMouseMove);
      window.removeEventListener('mouseup', handleGlobalMouseUp);
      // Ensure cursor is reset if component unmounts while dragging
      if (document.body.style.cursor === 'grabbing') {
        document.body.style.cursor = 'default';
      }
    };
    // Rerun if any state/handler changes
  }, [isDragging, isResizing, handleMouseMoveDrag, handleMouseUpDrag, handleMouseMoveResize, handleMouseUpResize]);

  // Internal close handler to stop propagation if needed
  const handleCloseClick = (e) => {
      e.stopPropagation(); // Prevent triggering drag start
      if (onClose) {
          onClose();
      }
  };

  return (
    <div
      ref={viewerRef}
      className={`absolute z-20 select-none overflow-hidden border border-black shadow-lg`}
      style={{
        top: `${position.y}px`,
        left: `${position.x}px`,
        width: `${size.width}px`, // Apply width from state
        height: `${size.height}px`, // Apply height from state
        cursor: isDragging ? 'grabbing' : 'grab',
      }}
      onMouseDown={handleMouseDownDrag} // Use the specific drag handler
    >
      {/* Header */}
      <div
        className="flex h-6 w-full items-center justify-between border-b border-neutral-600 bg-[#e5e5e5] px-2"
        // Header itself doesn't need mousedown now
      >
        <Link
          to={link}
          onMouseDown={(e) => e.stopPropagation()}
          target="_blank"
          rel="noopener noreferrer"
        >
          <span
            className={`
              flex items-center space-x-1 text-xs text-neutral-800 transition
              duration-200 ease-out hover:text-neutral-200
            `}
          >
            <span>{fileName}</span>
            <ArrowUpRight size={14} />
          </span>
        </Link>
        <button
          onClick={handleCloseClick}
          className="p-0.5 font-mono text-neutral-400 transition-colors hover:bg-neutral-700 hover:text-neutral-100"
          aria-label="Close code viewer"
        >
          <IconX size={14} strokeWidth={3} />
        </button>
      </div>
      {/* Body - Allow overflow for scrolling if content exceeds size */}
      <div className="h-[calc(100%-1.5rem)] w-full overflow-auto bg-[#f5f5f5] text-sm">
        {children}
      </div>
      {/* Resize Handle (Bottom Right) */}
      <div
        ref={resizeHandleRef}
        onMouseDown={handleMouseDownResize} // Attach resize handler
        className="absolute bottom-0 right-0 h-4 w-4 cursor-nwse-resize bg-[#ffdd33] hover:bg-red-500/70" // Light yellow with darker hover
        style={{ zIndex: 1 }} // Ensure it's clickable over content
      ></div>
    </div>
  )
}

export default Hero
