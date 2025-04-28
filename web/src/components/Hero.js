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
          onClose={handleCloseCodeViewer}
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
              <span className="font-pixelated mx-3 text-yellow-500 lg:text-8xl">
                Everything
              </span>
              <br />
              in a few lines of code.
            </h1>

            <p className="mt-4 text-xl text-neutral-500 sm:mt-5 lg:text-xl">
              The React, NodeJS, & Prisma alternative to Laravel -- truly full-stack w/ batteries-included.
            </p>
          </div>

          {/* EOF Hero title and subtitle */}
          <div className="flex items-center justify-start gap-1">
            {/* Get Started Button - Apply new style */}
            <Link to="/docs/quick-start">
              <button
                // Replaced classes with nav button style
                className={`
                   inline-flex items-center rounded-sm bg-[#ffcc00]/70 p-2 
                   font-mono text-xs text-neutral-700 
                   backdrop-blur-sm transition-colors duration-150 hover:bg-[#ffcc00] hover:text-neutral-800
                 `}
              >
                {/* Removed icon, updated text */}
                <span className="flex items-center gap-1">
                  [S]
                  GET STARTED →
                </span>
              </button>
            </Link>

            {/* Removed Documentation Button */}

            {/* Show Code Button - Apply new style */}
            <button
              onClick={handleShowCodeClick}
              // Replaced classes with nav button style
              className={`
                  inline-flex items-center rounded-sm bg-neutral-200/70 p-2 
                  font-mono text-xs text-neutral-700 
                  backdrop-blur-sm transition-colors duration-150 hover:bg-[#ffcc00] hover:text-neutral-800
                 `}
              title="Press 'c' to toggle"
            >
              {/* Changed text to uppercase */}
              <span>[C] CODE EXAMPLES</span>
            </button>
          </div>
        </div>
        <div className="lg:col-span-2">
          {/* <small className="text-xs text-neutral-500">Works with</small> */}

          <div className="flex flex-col justify-start gap-4 p-4 sm:px-6 lg:px-8">
            <img
              className="h-8 pr-3 md:h-20 md:pr-5"
              src="img/lp/react-logo-gray.svg"
              alt="React"
            />
            <img
              className="h-8 pr-3 md:h-20 md:pr-5"
              src="img/lp/nodejs-logo-gray.svg"
              alt="Node"
            />
            <img
              className="h-8 pr-3 md:h-20 md:pr-5"
              src="img/lp/prisma-logo-gray.svg"
              alt="Prisma"
            />
          </div>

          {/* <span className="mt-6 flex items-center">
            <small className="text-xs text-neutral-500">Backed by</small>
            <img
              className="ml-2 w-24"
              src="img/lp/yc-logo-rounded.webp"
              alt="YC"
            />
          </span> */}
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

function FileViewer({ fileName, fileExplanation, link, children, onClose }) {
  const [size, setSize] = useState({ width: 712, height: 400 });
  const [isResizing, setIsResizing] = useState(false);
  const resizeStartInfo = useRef({ initialMouseX: 0, initialMouseY: 0, initialWidth: 0, initialHeight: 0 });
  const viewerRef = useRef(null);
  const resizeHandleRef = useRef(null);

  // State for position and dragging
  const [position, setPosition] = useState({ x: 0, y: 0 });
  const [isDragging, setIsDragging] = useState(false);
  const dragStartInfo = useRef({ initialMouseX: 0, initialMouseY: 0, initialPosX: 0, initialPosY: 0 });

  // Effect to center the viewer initially
  useEffect(() => {
    if (viewerRef.current) {
      const viewerRect = viewerRef.current.getBoundingClientRect();
      setPosition({
        x: (window.innerWidth - viewerRect.width) / 2,
        y: (window.innerHeight - viewerRect.height) / 2,
      });
    }
  }, []); // Runs once on mount

  const handleMouseDownResize = useCallback((e) => {
    if (e.button !== 0) return; // Only react to left mouse button
    setIsResizing(true);
    resizeStartInfo.current = {
      initialMouseX: e.pageX,
      initialMouseY: e.pageY,
      initialWidth: size.width,
      initialHeight: size.height,
    };
    e.preventDefault();
    e.stopPropagation();
  }, [size.width, size.height]);

  const handleMouseMoveResize = useCallback((e) => {
    if (!isResizing) return;
    const deltaX = e.pageX - resizeStartInfo.current.initialMouseX;
    const deltaY = e.pageY - resizeStartInfo.current.initialMouseY;
    const minWidth = 200;
    const minHeight = 100;
    const newWidth = Math.max(minWidth, resizeStartInfo.current.initialWidth + deltaX);
    const newHeight = Math.max(minHeight, resizeStartInfo.current.initialHeight + deltaY);
    setSize({ width: newWidth, height: newHeight });
  }, [isResizing]);

  const handleMouseUpResize = useCallback(() => {
    if (isResizing) {
      setIsResizing(false);
    }
  }, [isResizing]);

  // Drag handlers
  const handleMouseDownDrag = useCallback((e) => {
      if (e.button !== 0) return; // Only react to left mouse button
      setIsDragging(true);
      dragStartInfo.current = {
          initialMouseX: e.pageX,
          initialMouseY: e.pageY,
          initialPosX: position.x,
          initialPosY: position.y,
      };
      e.preventDefault(); // Prevent text selection while dragging
      e.stopPropagation();
  }, [position.x, position.y]);

  const handleMouseMoveDrag = useCallback((e) => {
      if (!isDragging) return;
      const deltaX = e.pageX - dragStartInfo.current.initialMouseX;
      const deltaY = e.pageY - dragStartInfo.current.initialMouseY;
      setPosition({
          x: dragStartInfo.current.initialPosX + deltaX,
          y: dragStartInfo.current.initialPosY + deltaY,
      });
  }, [isDragging]);

  const handleMouseUpDrag = useCallback(() => {
      if (isDragging) {
          setIsDragging(false);
      }
  }, [isDragging]);

  useEffect(() => {
    if (isResizing) {
      window.addEventListener('mousemove', handleMouseMoveResize);
      window.addEventListener('mouseup', handleMouseUpResize);
    } else if (isDragging) { // Add listeners for dragging
      window.addEventListener('mousemove', handleMouseMoveDrag);
      window.addEventListener('mouseup', handleMouseUpDrag);
    }

    return () => {
      window.removeEventListener('mousemove', handleMouseMoveResize);
      window.removeEventListener('mouseup', handleMouseUpResize);
      window.removeEventListener('mousemove', handleMouseMoveDrag); // Cleanup drag listeners
      window.removeEventListener('mouseup', handleMouseUpDrag);
    };
  }, [isResizing, handleMouseMoveResize, handleMouseUpResize, isDragging, handleMouseMoveDrag, handleMouseUpDrag]); // Add drag dependencies

  const handleCloseClick = (e) => {
      e.stopPropagation();
      if (onClose) {
          onClose();
      }
  };

  return (
    <div
      ref={viewerRef}
      className={`fixed z-20 select-none overflow-hidden border border-black shadow-lg ${isDragging ? 'cursor-grabbing' : ''}`} // Add grabbing cursor when dragging
      style={{
        width: `${size.width}px`,
        height: `${size.height}px`,
        top: `${position.y}px`, // Use state for position
        left: `${position.x}px`, // Use state for position
        cursor: 'default',
      }}
    >
      <div
        onMouseDown={handleMouseDownDrag} // Attach drag handler to the header
        className="flex h-6 w-full items-center justify-between border-b border-neutral-600 bg-[#e5e5e5] px-2 cursor-grab" // Add grab cursor
      >
        <Link
          to={link}
          onMouseDown={(e) => e.stopPropagation()}
          target="_blank"
          rel="noopener noreferrer"
        >
          <span
            className={`flex items-center space-x-1 text-xs text-neutral-800 transition duration-200 ease-out hover:text-neutral-600`}
          >
            <span>{fileName}</span>
            <ArrowUpRight size={14} />
          </span>
        </Link>
        <button
          onClick={handleCloseClick}
          className="p-0.5 font-mono text-neutral-500 transition-colors hover:bg-neutral-300 hover:text-neutral-700"
          aria-label="Close code viewer"
        >
            <IconX size={14} strokeWidth={3}/>
        </button>
      </div>
      <div className="h-[calc(100%-1.5rem)] w-full overflow-auto bg-[#f5f5f5] text-sm">
        {children}
      </div>
      <div
        ref={resizeHandleRef}
        onMouseDown={handleMouseDownResize}
        className="absolute bottom-0 right-0 h-4 w-4 cursor-nwse-resize bg-[#ffdd33] hover:bg-red-500/70"
        style={{ zIndex: 1 }}
      ></div>
    </div>
  );
}

export default Hero
