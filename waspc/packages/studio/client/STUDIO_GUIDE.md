# Wasp Studio - Complete Guide

## 🎯 Overview

**Wasp Studio** is an interactive graph visualization tool for exploring Wasp full-stack applications. It provides real-time visualization of your app's architecture with intelligent navigation, relationship analysis, and AI-powered insights.

## 🏗️ Architecture

```
Data Source → WebSocket → React Flow Graph + Detail Viewer
                           ↓
                    Visual Focus System
                           ↓
                    Three-Tier Highlighting
```

### Core Components

| Component | Purpose | File |
|-----------|---------|------|
| **App** | Root container, navbar, connection status | `App.tsx` |
| **Flow** | Graph renderer, layout engine (ELK), node selection | `Flow.tsx` |
| **DetailViewer** | Sidebar panel with node details and AI summary | `DetailViewer.tsx` |
| **Graph Nodes** | Entity, Query, Action, API, Route, Page, Job, App | `graph/*.tsx` |
| **Socket** | WebSocket connection to backend (port 4000) | `socket.ts` |

## ✨ Features Implemented

### 1. Interactive Graph Visualization
- ✅ Auto-layout using ELK algorithm
- ✅ Clickable nodes for selection
- ✅ Animated edges
- ✅ Pan and zoom controls
- ✅ Toggle sidebar panel

### 2. Three-Tier Visual Focus System
When you select a node:
- **Selected node**: 100% opacity + green border
- **Connected nodes**: 65% opacity (semi-highlighted)
- **Unrelated nodes**: 25% opacity (dimmed)

**How it works:**
- CSS classes: `.focus-mode`, `.selected`, `.connected`
- Smooth 0.3s transitions
- Connected nodes calculated via edge traversal

**Customization:** Edit `index.css` lines 16-30

### 3. Comprehensive Detail Viewer

Shows type-specific information for each node type:

| Node Type | Shows | Status |
|-----------|-------|--------|
| **Entity** | Schema fields, relations, auth indicator | ⚠️ Placeholders |
| **Query/Action** | Function signature, parameters, return types, auth | ⚠️ Placeholders |
| **API** | HTTP method, endpoint path | ✅ Real data |
| **Route** | URL path, connected page | ✅ Real data |
| **Page** | Auth requirements | ✅ Real data |
| **Job** | Cron schedule | ✅ Real data |
| **App** | Database, auth config, statistics | ✅ Real data |

**All nodes show:**
- Relationships (Uses/Used By)
- Impact metrics (dependency counts)
- Clickable navigation chips

### 4. AI Summary (Claude Haiku Integration)

- ✅ Frontend UI complete
- ✅ Context preparation
- ✅ Loading/error/success states
- ⚠️ Backend endpoint needed
- ⚠️ Anthropic API integration needed

## 📋 TODO Lists

### Entity Details - Prisma Schema Integration

**Current:** Placeholder fields shown  
**Goal:** Display actual Prisma schema

```typescript
// DetailViewer.tsx - EntityDetails component
```

**TODO:**
- [ ] Parse `schema.prisma` file on backend
- [ ] Add `prismaFields` to entity data type:
  ```typescript
  entity: {
    name: string;
    prismaFields: Array<{
      name: string;
      type: string;
      attributes: string[];
      isRequired: boolean;
      defaultValue?: string;
    }>;
    relations: Array<{
      name: string;
      targetEntity: string;
      relationType: "one-to-one" | "one-to-many" | "many-to-many";
    }>;
  }
  ```
- [ ] Update `EntityDetails` to render real data
- [ ] Remove placeholder markers

**Backend work:** 
- Use Prisma's DMMF (Data Model Meta Format) to parse schema
- Send field definitions in WebSocket data

---

### Operation Details - TypeScript Signature Parsing

**Current:** Generic placeholder signatures  
**Goal:** Display actual function parameters and return types

```typescript
// DetailViewer.tsx - OperationDetails component
```

**TODO:**
- [ ] Parse TypeScript AST for operation files
- [ ] Add `signature` to operation data type:
  ```typescript
  operation: {
    name: string;
    type: "query" | "action";
    signature: {
      parameters: Array<{
        name: string;
        type: string;
        required: boolean;
        description?: string;
      }>;
      returnType: string;
    };
    auth: boolean;
  }
  ```
- [ ] Update `OperationDetails` to render real signatures
- [ ] Add syntax highlighting for types
- [ ] Remove placeholder markers

**Backend work:**
- Use TypeScript Compiler API to parse operation files
- Extract function signatures and JSDoc comments

---

### File Location Resolution

**Current:** Placeholder paths (`src/server/operations.ts:42`)  
**Goal:** Display actual file paths with line numbers

```typescript
// DetailViewer.tsx - InfoRow component (Header section)
```

**TODO:**
- [ ] Add source location tracking to backend
- [ ] Add `location` to all node data types:
  ```typescript
  location: {
    file: string;      // relative path from project root
    line: number;      // declaration line number
    column?: number;   // optional column number
  }
  ```
- [ ] Update header InfoRow to show real location
- [ ] Add "Jump to Code" button (opens in IDE)
- [ ] Remove placeholder marker

**Backend work:**
- Track source locations during Wasp compilation
- Store file paths and line numbers for each declaration

---

### AI Summary - Claude Haiku Backend

**Current:** Frontend complete with placeholder API  
**Goal:** Real AI-powered summaries

```typescript
// DetailViewer.tsx - fetchAISummary function
```

**TODO:**
- [ ] Install Anthropic SDK: `npm install @anthropic-ai/sdk`
- [ ] Set environment variable: `ANTHROPIC_API_KEY`
- [ ] Create backend API endpoint:
  ```typescript
  // src/server/aiSummary.ts
  import Anthropic from '@anthropic-ai/sdk';
  
  export async function generateAISummary(context: string) {
    const anthropic = new Anthropic({
      apiKey: process.env.ANTHROPIC_API_KEY,
    });
    
    const message = await anthropic.messages.create({
      model: 'claude-haiku-4.5',
      max_tokens: 300,
      messages: [{ role: 'user', content: context }]
    });
    
    return message.content[0].text;
  }
  ```
- [ ] Add Wasp API route in `main.wasp`:
  ```wasp
  api aiSummary {
    fn: import { generateAISummary } from "@server/aiSummary",
    httpRoute: (POST, "/api/ai-summary")
  }
  ```
- [ ] Update frontend `fetchAISummary()` to call real endpoint
- [ ] Add rate limiting (10 requests per 15 min)
- [ ] Add response caching (24 hours)
- [ ] Add cost tracking

**Cost:** ~$0.38 per 1000 summaries

---

### Component File Locations (Pages)

**Current:** Placeholder message  
**Goal:** Show React component file path

```typescript
// DetailViewer.tsx - PageDetails component
```

**TODO:**
- [ ] Track component file paths in Wasp compiler
- [ ] Add `componentFile` to page data:
  ```typescript
  page: {
    name: string;
    authRequired: boolean;
    componentFile: string;  // e.g., "src/client/pages/MainPage.tsx"
  }
  ```
- [ ] Update `PageDetails` to show real path
- [ ] Remove placeholder

---

### Job Execution Details

**Current:** Placeholder message  
**Goal:** Show job implementation details

```typescript
// DetailViewer.tsx - JobDetails component
```

**TODO:**
- [ ] Add job metadata to data type:
  ```typescript
  job: {
    name: string;
    schedule: string;
    executionFile: string;      // e.g., "src/server/jobs/cleanupJob.ts"
    lastRun?: Date;             // if tracking execution
    nextRun?: Date;             // calculated from schedule
  }
  ```
- [ ] Update `JobDetails` to show execution info
- [ ] Add cron schedule parser for human-readable format
- [ ] Remove placeholder

---

### API Request/Response Types

**Current:** Placeholder message  
**Goal:** Show TypeScript types for API endpoints

```typescript
// DetailViewer.tsx - ApiDetails component
```

**TODO:**
- [ ] Parse TypeScript types for API handlers
- [ ] Add type information to API data:
  ```typescript
  api: {
    name: string;
    httpRoute: { method: string; path: string };
    requestType?: string;      // TypeScript type for request body
    responseType?: string;     // TypeScript type for response
  }
  ```
- [ ] Update `ApiDetails` to show types
- [ ] Add syntax highlighting
- [ ] Remove placeholder

---

## 🎨 Customization Guide

### Adjust Visual Focus Opacity

**File:** `src/index.css`

```css
/* Line 29: Connected nodes opacity (currently 65%) */
.focus-mode .react-flow__node.connected {
  opacity: 0.65 !important;  /* Change: 0.5-0.9 */
}

/* Line 20: Dimmed nodes opacity (currently 25%) */
.focus-mode .react-flow__node:not(.selected):not(.connected) {
  opacity: 0.25;  /* Change: 0.1-0.4 */
}
```

### Change Selection Color

**File:** `src/index.css`

```css
/* Line 35: Selected node border (currently green) */
.react-flow__node.selected {
  box-shadow: 0 0 0 3px #10b981 !important;  /* Green */
}

/* Try:
  #3b82f6  Blue
  #a855f7  Purple  
  #f59e0b  Amber
*/
```

### Adjust Panel Width

**File:** `src/Flow.tsx` line 341

```typescript
width: "400px"  // Change: 300-600px
```

### Customize AI Summary Prompt

**File:** `DetailViewer.tsx` line 334 (backend)

Modify the prompt sent to Claude to focus on specific aspects:
- Security implications
- Performance considerations
- Architectural patterns
- Beginner-friendly explanations

## 🔌 Backend Data Contract

### Current Data Structure

```typescript
type Data = {
  entities: { name: string }[];
  operations: {
    entities: { name: string }[];
    name: string;
    type: "query" | "action";
    auth: string;
  }[];
  apis: {
    entities: { name: string }[];
    httpRoute: { method: string; path: string };
    name: string;
    auth: string;
  }[];
  jobs: {
    schedule: string;
    entities: { name: string }[];
    name: string;
  }[];
  pages: {
    authRequired: string;
    name: string;
  }[];
  routes: {
    name: string;
    toPage: { name: string };
    path: string;
  }[];
  app: {
    name: string;
    auth: {
      userEntity: { name: string };
      methods: string[];
    };
    db: { system: string };
  };
}
```

### Enhanced Data Structure (Target)

```typescript
type EnhancedData = {
  entities: {
    name: string;
    location: FileLocation;           // NEW
    prismaFields: PrismaField[];      // NEW
    relations: PrismaRelation[];      // NEW
  }[];
  operations: {
    entities: { name: string }[];
    name: string;
    type: "query" | "action";
    auth: string;
    location: FileLocation;           // NEW
    signature: FunctionSignature;     // NEW
  }[];
  apis: {
    entities: { name: string }[];
    httpRoute: { method: string; path: string };
    name: string;
    auth: string;
    location: FileLocation;           // NEW
    requestType?: string;             // NEW
    responseType?: string;            // NEW
  }[];
  jobs: {
    schedule: string;
    entities: { name: string }[];
    name: string;
    location: FileLocation;           // NEW
    nextRun?: Date;                   // NEW
  }[];
  pages: {
    authRequired: string;
    name: string;
    location: FileLocation;           // NEW
    componentFile: string;            // NEW
  }[];
  routes: {
    name: string;
    toPage: { name: string };
    path: string;
  }[];
  app: {
    name: string;
    auth: {
      userEntity: { name: string };
      methods: string[];
    };
    db: { system: string };
  };
};

// Helper types
type FileLocation = {
  file: string;
  line: number;
  column?: number;
};

type PrismaField = {
  name: string;
  type: string;
  attributes: string[];
  isRequired: boolean;
  defaultValue?: string;
};

type PrismaRelation = {
  name: string;
  targetEntity: string;
  relationType: "one-to-one" | "one-to-many" | "many-to-many";
};

type FunctionSignature = {
  parameters: Array<{
    name: string;
    type: string;
    required: boolean;
    description?: string;
  }>;
  returnType: string;
};
```

## 🚀 Quick Start for Contributors

### Setup
```bash
cd waspc/packages/studio/client
npm install
npm run dev
```

### Key Files to Know
- `Flow.tsx` - Graph logic, selection, layout
- `DetailViewer.tsx` - Sidebar panel with all details
- `index.css` - Visual focus system styles
- `graph/*.tsx` - Individual node components

### Making Changes

**1. Add new node data:**
- Update `types.ts` with new fields
- Update backend to send new data
- Update DetailViewer component to display it

**2. Adjust visual styling:**
- Edit `index.css` for focus system
- Edit node components in `graph/` for node appearance

**3. Add new features:**
- Follow existing component patterns
- Update this guide with new TODOs
- Add documentation comments

## 📊 Feature Completion Status

| Feature | Status | Priority |
|---------|--------|----------|
| Graph Visualization | ✅ Complete | - |
| Three-Tier Focus | ✅ Complete | - |
| Basic Detail Viewer | ✅ Complete | - |
| Relationship Navigation | ✅ Complete | - |
| AI Summary UI | ✅ Complete | High |
| Prisma Schema Display | ⚠️ Placeholder | High |
| TypeScript Signatures | ⚠️ Placeholder | High |
| File Locations | ⚠️ Placeholder | Medium |
| AI Backend Integration | ⚠️ Not Started | Medium |
| Component File Paths | ⚠️ Placeholder | Low |
| Job Execution Details | ⚠️ Placeholder | Low |
| API Type Information | ⚠️ Placeholder | Low |

## 🎯 Implementation Priority

### Phase 1: High-Value Data (Recommend First)
1. **File Locations** - Easy win, very useful for "jump to code"
2. **Prisma Schema** - Most requested, shows real entity structure
3. **TypeScript Signatures** - Critical for understanding operations

### Phase 2: AI Enhancement
4. **AI Summary Backend** - Provides immediate value, low cost

### Phase 3: Nice-to-Haves
5. **Component File Paths** - Useful but less critical
6. **Job Execution Details** - Nice for monitoring
7. **API Type Information** - Helpful for API consumers

## 🐛 Known Issues & Limitations

1. **No multi-level relationship trails** - Only shows direct connections
2. **No field-level entity dependencies** - Shows entity-level only
3. **No performance metrics** - Could integrate query timing data
4. **No test coverage indicators** - Could show which nodes have tests
5. **Static layout only** - No manual node repositioning (by design)

## 📝 Development Notes

### Adding Placeholder Data
When adding new placeholders, follow this pattern:
```typescript
<InfoRow
  label="Your Field"
  value="Placeholder value"
  placeholder  // Marks as placeholder (italic, dimmed)
/>
```

### Removing Placeholders
1. Update backend to send real data
2. Update TypeScript types in `types.ts`
3. Update component to use real data
4. Remove `placeholder` prop
5. Update this guide's TODO list

### Testing Locally
- Mock backend can be created by editing `.wasp-studio-data.json`
- Useful for testing UI without backend changes
- WebSocket connection will use this static data

## 📚 Additional Resources

- **Wasp Docs**: https://wasp-lang.dev/docs
- **ReactFlow Docs**: https://reactflow.dev/
- **ELK Layout**: https://www.eclipse.org/elk/
- **Anthropic API**: https://docs.anthropic.com/
- **Prisma DMMF**: https://www.prisma.io/docs/concepts/components/prisma-schema/data-model

---

## Quick Reference

### Commands
```bash
npm run dev      # Start dev server
npm run build    # Build for production
npm run copy     # Build + copy to parent public/
npm run lint     # Run ESLint
```

### Environment Variables
```bash
ANTHROPIC_API_KEY=sk-ant-...  # For AI summary (backend only)
```

### File Structure
```
client/
├── src/
│   ├── App.tsx              # Root component
│   ├── Flow.tsx             # Graph visualization
│   ├── DetailViewer.tsx     # Sidebar panel (771 lines)
│   ├── socket.ts            # WebSocket connection
│   ├── types.ts             # TypeScript types
│   ├── index.css            # Global styles + focus system
│   └── graph/               # Node components
│       ├── Entity.tsx
│       ├── Operation.tsx
│       ├── Api.tsx
│       ├── Route.tsx
│       ├── Page.tsx
│       ├── Job.tsx
│       ├── App.tsx
│       └── factories.ts     # Node/edge creators
├── STUDIO_GUIDE.md          # This file
└── [other .md files]        # Can be deleted
```

---

**Last Updated:** December 2024  
**Version:** 1.0  
**Contributors:** Add your name when you contribute!

