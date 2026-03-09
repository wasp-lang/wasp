---
comments: true
last_checked_with_versions:
  Wasp: "0.21"
  multer: "2.1.1"
---

# File Uploads

This guide shows you how to implement file uploads in your Wasp application using [Multer](https://github.com/expressjs/multer).

## Setting up File Uploads

### 1. Install Multer

Install the Multer package and its types:

```bash
npm install multer
npm install --save-dev @types/multer
```

### 2. Define the API endpoint in main.wasp

Create an API namespace with middleware configuration and the upload endpoint:

```wasp title="main.wasp"
app FileUpload {
  wasp: {
    version: "^0.21.0"
  },
  title: "file-upload",
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage"
}

// highlight-start
apiNamespace fileUploadMiddleware {
  middlewareConfigFn: import { addMiddleware } from "@src/apis",
  path: "/api/upload"
}
// highlight-end

// highlight-start
api fileUpload {
  httpRoute: (POST, "/api/upload"),
  fn: import { uploadFile } from "@src/apis",
  entities: []
}
// highlight-end
```

### 3. Create the API handlers

Create the middleware configuration and upload handler:

```ts title="src/apis.ts" auto-js
import type { MiddlewareConfigFn } from "wasp/server";
import type { FileUpload } from "wasp/server/api";
import multer from "multer";

const upload = multer({ dest: "uploads/" });

export const addMiddleware: MiddlewareConfigFn = (config) => {
  config.set("multer", upload.single("file"));
  return config;
};

export const uploadFile: FileUpload = (req, res) => {
  console.log(req.body);
  console.log(req.file);
  const file = req.file!;
  return res.json({
    fileExists: !!file,
  });
};
```

### 4. Create the upload form

Create a form component to handle file uploads:

```tsx title="src/MainPage.tsx" auto-js
import { useState } from "react";
import { api } from "wasp/client/api";

export const MainPage = () => {
  const [name, setName] = useState("");
  const [file, setFile] = useState<File>();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!file) return;
    const formData = new FormData();
    formData.append("name", name);
    formData.append("file", file);
    const { data } = await api.post("/api/upload", formData);
    alert(JSON.stringify(data, null, 2));
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="text"
        placeholder="Name"
        value={name}
        onChange={(e) => setName(e.target.value)}
      />
      <input type="file" onChange={(e) => setFile(e.target.files?.[0])} />
      <button type="submit">Upload</button>
    </form>
  );
};
```

## Customizing Upload Settings

### Change upload destination

You can customize where files are stored:

```ts auto-js
const upload = multer({ dest: "my-custom-uploads/" });
```

### Limit file size

Add file size limits:

```ts auto-js
const upload = multer({
  dest: "uploads/",
  limits: {
    fileSize: 5 * 1024 * 1024, // 5MB limit
  },
});
```

### Filter file types

Only accept certain file types:

```ts auto-js
const upload = multer({
  dest: "uploads/",
  fileFilter: (req, file, cb) => {
    if (file.mimetype.startsWith("image/")) {
      cb(null, true);
    } else {
      cb(new Error("Only images are allowed"));
    }
  },
});
```

### Handle multiple files

To handle multiple file uploads:

```ts auto-js
export const addMiddleware: MiddlewareConfigFn = (config) => {
  config.set("multer", upload.array("files", 10)); // Max 10 files
  return config;
};
```

For more options, see the [Multer documentation](https://github.com/expressjs/multer).
