import fs from 'fs-extra'
import path from 'path'
import { globSync } from 'glob'
import fm from 'front-matter'

const SITE_ROOT = process.cwd()
const STATIC_DIR = path.join(SITE_ROOT, 'static/')
const DOCS_DIR = path.join(SITE_ROOT, 'docs/')
const BLOG_DIR = path.join(SITE_ROOT, 'blog/')
const GITHUB_RAW_BASE_URL = 'https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/' // Use the release branch
const WASP_BASE_URL = 'https://wasp.sh/'
const LLM_FULL_FILENAME = 'llms-full.txt'
const LLM_OVERVIEW_FILENAME = 'llms.txt'
const LLMS_TXT_FILE_PATH = path.join(STATIC_DIR, LLM_OVERVIEW_FILENAME)
const LLMS_FULL_TXT_FILE_PATH = path.join(STATIC_DIR, LLM_FULL_FILENAME)

generateFiles().catch((error) => {
  console.error('Unhandled error during LLM file generation:', error)
  process.exit(1)
})

async function generateFiles() {
  console.log('Starting LLM file generation...')
  let llmsTxtContent = initializeOverviewContent()
  let llmsFullTxtContent = ''

  try {
    const docsSidebarConfig = await getDocsPageSidebarConfig()
    const docProcessingResult = await processDocumentationFiles(docsSidebarConfig)

    llmsTxtContent += docProcessingResult.overviewDocsSection
    llmsFullTxtContent = docProcessingResult.fullConcatContent

    const blogSection = await processBlogFiles()
    if (blogSection) {
      llmsTxtContent += `\n${blogSection}\n`
    }

  } catch (error) {
    console.error('Error during content processing and gathering:', error)
  }

  await writeOutputFiles(llmsTxtContent, llmsFullTxtContent)

  console.log('🎉 LLM file generation complete.')
}

function initializeOverviewContent() {
  const crawlerPermissions = `# llms.txt for wasp.sh\n# This site allows LLMs to crawl and use its content.\n# Contact: info@wasp-lang.dev\nUser-Agent: *\nDisallow:\n\n`
  const introSummary =
    '# The Wasp Full-stack Framework\nWasp is a full-stack framework with batteries included for React, Node.js, and Prisma.\n\n'
  return `${crawlerPermissions}${introSummary}# Full Documentation\n- [Complete LLM-formatted Wasp Documentation](${WASP_BASE_URL}${LLM_FULL_FILENAME})\n\n# Individual documentation sections and guides:\n`
}

async function getDocsPageSidebarConfig() {
  const sidebarFilePath = path.join(SITE_ROOT, 'sidebars.js')
  const absoluteSidebarFilePath = path.resolve(sidebarFilePath)
  console.log(`Loading sidebar configuration from: ${absoluteSidebarFilePath}`)
  const sidebarsModule = await import(`file://${absoluteSidebarFilePath}`)
  const sidebarDocs = sidebarsModule.default?.docs || sidebarsModule.docs

  if (!sidebarDocs) {
    throw new Error('`docs` array not found in sidebars.js module')
  }
  return sidebarDocs
}

async function processDocumentationFiles(sidebarConfig) {
  const docInfoMap = new Map()
  let overviewDocsSection = ''
  let fullConcatContent = ''

  const orderedDocIds = getOrderedSidebarCategoryItems(sidebarConfig)
  console.log(
    `Found ${orderedDocIds.length} document IDs in sidebar order for processing.`
  )

  const sidebarOverviewStructure = getDocsSidebarCategoryStructure(sidebarConfig)

  console.log(`Gathering and processing source files from: ${DOCS_DIR}...`)
  const allGlobbedRelativeFilePaths = globSync('*/**/*.{md,mdx}', {
    cwd: DOCS_DIR,
    nodir: true,
    ignore: ['**/_*.md', '**/_*.mdx'],
  })

  const orderedDocsRelativePaths = new Map()
  allGlobbedRelativeFilePaths.forEach((filePath) => {
    // relativeFile is a path like 'tutorial/create.mdx' or 'data-model/entities/index.md'
    // So, for example:
    //   'data-model/entitie.md => 'data-model/entities'
    //   'tutorial/01-create.mdx' should become 'tutorial/create'
    const docIdWithoutExtAndIndex = filePath
      .replace(/\.(mdx|md)$/, '')
      .replace(/\/index$/, '');

    const pathSegments = docIdWithoutExtAndIndex.split('/');
    const lastSegment = pathSegments.pop(); // This will be the filename or last directory name
    
    // Remove leading "NN-" or "NN." from the filename part, e.g. "01-create" => "create"
    const cleanedLastSegment = lastSegment.replace(/^\d+[-.]/, ''); 
    
    let docId;
    if (pathSegments.length > 0) {
      docId = [...pathSegments, cleanedLastSegment].join('/');
    } else {
      docId = cleanedLastSegment;
    }

    if (!orderedDocsRelativePaths.has(docId)) {
      orderedDocsRelativePaths.set(docId, filePath);
    }
  })

  for (const docId of orderedDocIds) {
    let relativeDocPath = orderedDocsRelativePaths.get(docId)
    if (!relativeDocPath && orderedDocsRelativePaths.has(`${docId}/index`)) {
      relativeDocPath = orderedDocsRelativePaths.get(`${docId}/index`)
    }

    if (relativeDocPath) {
      const absolutePath = path.join(DOCS_DIR, relativeDocPath)
      try {
        const rawContent = await fs.readFile(absolutePath, 'utf8')
        const { attributes, body } = fm(rawContent)
        const title =
          attributes['title-llm'] ||
          attributes.title ||
          path.basename(absolutePath, path.extname(absolutePath))
        const processedBody = cleanDocContent(body)
        docInfoMap.set(docId, {
          title,
          processedBody,
          absolutePath,
          relativeDocPath,
        })
      } catch (fileReadError) {
        console.error(
          `Error reading or processing file for docId '${docId}' at ${absolutePath}:`,
          fileReadError
        )
      }
    } else {
      console.warn(
        `Document ID "${docId}" was not found in orderedDocIds or not processed because it is ignored.`
      )
    }
  }

  for (const category of sidebarOverviewStructure) {
    overviewDocsSection += `## ${category.categoryLabel}\n`
    for (const docId of category.docIds) {
      if (docInfoMap.has(docId)) {
        const info = docInfoMap.get(docId)
        const relativeToSiteForGithub = path
          .relative(SITE_ROOT, info.absolutePath)
          .replace(/\\/g, '/')
        const githubRawUrl = GITHUB_RAW_BASE_URL + relativeToSiteForGithub
        overviewDocsSection += `- [${info.title}](${githubRawUrl})\n`
      } else {
        // Warning already issued during docInfoMap population
      }
    }
  }

  let currentPathSegments = []
  for (const docId of orderedDocIds) {
    if (docInfoMap.has(docId)) {
      const info = docInfoMap.get(docId)
      const dirParts = path
        .dirname(info.relativeDocPath)
        .split(path.sep)
        .filter((p) => p && p !== '.')
      let newHeaders = ''
      let differingSegmentFound = false
      for (let i = 0; i < dirParts.length; i++) {
        if (
          i >= currentPathSegments.length ||
          dirParts[i] !== currentPathSegments[i]
        ) {
          differingSegmentFound = true
        }
        if (differingSegmentFound) {
          const segmentTitle = dirParts[i]
            .split('-')
            .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
            .join(' ')
          const headerLevel = i + 1
          newHeaders += `${'#'.repeat(headerLevel)} ${segmentTitle}\n\n`
        }
      }
      if (newHeaders) {
        fullConcatContent += newHeaders
      }
      currentPathSegments = dirParts
      const fileTitleLevel = dirParts.length + 1
      fullConcatContent += `${'#'.repeat(fileTitleLevel)} ${info.title}\n\n${
        info.processedBody
      }\n\n---\n\n`
    }
  }
  return { docInfoMap, overviewDocsSection, fullConcatContent }
}

function getOrderedSidebarCategoryItems(items) {
  let paths = []
  if (!items) return paths
  for (const item of items) {
    if (typeof item === 'string') {
      paths.push(item)
    } else if (item.type === 'category' && item.items) {
      paths = paths.concat(getOrderedSidebarCategoryItems(item.items))
    } else if (item.type === 'autogenerated') {
      console.warn(
        `Warning: 'autogenerated' sidebar type for dirName '${item.dirName}' might not be fully processed.`
      )
    }
  }
  return paths
}

/**
 * Returns an ordered structure of the sidebar categories 
 * and their docIds (relative paths w/out file extension, e.g. 'tutorial/create').
 * Used for generating the overview section of the LLM files.
 */
function getDocsSidebarCategoryStructure(sidebarTopLevelItems) {
  const structuredOverview = []
  const categoriesToIgnore = ['Miscellaneous']
  if (!sidebarTopLevelItems) return structuredOverview

  for (const topItem of sidebarTopLevelItems) {
    if (
      topItem.type === 'category' &&
      topItem.label &&
      topItem.items &&
      !categoriesToIgnore.includes(topItem.label)
    ) {
      const docIdsWithinCategory = getOrderedSidebarCategoryItems(topItem.items)
      if (docIdsWithinCategory.length > 0) {
        structuredOverview.push({
          categoryLabel: topItem.label,
          docIds: docIdsWithinCategory,
        })
      }
    }
  }
  return structuredOverview
}

async function processBlogFiles() {
  let blogSectionContent = `# Blogposts\n`
  const blogPostFiles = globSync('*.{md,mdx}', {
    cwd: BLOG_DIR,
    nodir: true,
    ignore: ['_*.md', '_*.mdx', 'authors.yml', 'components/**'],
  })

  const blogPostsData = []

  for (const file of blogPostFiles) {
    const dateMatch = file.match(/^(\d{4}-\d{2}-\d{2})-.*\.(mdx|md)$/)
    if (!dateMatch) {
      if (
        file !== 'authors.yml' &&
        !file.startsWith('_') &&
        !file.includes('components/')
      ) {
        console.warn(
          `Skipping file in web/blog, does not match YYYY-MM-DD-name.md(x) naming convention or is an ignored type: ${file}`
        )
      }
      continue
    }

    const dateString = dateMatch[1]
    const absoluteFilePath = path.join(BLOG_DIR, file)
    try {
      const rawContent = await fs.readFile(absoluteFilePath, 'utf8')
      const { attributes } = fm(rawContent)
      let title = attributes.title
      if (!title) {
        title = path.basename(file, path.extname(file))
        title = title.replace(/^\d{4}-\d{2}-\d{2}-/, '')
        title = title.replace(/-/g, ' ')
        title = title
          .split(' ')
          .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
          .join(' ')
      }

      const fileNoExt = file.replace(/\.(mdx|md)$/, '')
      const fileParts = fileNoExt.split('-')
      if (fileParts.length >= 4) {
        const year = fileParts[0]
        const month = fileParts[1]
        const day = fileParts[2]
        const slug = fileParts.slice(3).join('-')
        let blogPath = `${WASP_BASE_URL}blog/${year}/${month}/${day}/${slug}`
        blogPath = blogPath.replace(/\\/g, '/')
        blogPostsData.push({ title, dateString, linkPath: blogPath })
      } else {
        console.warn(
          `Skipping blog post ${file}, does not match YYYY-MM-DD-name.md(x) naming convention`
        )
      }
    } catch (fileReadError) {
      console.error(
        `Error reading or processing blog file ${absoluteFilePath}:`,
        fileReadError
      )
    }
  }

  blogPostsData.sort((a, b) => b.dateString.localeCompare(a.dateString))

  if (blogPostsData.length > 0) {
    for (const post of blogPostsData) {
      blogSectionContent += `- [${post.title}](${post.linkPath})\n`
    }
    return blogSectionContent.trim()
  } else {
    return ''
  }
}

async function writeOutputFiles(llmsTxtContent, llmsFullTxtContent) {
  console.log('Writing output files to static/ ...')
  try {
    await fs.writeFile(LLMS_TXT_FILE_PATH, llmsTxtContent.trim(), 'utf8')
    console.log(`Generated overview file: ${LLMS_TXT_FILE_PATH}`)

    await fs.writeFile(
      LLMS_FULL_TXT_FILE_PATH,
      llmsFullTxtContent.trim(),
      'utf8'
    )
    console.log(`Generated full concatenated file: ${LLMS_FULL_TXT_FILE_PATH}`)
  } catch (error) {
    console.error('Error writing output files:', error)
  }
}

function cleanDocContent(content) {
  if (!content) return ''

  const componentsToReplace = new Set()
  // NOTE: Not sure if this is needed, as LLMs can probably parse the components's meaning from the context.
  // Regex to capture imports from '@site/src/components/Tag'
  // Example: import MyTag from '...' -> MyTag
  // Example: import {Tag1, Tag2 as MyTag2} from '...' -> {Tag1, Tag2 as MyTag2}
  const importRegex =
    /^\s*import\s+(.+?)\s+from\s+['"]@site\/src\/components\/Tag['"]\s*;?\s*$/gm
  let importMatch

  // Helper to extract actual component names (including aliases)
  // Defined inside cleanContent to be self-contained
  function extractNames(specifier) {
    const names = []
    specifier = specifier.trim()
    if (specifier.startsWith('{') && specifier.endsWith('}')) {
      const inner = specifier.substring(1, specifier.length - 1).trim()
      if (inner) {
        inner.split(',').forEach((part) => {
          part = part.trim()
          if (part.includes(' as ')) {
            names.push(part.split(' as ')[1].trim())
          } else {
            names.push(part)
          }
        })
      }
    } else {
      // Default import
      names.push(specifier)
    }
    return names.filter(Boolean) // Filter out any empty names
  }

  // Scan the original content for these specific imports BEFORE any modifications
  // Global regex exec updates lastIndex, so use original content string for scanning.
  while ((importMatch = importRegex.exec(content)) !== null) {
    extractNames(importMatch[1]).forEach((name) =>
      componentsToReplace.add(name)
    )
  }
  importRegex.lastIndex = 0 // Reset regex for any potential future use within same scope if it were designed differently

  let cleaned = content // Start modifications from original content

  componentsToReplace.forEach((compName) => {
    // Regex to match <CompName ... /> (self-closing with optional attributes)
    // Example: <Internal />, <Internal prop="foo" />, <Internal/>
    const tagRegex = new RegExp(`<(?:${compName})(\\s+[^>]*)?\\s*/>`, 'g')
    cleaned = cleaned.replace(tagRegex, `${compName}!`)
  })

  // General import removal (removes all imports, including processed ones from @site/src/components/Tag)
  cleaned = cleaned.replace(/^import\s+.*(?:from\s+['"].*['"])?;?\s*$/gm, '')
  // Remove lines like {/* TODO: ... */}
  cleaned = cleaned.replace(/^\{\/\*.*\*\/\}\s*$/gm, '')
  // Remove lines like <!-- TODO: ... -->
  cleaned = cleaned.replace(/^<!--.*-->\s*$/gm, '')
  // Remove components that are not in the componentsToReplace set
  cleaned = cleaned.replace(/<[^>]+>|<\/[^>]+>/g, '')
  // Remove Emojis using specific ranges (less likely to remove digits)
  cleaned = cleaned.replace(
    /[\u{1F600}-\u{1F64F}]|[\u{1F300}-\u{1F5FF}]|[\u{1F680}-\u{1F6FF}]|[\u{1F1E0}-\u{1F1FF}]|[\u{2600}-\u{26FF}]|[\u{2700}-\u{27BF}]|[\u{FE00}-\u{FE0F}]|[\u{1F900}-\u{1F9FF}]|[\u{1FA70}-\u{1FAFF}]/gu,
    ''
  )
  cleaned = cleaned.replace(/\u00A0/g, ' ') // Replace non-breaking space with regular space
  // Remove Box Drawing Characters (single and double line)
  cleaned = cleaned.replace(/[│├└─╔═╗║╚╝]/g, '')
  // Remove more than two line breaks
  cleaned = cleaned.replace(/\n{3,}/g, '\n\n')

  return cleaned.trim()
}
