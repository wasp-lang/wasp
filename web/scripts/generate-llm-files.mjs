import fs from 'fs/promises'
import path from 'path'
import { globSync } from 'glob'
import fm from 'front-matter'
import docsSidebarConfig from '../sidebars.js'

const SITE_ROOT = process.cwd()
const STATIC_DIR = path.join(SITE_ROOT, 'static/')
const DOCS_DIR = path.join(SITE_ROOT, 'docs/')
const BLOG_DIR = path.join(SITE_ROOT, 'blog/')
const GITHUB_RAW_BASE_URL =
  'https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/' // Use the release branch
const WASP_BASE_URL = 'https://wasp.sh/'
const LLM_FULL_FILENAME = 'llms-full.txt'
const LLM_OVERVIEW_FILENAME = 'llms.txt'
const LLMS_TXT_FILE_PATH = path.join(STATIC_DIR, LLM_OVERVIEW_FILENAME)
const LLMS_FULL_TXT_FILE_PATH = path.join(STATIC_DIR, LLM_FULL_FILENAME)

const OVERVIEW_INTRO_CONTENT = `
# Wasp
Wasp is a full-stack framework with batteries included for React, Node.js, and Prisma.

## Individual documentation sections and guides:
`

await generateFiles()

/**
 * Main function to generate the LLM-friendly doc files.
 * It orchestrates the process by fetching and processing documentation and blog files,
 * and then writing the final output to the static directory.
 */
async function generateFiles() {
  console.log('Starting LLM file generation...')
  let llmsTxtContent = OVERVIEW_INTRO_CONTENT
  let llmsFullTxtContent = ''

  const docProcessingResult = await processDocumentationFiles(
    docsSidebarConfig.docs
  )

  llmsTxtContent += docProcessingResult.overviewDocsSection
  llmsFullTxtContent = docProcessingResult.fullConcatContent

  const blogSection = await processBlogFiles()
  if (blogSection) {
    llmsTxtContent += `\n${blogSection}\n\n`
  }

  llmsTxtContent += getMiscSectionContent()

  await writeOutputFiles(llmsTxtContent, llmsFullTxtContent)
  console.log('🎉 LLM file generation complete.')
}

/**
 * Processes all documentation files based on the sidebar configuration's order.
 * It builds a map of document IDs to file paths, processes each document,
 * and generates both a summarized overview and a full concatenated string of the content.
 * @param {object[]} sidebarConfig - The `docs` array from the sidebars.js configuration.
 * @returns {{overviewDocsSection: string, fullConcatContent: string}} - An object containing the generated overview section and the full concatenated content.
 */
async function processDocumentationFiles(sidebarConfig) {
  let overviewDocsSection = ''
  let fullConcatContent = ''

  const orderedDocIds = flattenSidebarItemsToDocIds(sidebarConfig)
  console.log(
    `Found ${orderedDocIds.length} document IDs in sidebar order for processing.`
  )

  const sidebarOverviewStructure =
    getDocsSidebarCategoryStructure(sidebarConfig)

  const docIdToPathMap = buildDocIdToPathMap(DOCS_DIR)

  const docInfoMap = await populateDocInfoMap(orderedDocIds, docIdToPathMap)

  for (const category of sidebarOverviewStructure) {
    overviewDocsSection += `${category.categoryLabel}\n`
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

  // Build fullConcatContent using sidebar structure with proper heading hierarchy
  for (const category of sidebarOverviewStructure) {
    // Add category header as H1 and separator
    fullConcatContent += `# ${category.categoryLabel}\n\n`

    for (const docId of category.docIds) {
      if (docInfoMap.has(docId)) {
        const info = docInfoMap.get(docId)
        // Add document title as H2
        fullConcatContent += `## ${info.title}\n\n${info.processedBody}\n\n`
      }
    }

    // Add category separator
    fullConcatContent += `------\n\n`
  }
  return { overviewDocsSection, fullConcatContent }
}

/**
 * Scans a directory for markdown files, normalizes their paths to create doc IDs,
 * and returns a map from each unique doc ID to its relative file path.
 * @param {string} directory - The directory to scan for documentation files (e.g., DOCS_DIR).
 * @returns {Map<string, string>} A map where keys are normalized doc IDs and values are the original file paths.
 */
function buildDocIdToPathMap(directory) {
  console.log(`Gathering and processing source files from: ${directory}...`)
  // Use **/*.{md,mdx} to match files at any depth, including directly under docs/
  const allGlobbedRelativeFilePaths = globSync('**/*.{md,mdx}', {
    cwd: directory,
    nodir: true,
    ignore: ['**/_*.md', '**/_*.mdx'],
  })

  const docsRelativePaths = new Map()
  for (const filePath of allGlobbedRelativeFilePaths) {
    const docId = normalizePathToDocId(filePath)
    if (!docsRelativePaths.has(docId)) {
      docsRelativePaths.set(docId, filePath)
    }
  }
  return docsRelativePaths
}

/**
 * Iterates through an ordered list of doc IDs, reads the corresponding file for each,
 * processes its content, and returns a map containing the processed information for each document.
 * This loop ensures that we process the files in the exact order specified in sidebars.js,
 * regardless of how the filesystem returns them.
 * @param {string[]} orderedDocIds - An array of doc IDs in the desired order.
 * @param {Map<string, string>} docIdToPathMap - A map from doc IDs to their file paths.
 * @returns {Promise<Map<string, object>>} A promise that resolves to a map where keys are doc IDs
 * and values are objects containing the title, processedBody, and path information.
 */
async function populateDocInfoMap(orderedDocIds, docIdToPathMap) {
  const docInfoMap = new Map()
  for (const docId of orderedDocIds) {
    const relativeDocPath = docIdToPathMap.get(docId)

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
        // This is intentionally not re-thrown to allow the script to continue
        // and generate a partial file, even if some source files have errors.
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
  return docInfoMap
}

/**
 * Normalizes a file path into a standardized document ID.
 * e.g., 'tutorial/01-create.mdx' becomes 'tutorial/create'
 * e.g., 'data-model/entities/index.md' becomes 'data-model/entities'
 * @param {string} filePath - The relative file path to normalize.
 * @returns {string} The normalized doc ID.
 */
function normalizePathToDocId(filePath) {
  // This function converts a file path like 'tutorial/01-create.mdx' into a docId like 'tutorial/create'
  const docIdWithoutExtAndIndex = filePath
    .replace(/\.(mdx|md)$/, '')
    .replace(/\/index$/, '')

  const pathSegments = docIdWithoutExtAndIndex.split('/')
  const lastSegment = pathSegments.pop() // This will be the filename or last directory name

  // Remove leading "NN-" or "NN." from the filename part, e.g. "01-create" => "create"
  const cleanedLastSegment = lastSegment.replace(/^\d+[-.]/, '')

  let docId
  if (pathSegments.length > 0) {
    docId = [...pathSegments, cleanedLastSegment].join('/')
  } else {
    docId = cleanedLastSegment
  }
  return docId
}

/**
 * Recursively traverses the sidebar configuration to produce a flat, ordered list of document IDs.
 * @param {object[]} items - An array of sidebar items (strings or objects).
 * @returns {string[]} A flat array of doc ID strings.
 */
function flattenSidebarItemsToDocIds(items) {
  let paths = []
  if (!items) return paths
  for (const item of items) {
    if (typeof item === 'string') {
      paths.push(item)
    } else if (item.type === 'category' && item.items) {
      paths = paths.concat(flattenSidebarItemsToDocIds(item.items))
    } else if (item.type === 'autogenerated') {
      console.warn(
        `Warning: 'autogenerated' sidebar type for dirName '${item.dirName}' might not be fully processed.`
      )
    }
  }
  return paths
}

/**
 * Returns an ordered structure of the sidebar categories and their docIds.
 * This is used for generating the overview section of the LLM files.
 * It ignores categories that are not relevant for the LLM context (e.g., 'Miscellaneous').
 * @param {object[]} sidebarTopLevelItems - The top-level items from the sidebar config.
 * @returns {object[]} A structured array of objects, each containing a categoryLabel and its array of docIds.
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
      const docIdsWithinCategory = flattenSidebarItemsToDocIds(topItem.items)
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

/**
 * Processes all blog post files, extracting their title, date, and URL.
 * It returns a markdown-formatted string of the blog posts, sorted by date.
 * @returns {Promise<string>} A promise that resolves to a markdown string listing the blog posts.
 */
async function processBlogFiles() {
  let blogSectionContent = `## Blogposts\n`
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
      // This is intentionally not re-thrown to allow the script to continue
      // and generate a partial file, even if some source files have errors.
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

/**
 * Writes the LLM-friendly content to their respective output files.
 * @param {string} llmsTxtContent - The content for the overview file (llms.txt).
 * @param {string} llmsFullTxtContent - The content for the full concatenated file (llms-full.txt).
 */
async function writeOutputFiles(llmsTxtContent, llmsFullTxtContent) {
  console.log('Writing output files to static/ ...')

  await fs.writeFile(LLMS_TXT_FILE_PATH, llmsTxtContent.trim(), 'utf8')
  console.log(`Generated overview file: ${LLMS_TXT_FILE_PATH}`)

  await fs.writeFile(LLMS_FULL_TXT_FILE_PATH, llmsFullTxtContent.trim(), 'utf8')
  console.log(`Generated full concatenated file: ${LLMS_FULL_TXT_FILE_PATH}`)
}

/**
 * Cleans the raw markdown content of a document to make it more suitable for an LLM.
 * This involves removing React/HTML components, import statements, comments, and other non-content elements.
 * It also adjusts heading levels to be consistent with the overall document structure.
 * @param {string} content - The raw markdown string.
 * @returns {string} The cleaned markdown string.
 */
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

  // Increase heading level by adding an extra # to any existing headings
  cleaned = cleaned.replace(/^(#{1,6})\s/gm, '#$1 ')

  return cleaned.trim()
}

function getMiscSectionContent() {
  return `## Miscellaneous\n- [Wasp Developer Discord](https://discord.com/invite/rzdnErX)\n- [Open SaaS -- Wasp's free, open-source SaaS boilerplate starter](https://opensaas.sh)`
}
