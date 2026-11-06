module Wasp.Generator.DepVersions
  ( prismaVersionRange,
    superjsonVersionRange,
    typescriptVersionRange,
    reactVersionRange,
    reactDomVersionRange,
    reactTypesVersionRange,
    reactDomTypesVersionRange,
    reactRouterVersionRange,
    reactQueryVersionRange,
    expressVersionRange,
    expressTypesVersionRange,
    viteVersionRange,
    dotenvVersionRange,
    kyVersionRange,
    nodeTypesVersionRange,
  )
where

import Wasp.Generator.DepVersions.Catalog (catalogVersionRange)
import qualified Wasp.SemanticVersion as SV

prismaVersionRange :: SV.Range
prismaVersionRange = $$(catalogVersionRange "prisma")

superjsonVersionRange :: SV.Range
superjsonVersionRange = $$(catalogVersionRange "superjson")

typescriptVersionRange :: SV.Range
typescriptVersionRange = $$(catalogVersionRange "typescript")

reactVersionRange :: SV.Range
reactVersionRange = $$(catalogVersionRange "react")

reactDomVersionRange :: SV.Range
reactDomVersionRange = $$(catalogVersionRange "react-dom")

reactTypesVersionRange :: SV.Range
reactTypesVersionRange = $$(catalogVersionRange "@types/react")

reactDomTypesVersionRange :: SV.Range
reactDomTypesVersionRange = $$(catalogVersionRange "@types/react-dom")

reactRouterVersionRange :: SV.Range
reactRouterVersionRange = $$(catalogVersionRange "react-router")

reactQueryVersionRange :: SV.Range
reactQueryVersionRange = $$(catalogVersionRange "@tanstack/react-query")

expressVersionRange :: SV.Range
expressVersionRange = $$(catalogVersionRange "express")

expressTypesVersionRange :: SV.Range
expressTypesVersionRange = $$(catalogVersionRange "@types/express")

viteVersionRange :: SV.Range
viteVersionRange = $$(catalogVersionRange "vite")

dotenvVersionRange :: SV.Range
dotenvVersionRange = $$(catalogVersionRange "dotenv")

kyVersionRange :: SV.Range
kyVersionRange = $$(catalogVersionRange "ky")

nodeTypesVersionRange :: SV.Range
nodeTypesVersionRange = $$(catalogVersionRange "@types/node")
