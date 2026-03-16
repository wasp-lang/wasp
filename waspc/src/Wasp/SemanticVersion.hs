{-- This module describes the `SemanticVersion` public API --}
module Wasp.SemanticVersion
  ( module Wasp.SemanticVersion.Range,
    module Wasp.SemanticVersion.Version,
    module Wasp.SemanticVersion.VersionBound,
  )
where

import Wasp.SemanticVersion.Range
  ( Range (..),
    approximatelyEquivalentTo,
    backwardsCompatibleWith,
    doesVersionRangeAllowMajorChanges,
    eq,
    gt,
    gte,
    hyphenRange,
    isVersionInRange,
    lt,
    lte,
    parseRange,
    r,
    rangeParser,
    strictParseRange,
  )
import Wasp.SemanticVersion.Version
  ( Version (..),
    nextBreakingChangeVersion,
    parseVersion,
    v,
    versionParser,
  )
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds,
    VersionBound,
    VersionInterval,
    allVersionsInterval,
    intervalIntersection,
    intervalUnion,
    isSubintervalOf,
    isVersionInInterval,
    noVersionInterval,
    versionBounds,
    versionFromBound,
    vi,
  )

{--
# The `node-semver` implementation

We implement the `node-semver` package differently from the original implementation.
This comment explains:
- How does the `node-semver` behave?
- How can we implement this?
- Which one did we pick and why?

## How does the `node-semver` behave?

The `node-semver` package implements three classes: `SemVer`, `Comparator`, and `Range`:
- `SemvVer` class is an implementation of the semantic version.
- `Comparator` class composed of primitive operator (=, >, <, >=, <=) and a `SemVer`.
  Comparators allow us to compare two semantic versions.
- `Range` class composed of one or more comparator sets,
  where each comparator set is composed of one or more `Comparator`.
  `Range` is satisfied by the union of comparator sets it holds (logical OR),
  while the comparator set is satisfied by the intersection of all comparators it holds (logical AND).

This is the core domain of `node-semver`.
However, you may notice it's missing functionality that we often rely on,
like caret ranges (^1.2.3), tilde ranges (~1.2.3),
X-ranges (partial versions, e.g., 1.2), or hyphen ranges (1.2.3 - 1.2.3).

This is because the rest of the business logic is hidden in the parsing layer,
which desugars it into the three classes above.
E.g. "~1.2" ~~> ">=1.2.0 <1.3.0", desugars both the X-range and the tilde range.

All of this happens in the `Range` class parser.
It transforms the advanced range syntax (caret range, tilde range, X-range, hyphen range)
into primitive comparators parsable by the `Comparator` class.
(Advanced range syntax: https://github.com/npm/node-semver#advanced-range-syntax)

The bigger problem is that the parsing layer not only adds the syntax sugar,
but it also adds some additional restrictions to the valid `Range` format.
One restriction is that a hyphen range can't be combined with any other comparator in a comparator set.
So a range is either a hyphen range or a set of primitive (comparators), tilde, and caret ranges.
Even worse, this behavior is only recorded in the source code and the grammar.
(Grammar: https://github.com/npm/node-semver#range-grammar)

This is a recurring issue with `node-semver`.
Their documentation is not really written as a specification,
which makes it hard to replicate the API behavior fully.

## How can we implement this?

So the important context is that `node-semver` has two separate domains:
1. The core domain - `SemVer`, `Comparator`, and `Range`.
2. The parsing/syntax/grammar domain - allows advanced range syntax, defines grammar restrictions.

We can either:

1) Copy the `node-semver` implementation fully.
   As in, try to keep both the public API and the internal implementation as
   identical as possible.

   Good:
   - Our API is identical to the `node-semver` API.
   Bad:
   - We have to bridge the gap between the two domains.
     E.g., how do we save the pre-desugared form of range syntax?
   - We can access advanced range syntax only through:
     - Parsing (passing strings).
     - Helper functions (transforming a `SemVer` to a `Range`).
       - How to combine X-range with caret range? Not possbile!

2) Merge the parsing/syntax/grammar domain into the core domain.
   Parsing follows the core domain and implements no business logic.

   Good:
   - The domain constraints are expressed in the models.
   - Advanced range syntax is directly available in the API.
   - The original input is easily preserved (except whitespaces and wildcard characters).
   Bad:
   - We partially deviate from the `node-semver` API:
     - `SemVer` and `Range` would behave the same.
     - The middle layer between the `SemVer` and `Range` would be implemented differently.
     - The middle layer is hidden from the public API.
     - In the end, we would only differentiate on the `Comparator`, which isn't that bad.

## Which one did we pick and why?

We picked the second option.

The second solution is easier. Separating domains forces us to either
lose the original syntax (e.g. showing ">=1.2.0 <1.3.0" instead of "~1.2")
or store redundant representations.

With a merged model, the types carry enough information to:
- Preserve the original syntax via `show` (expect whitespaces and wildcard characters).
- Enforce the domain restrictions (e.g., hyphen range shenanigans).
- Computing version intervals on demand via `versionBounds`.

The `versionBounds` function effectively acts as the desugaring step,
translating grammar constructs (tilde, caret, x-range, hyphen range)
into `Comparator`-based version intervals.

Finally, in practice, this deviation only affects the middle layer.
`Version` and `Range` (which are the most important) behave the same as
their `node-semver` counterparts on the outside.

Note(Franjo): The range grammar provided by the `node-semver` docs is NOT a correct
              implementation of the grammar that `node-semver` actually parses.
              It's missing:
               - Numerous whitespaces allowed by the `node-semver`.
               - Restriction to the maximum length of the version component (16 chars).
                 - E.g.: `new SemVer("1.2.33333333333333333")` throws an error due to having 17 characters in the patch component.
              Grammar: https://github.com/npm/node-semver#range-grammar
--}
