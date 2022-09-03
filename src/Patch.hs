module Patch (
  simplifyMinimalDiff,
  dropChangelog,
  isTrivialRebuildCommit,
  removeDiffContext
  )
where

import Common

simplifyMinimalDiff :: [String] -> [String]
simplifyMinimalDiff =
  maybeRemoveDiffGit . filterCommon
  where
    filterCommon =
      filter (not .
              -- FIXME a/ and b/
              matchPreds (map isPrefixOf ["--- ", "+++ ", "index ", "@@ -"]))

    maybeRemoveDiffGit ls =
      let gitDiffs = filter ("diff --git " `isPrefixOf`) ls in
        if length gitDiffs == 1
        then ls \\ gitDiffs
        else ls

-- adapted from flist in swish
matchPreds :: [a -> Bool] -> a -> Bool
matchPreds fs a = any ($ a) fs

-- adapted from cabal-rpm PackageUtils
dropChangelog :: [String] -> [String]
dropChangelog ls =
  if " %changelog" `elem` ls
  then
    let rest = (dropWhileEnd ("@@ " `isPrefixOf`) . dropWhileEnd (== " ") . takeWhile (/= " %changelog")) ls in
      if length rest > 2 then rest else []
  else ls

isTrivialRebuildCommit :: [String] -> Bool
isTrivialRebuildCommit ls =
  let nontrivial =
        (simplifyMinimalDiff . removeDiffContext . dropChangelog) ls
  in
    length nontrivial `elem` [0,2] &&
    all (matchPreds (map isPrefixOf ["-Release:", "+Release:"])) nontrivial

  -- not
  -- (any
  --    (not . matchPreds (map isPrefixOf ["-Release:", "+Release:"])) ls)
--    (all . matchPreds (map isPrefixOf ["-Release:", "+Release:"])) ls

removeDiffContext :: [String] -> [String]
removeDiffContext = filter ((/= ' ') . head)
