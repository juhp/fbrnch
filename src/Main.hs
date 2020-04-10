{-# LANGUAGE CPP #-}

import Distribution.Fedora.Branch
import SimpleCmd
import SimpleCmdArgs

import Control.Monad
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Options.Applicative (maybeReader)
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

-- commands
import Cmd.Build
import Cmd.Import
import Cmd.Merge
import Cmd.PkgReview
import Cmd.Pull
import Cmd.RequestBranch
import Cmd.RequestRepo
import Cmd.Reviews
import Cmd.Status

import Bugzilla (testBZlogin)
import ListReviews
import Types

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  activeBranches <- getFedoraBranches
  dispatchCmd activeBranches

dispatchCmd :: [Branch] -> IO ()
dispatchCmd activeBranches =
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "create-review" "Create a Package Review request" $
      createReview <$> noScratchBuild <*> optional (strArg "SPECFILE")
    , Subcommand "update-review" "Update a Package Review" $
      updateReview <$> noScratchBuild <*> optional (strArg "SPECFILE")
    , Subcommand "reviews" "List package reviews" $
      reviewsCmd <$> reviewStatusOpt
    , Subcommand "request-repos" "Request dist git repo for new approved packages" $
      requestRepos <$> many (strArg "NEWPACKAGE...")
    , Subcommand "import" "Import new approved created packages from bugzilla review" $
      importCmd <$> many (strArg "NEWPACKAGE...")
    , Subcommand "status" "Status package/branch status" $
      statusCmd <$> optional (optionWith branchM 'b' "branch" "BRANCH" "branch") <*> many pkgArg
    , Subcommand "merge" "Merge branches" $
      mergeCmd <$> optional (optionWith branchM 't' "to" "BRANCH" "merge from newer branch") <*> some pkgArg
    , Subcommand "build" "Build package(s)" $
      buildCmd <$> mergeOpt <*> optional scratchOpt <*> targetOpt <*> optional (optionWith branchM 'b' "branch" "BRANCH" "branch") <*> some pkgArg
    , Subcommand "request-branches" "Request branches for approved created packages" $
      requestBranches <$> mockOpt <*> branchesRequestOpt
    , Subcommand "build-branch" "Build branch(s) of package" $
      buildBranches False <$> mergeOpt <*> optional scratchOpt <*> targetOpt <*> pkgOpt <*> some branchArg
    , Subcommand "pull" "Git pull packages" $
      pullPkgs <$> some (strArg "PACKAGE...")
    , Subcommand "find-review" "Find package review bug" $
      review <$> strArg "PACKAGE"
    , Subcommand "test-bz-token" "Check bugzilla login status" $
      pure testBZlogin
    ]
  where
    noScratchBuild = switchWith 'n' "no-scratch-build" "Skip Koji scratch build"

    reviewStatusOpt :: Parser ReviewStatus
    reviewStatusOpt =
      flagWith' ReviewUnApproved 'u' "unapproved" "Package reviews not yet approved" <|>
      flagWith' ReviewApproved 'a' "approved" "All open approved package reviews" <|>
      flagWith' ReviewWithoutRepoReq 'w' "without-request" "Approved package reviews without a repo request" <|>
      flagWith' ReviewRepoRequested 'r' "requested" "Approved package reviews with a pending repo request" <|>
      flagWith' ReviewRepoCreated 'c' "created" "Approved package reviews with a created repo" <|>
      flagWith ReviewAllOpen ReviewUnbranched 'b' "unbranched" "Approved created package reviews not yet branched"

    branchArg :: Parser Branch
    branchArg = argumentWith branchM "BRANCH.."

    branchM = maybeReader (readBranch activeBranches)

    scratchOpt :: Parser Scratch
    scratchOpt = flagWith' AllArches 's' "scratch" "Koji scratch test build" <|>
                 Arch <$> strOptionWith 'a' "arch" "ARCH" "Koji scratch test build"

    targetOpt :: Parser (Maybe String)
    targetOpt = optional (strOptionWith 't' "target" "TARGET" "Koji target")

    pkgArg :: Parser Package
    pkgArg = removeSuffix "/" <$> strArg "PACKAGE.."

    mergeOpt = switchWith 'm' "merge" "merge from newer branch"

    pkgOpt :: Parser (Maybe String)
    pkgOpt = optional (strOptionWith 'p' "package" "PKG" "package")

    branchesRequestOpt :: Parser BranchesRequest
    branchesRequestOpt = flagWith' AllReleases 'a' "all" "Request branches for all current releases [default latest 2]" <|> BranchesRequest <$> many branchArg

    mockOpt = switchWith 'm' "mock" "Do mock build to test branch"
