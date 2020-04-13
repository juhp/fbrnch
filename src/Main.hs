import Common

import Distribution.Fedora.Branch
import Options.Applicative (maybeReader)
import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

-- commands
import Cmd.Bugs
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
  gitdir <- isGitDir "."
  dispatchCmd gitdir activeBranches

dispatchCmd :: Bool -> [Branch] -> IO ()
dispatchCmd gitdir activeBranches =
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "status" "Status package/branch status" $
      statusCmd <$> switchWith 'r' "reviews" "Status of reviewed packages" <*> branchesPackages
    , Subcommand "merge" "Merge from newer branch" $
      mergeCmd <$> branchesPackages
    , Subcommand "build" "Build package(s)" $
      buildCmd <$> mergeOpt <*> optional scratchOpt <*> targetOpt <*> branchesPackages
    , Subcommand "bugs" "List package bugs" $
      bugsCmd <$> optional (pkgArg "PACKAGE")
    , Subcommand "pull" "Git pull packages" $
      pullPkgs <$> some (pkgArg "PACKAGE...")
    , Subcommand "create-review" "Create a Package Review request" $
      createReview <$> noScratchBuild <*> optional (strArg "SPECFILE")
    , Subcommand "update-review" "Update a Package Review" $
      updateReview <$> noScratchBuild <*> optional (strArg "SPECFILE")
    , Subcommand "reviews" "List package reviews" $
      reviewsCmd <$> reviewStatusOpt
    , Subcommand "request-repos" "Request dist git repo for new approved packages" $
      requestRepos <$> many (pkgArg "NEWPACKAGE...")
    , Subcommand "import" "Import new approved created packages from bugzilla review" $
      importCmd <$> many (pkgArg "NEWPACKAGE...")
    , Subcommand "request-branches" "Request branches for approved created packages" $
      requestBranches <$> mockOpt <*> branchesRequestOpt
    , Subcommand "find-review" "Find package review bug" $
      review <$> pkgArg "PACKAGE"
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

    branchOpt :: Parser Branch
    branchOpt = optionWith branchM 'b' "branch" "BRANCH" "branch"

    branchArg :: Parser Branch
    branchArg = argumentWith branchM "BRANCH.."

    branchM = maybeReader (readBranch activeBranches)

    pkgArg :: String -> Parser Package
    pkgArg lbl = removeSuffix "/" <$> strArg lbl

    pkgOpt :: Parser String
    pkgOpt = removeSuffix "/" <$> strOptionWith 'p' "package" "PKG" "package"

    branchesPackages :: Parser ([Branch],[Package])
    branchesPackages = if gitdir then
      pair <$> (many branchOpt <|> many branchArg) <*> (pure [])
      else pair <$> many branchOpt <*> many (pkgArg "PACKAGE..") <|>
           pair <$> many branchArg <*> some pkgOpt

    pair a b = (a,b)

    branchesRequestOpt :: Parser BranchesRequest
    branchesRequestOpt = flagWith' AllReleases 'a' "all" "Request branches for all current releases [default latest 2]" <|> BranchesRequest <$> many branchArg

    scratchOpt :: Parser Scratch
    scratchOpt = flagWith' AllArches 's' "scratch" "Koji scratch test build" <|>
                 Arch <$> strOptionWith 'a' "arch" "ARCH" "Koji scratch test build"

    targetOpt :: Parser (Maybe String)
    targetOpt = optional (strOptionWith 't' "target" "TARGET" "Koji target")

    mergeOpt = switchWith 'm' "merge" "merge from newer branch"

    mockOpt = switchWith 'm' "mock" "Do mock build to test branch"
