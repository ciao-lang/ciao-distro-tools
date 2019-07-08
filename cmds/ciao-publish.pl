% Copyright (C) 2016-2019 Jose F. Morales

:- module(_, [], [assertions, doccomments]).

%! \title Export from Ciao mono-repository
%
%  \module
%  This is a tool to publish pristine views of an "annotated" git
%  mono-repository.
%  
%  An annotated git repository contains NODISTRIBUTE empty files that mark
%  directories that must not be visible in a pristine copy.
%  
%  This script does not need a checkout (it works directly on the git
%  repository).
%  
%  This script is tailored for Ciao but it could be easily generalized:
%   - assumes the 'ciao-devel' structure
%   - keeps clones of publishing repos at "$HOME"/REPOS-ciao-publish
%  
%  ## Publishing code at Github
%  
%  Use `ciao-publish list` to enumerate publishable bundles from the
%  Ciao devel repository (either the main `ciao` or some bundles at
%  `bndls/`). E.g.:
%  ```
%  $ ciao-publish list
%  ```
%  
%  Publishing code is performed by the `publish` and `push`
%  operations. Use the `help` option to view other possible
%  commmands. Use the `--bundle BNDL` option to select the bundle to
%  publish (`ciao` by default).
%  
%  Example for publishing `ciao` (the special root bundle that
%  contains containing `core`, `builder`, etc.):
%  ```
%  $ ciao-publish pull
%  $ ciao-publish publish
%  # (please review commits)
%  $ ciao-publish push
%  ```
%  
%  Example for publishing `ciao_emacs`:
%  ```
%  $ ciao-publish --bundle ciao_emacs pull
%  $ ciao-publish --bundle ciao_emacs publish
%  # (please review commits)
%  $ ciao-publish --bundle ciao_emacs push
%  ```
%  
%  The first time that `publish` is performed, it may complain with a `sed:
%  first RE may not be empty` error. Fix it by specifying a first commit
%  number, or do `squash` instead of `publish`.
%  
%  ## Working on multiple bundles
%  
%  You can use the `--all` option to perform a command on all publishable
%  bundles (see `--help` for more info), or use shell scripts.
%  
%  Examples on all bundles:
%  ```
%  # Consult the status of all publishable bundles
%  $ ciao-publish --all status
%  
%  # Pull all public repositories (previous to publishing)
%  $ ciao-publish --all pull
%  
%  # Publish all publishable bundles
%  $ ciao-publish --all publish
%  
%  # Push to the public repository
%  $ ciao-publish --all push
%  ```
%  
%  Examples of scripts:
%  ```
%  # Consult the status of all publishable bundles
%  $ for i in `ciao-publish list`; do ciao-publish --bundle $i status; echo; done
%  
%  # Pull all public repositories (previous to publishing)
%  $ for i in `ciao-publish list`; do ciao-publish --bundle $i pull; done
%  
%  # Publish all publishable bundles
%  $ for i in `ciao-publish list`; do echo "PUBLISH $i? (C-c to abort)"; read; ciao-publish --bundle $i publish; done
%  
%  # Push to the public repository
%  $ for i in `ciao-publish list`; do ciao-publish --bundle $i push; done
%  ```
%  
%  ## Additional files for publishing
%  
%  Scripts for CI are located at:
%  ```
%  ci-scripts/
%  ```
%  
%  Scripts for checking the status of CI are located at:
%  ```
%  ci-status/
%  ```
%  
%  ## Consulting statistics
%  
%  For download statistics:
%  ```
%  $ ./ciao-download-stats.sh
%  ```
%  
%  ## Contribution from public repositories
%  
%  Assume that Bob maintains the private Ciao repository and Alice wants
%  to contribute to it. This is the typical workflow for contributing to
%  the project preserving authorship:
%  
%    - Alice: clone public Git repository (`github.com/ciao-lang/ciao`)
%    - Alice: make some changes and commit them
%    - Alice: format patches using `git format-patch` (never push them!)
%  ```
%  $ git format-patch HEAD^n # where n is the number of commits not in master
%  ```
%  
%    - Alice: send the `????-*.patch` files via email to Bob
%    - Bob: apply back the changes in the private repository with
%  
%  ```
%  $ git am < 0001-*.patch
%  $ git am < 0002-*.patch
%  $ ...
%  ```
%  
%    - Bob: check with `git log` that the code is pushed correctly
%    - Bob: do `ciao-publish.sh pull`, `ciao-publish.sh publish`,
%      and `ciao-publish.sh push`
%  
%    - Alice: do `git pull` and `git rebase -i`. The first command will
%      create a merge commit. The second command will break. After some
%      `git rebase --continue` it will end. If everything is fine there
%      will be no unpushed changes to master and all new commits will be
%      authored as Alice.
%  
%  ## Pointers to code and distribution files
%  
%  Source repositories:
%  
%    - **Local repositories**: run on a terminal "`ssh gitolite\@ciao-lang.org`"
%    - **Public repositories** at github: @href{https://github.com/ciao-lang}
%  
%  Binaries:
%  
%    - **Continuous integration** on Travis-CI and AppVeyor:
%       - @href{https://github.com/ciao-lang/ciao}
%       - @href{https://github.com/ciao-lang/ciao.CD}
%       - @href{https://ci.appveyor.com/project/jfmc/ciao}
%       - @href{https://ci.appveyor.com/project/jfmc/ciao.CD}
%  
%    - **Binaries** at Bintray: [log in with your Github account]:
%       - @href{https://bintray.com/ciao-lang/builds/ciao}
%       - @href{https://dl.bintray.com/ciao-lang/builds}
%  
%    - **Homebrew** formula (macOS): @href{https://github.com/ciao-lang/homebrew-ciao}
%  
%    - **Docker** image (with pointers to docker hub account): @href{https://github.com/ciao-lang/docker-ciao}
%  
%  Other binaries (not ready):
%  	 
%    - **Launchpad (Ubuntu)** account for Ciao team: @href{https://launchpad.net/~ciao-lang}
%      [Created with `jfmc` user, ask for membership]

:- use_module(library(process)).
:- use_module(library(stream_utils), [write_string/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

:- export(main/1).
main([H]) :-
	( H = help ; H = '-h' ; H = '--help' ),
	!,
	show_help.
main(Args) :-
	bundle_path('ciao-distro-tools', 'cmds/ciao-publish.sh', Cmd),
	process_call(Cmd, Args, [status(S)]),
	halt(S).

show_help :-
	write_string("Usage: ciao-publish.sh [OPTS] CMD

Publish the current git repository through another (public) repository.

Options:

  --bundle Bundle
                   Select bundle Bundle. Use 'ciao' as the special root
		   bundle (containing core, builder, etc.)
  --all            Select all bundles
  --pubrepos DIR   Select directory containing the publishing repository
                   clones (~/REPOS-ciao-publish by default)

Available commands:

  help             Show this message
  list             Show publishable bundles
  init             Create the remote repository for publishing
  info             Show publishing info about a bundle
  pull             Pull (public repo)
  publish [Id]     Update (git add -A) from the latest or given commit
                   (for each unpublished commit)
  squash [Id]      Like publish, but squash into a single commit
                   (more efficient than rebase but loses history)
  dry-squash [Id]  Simulate squash (do not commit, leave the tree in a temporary dir)
  status           Show status (unpublished commits)
  push [args]      Push (public repo)
  rebase [args]    Rebase (public repo)

Use the --bundle option to select a bundle. If not specified, the
bundle is automatically detected from the working directory (this
assumes that we have an updated checkout).

Use --all to work on all publishable bundles (only for pull, publish,
push, status).

").

