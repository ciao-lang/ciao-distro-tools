#!/bin/bash

# (Internal for ciao-publish)
#
# (c) 2016-2019 Jose F. Morales

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

set -e

# (not in OSX)
function realpath() {
    local d=`dirname "$1"`
    if ! [ -r "$d" ]; then
	printf "%s" "$1" # does not exists, get unmodified
	return
    fi
    d=`cd "$d"; pwd`
    local b=`basename "$1"`
    printf "%s" "$d"/"$b"
}

# Use 'tail -r' (mac) or 'tac' (linux) for reverse
if command -v tac > /dev/null 2>&1; then
    revtail=tac
else
    revtail="tail -r"
fi

# ---------------------------------------------------------------------------
# Bundles 

root_bundle="ciao" # Minimum system (not really a bundle!)

# List publishable bundles
function list_bundles() { # (needs init_src_config)
    local b
    # The root bundle
    echo "$root_bundle"
    # Go to bndls/ dir
    pushd "$srcgit" > /dev/null 2>&1
    cd ../bndls
    for b in *; do
	if [ -r "$b"/Manifest ] || [ -r "$b"/Manifest.pl ]; then
	    if [ ! -r "$b"/NODISTRIBUTE ]; then
		echo $b
	    fi
	fi
    done
    popd > /dev/null 2>&1
}

function detect_bundle() { # (needs init_src_config)
    # Locate root dir
    local rootdir
    pushd "$srcgit" > /dev/null 2>&1
    cd ..
    rootdir=`pwd`
    popd > /dev/null 2>&1
    # Check if we are in root or any bndls/ subdirectory
    local tmp curr
    tmp=`pwd`
    curr="${tmp#$rootdir/bndls/}"
    if [ "$tmp" != "$curr" ]; then # inside bndls/
	tmp=$curr
	while [ "$tmp" != "." ]; do
	    curr=$tmp
	    tmp=`dirname "$tmp"`
	done
	srcbundle="$curr"
    else
	srcbundle="$root_bundle" # The root bundle
    fi
}

# ---------------------------------------------------------------------------
# Configuration

function src_config_bundle() {
    if [ x"$srcbundle" == x"ciao" ]; then
	srcsubdir="" # (Prefix from src)
    else
	srcsubdir="bndls/""$srcbundle" # TODO: customize?
    fi
    srclicense="LGPL" # TODO: extract from Manifest.pl
}

function dst_config_bundle() {
    dsturl="github.com/ciao-lang/""$srcbundle"
    dstremote="git@github.com:ciao-lang/""$srcbundle"
}

function init_src_config() {
    repo=ciao-devel
    branch=master
    srcgit=`git rev-parse --git-dir`
    srcgit=`realpath "$srcgit"`
    srcremote="gitolite@ciao-lang.org:/ciao-devel"
}

function init_config() { # input: srcbundle (needs init_src_config)
    src_config_bundle
    dst_config_bundle
    #
    dstgit=`realpath "$pubrepos"/$dsturl/.git`
    dstgit_last_nopub_id=`realpath "$pubrepos"/$dsturl.last_nopub_id`
}

function config_summary() {
    cat <<EOF
Settings for '$srcbundle':

  srcgit: $srcgit 
  srcsubdir: $srcsubdir
  srclicense: $srclicense
  srcremote: $srcremote
 ===>
  dstgit: $dstgit
  dstremote: $dstremote

EOF
}

function config_short_summary() {
    cat <<EOF
[Bundle '$srcbundle' as $dstremote]
EOF
}

# ---------------------------------------------------------------------------

tmpd=/tmp/tmp-ciaopublish-$$
f_all=$tmpd/allfiles.txt
f_allpub=$tmpd/allfilespub.txt
f_nodist=$tmpd/nodist.txt
f_tmplog=$tmpd/tmplog.txt
commit_msg=$tmpd/commit_msg.txt
treedir=$tmpd/treedir

function init_tmpd() {
    mkdir -p "$tmpd"
}
function cleanup_tmpd() {
    rm -rf "$tmpd"
}
trap 'rm -rf "$tmpd"' EXIT

function filter_srcsubdir() {
    if [ x"$srcsubdir" == x"" ]; then
	cat
    else
	grep -e "^""$srcsubdir/"
    fi
}

# Get all public files
function get_public_files() {
    git --git-dir="$srcgit" ls-tree --name-only -r $id | filter_srcsubdir > $f_all
    cat $f_all | grep -e '/NODISTRIBUTE$' | while read p; do printf "^%s\\(/\\|$\\)\n" `dirname $p`; done > $f_nodist
    # (Exclude some more files)
    cat >> $f_nodist <<EOF
.arcconfig
.gitmodules
EOF
    cat $f_all | grep -v -f $f_nodist > $f_allpub
}

# Compose commit message
# TODO: add option to disable $rawbody
function compose_msg() {
    #Src-repo: $repo
    #Src-branch: $branch
    cat > "$commit_msg" <<EOF
$rawbody

Src-commit: $id
EOF
}

function patch_readme() {
    if [ -r README.md ]; then
	mv README.md README.md-old
    fi
    cat > README.md <<EOF
[![Build Status](https://travis-ci.org/ciao-lang/ciao.svg)](https://travis-ci.org/ciao-lang/ciao)
[![Build Status](https://ci.appveyor.com/api/projects/status/fu2eb23je22xc228?svg=true)](https://ci.appveyor.com/project/jfmc/ciao)

EOF
    if [ -r README.md-old ]; then
	cat README.md-old >> README.md
    fi
    cat >> README.md <<EOF

---
**NOTE**: This repository is [automatically synchronized](https://github.com/ciao-lang/ciao-distro-tools) from the Ciao monorepo.
EOF
    rm -f README.md-old
}

function patch_ci_scripts() {
    cp "$_base/../ci-scripts/travis.yml" .travis.yml
    cp "$_base/../ci-scripts/appveyor.yml" .appveyor.yml
}

function patch_license() {
    case "$srclicense" in
	LGPL)
	    cp "$_base/../licenses/LGPL" LGPL
	    ;;
	GPL)
	    cp "$_base/../licenses/GPL" GPL
	    ;;
	*) # None
	    ;;
    esac
}

function patch_tree() {
    pushd "$treedir" > /dev/null 2>&1
    # Patch github specific (depends on bundle)
    if [ x"$srcbundle" == x"ciao" ]; then
	rm -f COPYRIGHT LGPL GPL # Outdated...
	patch_readme
	patch_ci_scripts
    else
	# Remove ACTIVATE mark
	if [ -r ACTIVATE ]; then
	    rm ACTIVATE
	fi
    fi
    patch_license
    popd > /dev/null 2>&1
}

# Count number of components in a path
# (Replace everything except / and print length)
function count_components() { # path
    local slashes=`echo "$1" | sed 's;[^/];;g'`
    printf "%d" $((1 + ${#slashes}))
}

function untar_srcsubdir() {
    local comps
    if [ x"$srcsubdir" == x"" ]; then
	tar -xf -
    else
	comps=`count_components "$srcsubdir"`
	tar -xf - --strip-components=$comps
    fi
}

function checkout_tree() {
    # Do a checkout of the public files
    # Note: using xargs seems fine here, `getconf ARG_MAX` is 262144
    rm -rf "$treedir" 
    mkdir -p "$treedir"
    pushd "$treedir" > /dev/null 2>&1
    cat "$f_allpub" | xargs git --git-dir="$srcgit" archive $id | untar_srcsubdir
    popd > /dev/null 2>&1
}
function commit_tree() {
    #(debug) printf "Adding commit: %s\n" $id
    pushd "$treedir" > /dev/null 2>&1
    # Update index (add new files, update modified files, remove missing files) to match the working tree
    git --git-dir="$dstgit" add --dry-run -A > "$f_tmplog"
    if ! [ -s "$f_tmplog" ]; then # No expected changes
	printf "Commit $id does not contain public changes\n"
	# Save $id as the latest without public changes
	printf "%s" "$id" > "$dstgit_last_nopub_id"
    else
	printf "Commit $id contains public changes\n"
	git --git-dir="$dstgit" add -A | gitprefix
	if git --git-dir="$dstgit" commit --author="$author_str" --date="$date_str" -F "$commit_msg" > "$f_tmplog"; then
            cat "$f_tmplog" | gitprefix
	    did_commit=yes
	else
            cat "$f_tmplog" | gitprefix
	    # TODO: This should be a bug (we checked before with "add --dry-run")
	    printf "Could not commit $id! (does it really contain public changes?)\n"
	fi
    fi
    popd > /dev/null 2>&1
}

function get_commit_info() {
    author_str=`git --git-dir="$srcgit" log --format="%aN <%aE>" -n 1 -r $id`
    date_str=`git --git-dir="$srcgit" log --format=%ad -n 1 -r $id`
    rawbody=`git --git-dir="$srcgit" log --format=%B -n 1 -r $id`
}

function get_latest_commit() {
    id=`git --git-dir="$srcgit" log --format=%H -n 1`
}

function prepare_tree() { # (env: id)
    #(debug) printf "Getting pristine view for commit: %s\n" $id
    checkout_tree
    patch_tree
}

function publish_commits() {
    did_commit=no
    if [ $# == 0 ]; then
	get_latest_commit
    else
	id=$1
    fi
    local target_id=$id
    local l_id
    check_dst_id # (needed for unpublished commits)
    for l_id in `unpublished_commits`; do
	id=$l_id
	get_public_files
	get_commit_info
	compose_msg
	prepare_tree
	commit_tree
	if [ $l_id == $target_id ]; then
	    break
	fi
    done
    if [ x"$did_commit" == x"yes" ]; then
	after_commit_help
    fi
}

function squash_commit() {
    did_commit=no
    if [ $# == 0 ]; then
	get_latest_commit
    else
	id=$1
    fi
    get_public_files
    get_commit_info
    if [ x"`dst_id`" == x"" ]; then
	rawbody="Initial commit" # TODO: customize
    fi
    compose_msg
    prepare_tree
    if [ $dryrun == "no" ]; then
	commit_tree
    else
	drytreedir=/tmp/dry-treedir-$$
	mv "$treedir" "$drytreedir"
	cat <<EOF

Running in DRY-RUN mode. No commit has been made.
You can inspect the tree at:
  $drytreedir

EOF
    fi
    #
    if [ x"$did_commit" == x"yes" ]; then
	after_commit_help
    fi
}

function after_init_help() {
    cat <<EOF

Destination repository created, you can publish your commits.

Use the 'squash' command to publish the current version, or select the
first commit with 'squash Id'. Amending the commit message is
recommended.

EOF
}

function after_commit_help() {
    cat <<EOF

Use 'rebase -i' rebase commits if needed.
Use 'push' to send commits to remote.
EOF
}

function after_push_help() {
    # TODO: configure per bundle
    if [ x"$srcbundle" == x"ciao" ]; then
	cat <<EOF

NOTES:

  - Make sure that Travis-CI and AppVeyor builds the source correctly
    (go to https://github.com/ciao-lang/ciao)

  - Remember to trigger build at https://github.com/ciao-lang/docker-ciao

    Output: https://hub.docker.com/r/ciaolang/ciao/

  - Remember to trigger build at https://github.com/ciao-lang/ciao.CD

    Output: https://bintray.com/ciao-lang/builds/ciao/latest/view

EOF
    fi
}

# Commit (from srcgit) at dstgit
function dst_id() {
    local dst_id=`git --git-dir="$dstgit" log --format=%B -n 1 | grep "Src-commit:"`
    dst_id=${dst_id#"Src-commit: "}
    printf "%s" $dst_id
}

# (in chronological order)
# (Uses $dstgit_last_nopub_id if available)
function unpublished_commits() {
    if [ -r "$dstgit_last_nopub_id" ]; then
	local nopub_id=`cat "$dstgit_last_nopub_id"`
	# use 'sed' to stop if nopub_id is found
	unpublished_commits_ | sed -n '/'$nopub_id'/q;p' | $revtail
    else
	unpublished_commits_ | $revtail
    fi
}

function unpublished_commits_() {
    local dst_id=`dst_id`
    # use 'sed' to stop if dst_id is found
    git --git-dir="$srcgit" log --format=%H | sed -n '/'$dst_id'/q;p'
}

function check_dst_id() {
    if [ x"`dst_id`" == x"" ]; then
	cat <<EOF
ERROR: Could not find any 'Src-commit' mark in the publishing repository.
Please specify the first commit Id in 'publish' or 'squash'.
EOF
	exit 1
    fi 
}

function status() {
    get_latest_commit
    local dst_id=`dst_id`
    cat <<EOF
Latest commit: $id
Commit at dstgit: $dst_id
EOF
    if [ -r "$dstgit_last_nopub_id" ]; then
	printf "Last known commit without public changes: %s\n" `cat "$dstgit_last_nopub_id"`
    fi
    check_dst_id # (needed for unpublished commits)
    cat <<EOF
Unpublished commits (may not contain public changes):
EOF
    unpublished_commits
}

# Git command at dstgit
function cmd_git_dstgit() {
    pushd "$dstgit" > /dev/null 2>&1
    cd ..
    git "$@"
    popd > /dev/null 2>&1
}

# Prefix each line with "[git] "
function gitprefix() {
    sed -e 's/^/[git] /'
}

# ---------------------------------------------------------------------------

function check_repos() {
    if ! git --git-dir="$srcgit" remote -v | grep "$srcremote" > /dev/null; then
	config_summary
	cat <<EOF
ERROR: Source directory does not seem the right clone. 

  Make sure that directory:
    $srcgit
  contains a clone of:
    $srcremote

EOF
	exit 1
    fi
    if ! [ -x "$dstgit" ] || \
       ! git --git-dir="$dstgit" remote -v | grep "$dstremote" > /dev/null; then
	config_summary
	cat <<EOF
ERROR: Could not find a valid local clone for publishing

  Make sure that this directory exists:
    $dstgit
  and contains a clone of:
    $dstremote

If the repository for publishing does not exists yet, you can use the
'init' command of this script to create a new empty repository.

EOF
	exit 1
    fi
    config_short_summary
}

function init_dstgit() {
    # Create dstgit repository (if it does not exists)
    if [ -r "$dstgit" ]; then
       cat <<EOF
ERROR: A local clone for publishing seems to exists at:
  $dstgit 

Aborting repository initialization.

EOF
       exit 1
    fi
    if ! git ls-remote "$dstremote" > /dev/null 2>&1 ; then
	# TODO: add support to GitHub API if needed
	cat <<EOF
NOTE: Remote repository must be created manually.

Please login to https://github.com and create a new empty
repository for $dstremote.

Then try again the 'init' command.

EOF
	exit 1
    fi
    cat <<EOF
Initializing a publishing repository at:
  $dstremote
EOF
    local dstgitdir=`dirname "$dstgit"` # without .git
    mkdir -p "$dstgitdir"
    pushd "$dstgitdir" > /dev/null 2>&1
    git init | gitprefix
    cat > README <<EOF
(empty)
EOF
    git add . | gitprefix
    git commit -m "First commit" | gitprefix
    git remote add origin "$dstremote" | gitprefix
    git remote -v | gitprefix
    git push -u origin master | gitprefix
    popd > /dev/null 2>&1
}

function pull_dstgit() {
    # Pull to make sure that dstgit contains latest commits
    cmd_git_dstgit pull | gitprefix
}

function push_dstgit() { # [push args...]
    # TODO: make check for pending commits optional? (it is faster
    # than simply 'git push' but assumes remote is OK)
    cmd_git_dstgit log origin/master..master > "$f_tmplog"
    if ! [ -s "$f_tmplog" ]; then # No pending commits
	printf "No remaining commits to push\n"
    else
	cmd_git_dstgit push "$@" | gitprefix
	after_push_help 
    fi
}

allbundles=no
srcbundle= # none
pubrepos="$HOME"/REPOS-ciao-publish
while [ -t ]; do
    case $1 in
	"--bundle") # BundleName
	    shift
	    srcbundle=$1
	    shift
	    ;;
	"--all")
	    shift
	    allbundles=yes
	    ;;
	"--pubrepos")
	    shift
	    pubrepos=$1
	    shift
	    ;;
	*)
	    break
    esac
done

if ! [ -x "$pubrepos" ]; then
    cat <<EOF
ERROR: Directory for publishing repository clones do not exist or is not readable:
$pubrepos
EOF
	exit 1
fi

function run() { # env: srcbundle
    init_tmpd
    init_config
    dryrun=no
    case $1 in
	init)
	    shift; init_dstgit; after_init_help ;;
	list)
	    shift; list_bundles ;;
	info)
	    shift; config_summary ;;
	pull)
	    shift; check_repos; pull_dstgit ;;
	publish)
	    shift; check_repos; publish_commits "$@" ;;
	squash)
	    shift; check_repos; squash_commit "$@" ;;
	dry-squash)
	    shift; dryrun=yes; check_repos; squash_commit "$@" ;;
	status)
	    shift; check_repos; status "$@" ;;
	push)
	    shift; check_repos; push_dstgit ;;
	rebase)
	    shift; check_repos; cmd_git_dstgit rebase "$@" ;;
	*)
	    cat <<EOF
Bad command '$1', use 'help' for help.

EOF
	    exit 1
	    ;;
    esac
    cleanup_tmpd
}

if [ x"$allbundles" == x"yes" ]; then
    # Run for all bundles
    case $1 in
	pull|publish|status|push)
	    true
	    ;;
	*)
	    cat <<EOF
ERROR: '--all' is not allowed in command '$1'

EOF
	    exit 1
	    ;;
    esac
    init_src_config
    for b in `list_bundles`; do
	srcbundle=$b
	run "$@"
	echo
    done
else
    init_src_config
    if [ x"$srcbundle" == x"" ]; then
	# Guess the bundle if it is not provided
	detect_bundle
    fi
    run "$@"
fi
