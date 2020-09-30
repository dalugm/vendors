#
## Execute `git pull` on every directory within the current directory.
#
git-update-all() {
  find . \
    -maxdepth 1 \
    ! -path . \
    -type d \
    -print \
    -execdir git --git-dir={}/.git --work-tree="${PWD}/{}" pull --rebase \;
}

#
## Prints Git branch name and working tree status for prompt.
#
git_prompt_info() {
    local git_branch=''
    local git_status=''

    # Do nothing if the current directory is not a Git repository.
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) != 'true' ]]; then
        return
    fi

    # Do nothing if the current directory is inside `.git`.
    if [[ $(git rev-parse --is-inside-git-dir 2> /dev/null) == 'true' ]]; then
        return
    fi

    # Check for uncommitted changes.
    if ! git diff --quiet --ignore-submodules --cached; then
        git_status="${git_status}+"
    fi

    # Check for unstaged changes.
    if ! git diff-files --quiet --ignore-submodules --; then
        git_status="${git_status}!"
    fi

    # Check for untracked files.
    if [[ -n "$(git ls-files --others --exclude-standard)" ]]; then
        git_status="${git_status}?"
    fi

    # Check for stashed files.
    if git rev-parse --verify refs/stash &> /dev/null; then
        git_status="${git_status}$"
    fi

    if [[ -n "${git_status}" ]]; then
        git_status=" ${git_status}"
    fi

    # Get the branch name or short commit SHA.
    git_branch="$(git rev-parse --abbrev-ref HEAD 2> /dev/null \
    || git rev-parse --short HEAD 2> /dev/null \
    || printf '(unknown)')"

    printf "%s" "${1}${git_branch}${git_status}${2}"
}
