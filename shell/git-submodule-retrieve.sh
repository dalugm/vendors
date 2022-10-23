if [ ! -d '.git' ]; then
    git init .

    # Reconstruct submodules using .gitmodules
    # Based on https://stackoverflow.com/a/11258810
    git config -f .gitmodules --get-regexp '^submodule\..*\.path'

    while read path_key path
    do
        rm -rf $path
        url_key=$(echo $path_key | sed 's/\.path/.url/')
        branch_key=$(echo $path_key | sed 's/\.path/.branch/')
        url=$(git config -f .gitmodules --get "$url_key")
        branch=$(git config -f .gitmodules --get "$branch_key")
        git submodule add -b $branch --depth 1 $url $path
    done
fi

# Continue as we would /with/ a .git directory
git submodule foreach --recursive 'git fetch --tags'
git submodule update --recursive --init
