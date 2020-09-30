#!/usr/bin/env bash

#
# Tests whether a directory can be added to `PATH`.
#
test_path() {
    [[ -d "${1}" && ":${PATH}:" != *":${1}:"* ]]
}

#
# Sets the `PATH` environment variable.
#
set_path() {
    # Define the directories to be prepended to `PATH`.
    local -a prepend_dirs=(
        /usr/local/bin
    )

    # Define the directories to be appended to `PATH`.
    local -a append_dirs=(
        /usr/bin
        "${HOME}/bin"
        "${HOME}/.local/bin"
    )

    # Prepend directories to `PATH`.
    for index in ${!prepend_dirs[*]}; do
        if test_path "${prepend_dirs[$index]}"; then
            PATH="${prepend_dirs[$index]}:${PATH}"
        fi
        echo "HERE"
    done

    # Append directories to `PATH`.
    for index in ${!append_dirs[*]}; do
        if test_path "${append_dirs[$index]}"; then
            PATH="${PATH}:${append_dirs[$index]}"
        fi
    done

    export PATH
}

set_path
unset -f test_path
unset -f set_path
