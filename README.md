# misc-tools

Collection of simple tools I use

## Build

### shell

    cp shell/*.sh build/ && cd build/
    for file in $(ls); do mv $file ${file%.*}; done
