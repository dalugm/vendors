" Filename: compile.vim
" Author: dalu <mou.tong@qq.com>
" Maintainer: dalu <mou.tong@qq.com>
" Created: 2020-04-30 13:33
" Last Upated: 2020-05-03 20:53
"          By: dalu <mou.tong@qq.com>
" Keywords: compile
" Version: 0.2
" Changelog:
"     0.1 - initial version
"     0.2 - add async version
" Commentary:

" compile files and run


" Code:

" Default version
function! CompileRun()
    exec "w"
    if &filetype == "c"
        exec "call CompileGcc()"
        if search("mpi\.h") != 0
            exec "!mpirun -np 4 ./%<"
        else
            exec "! ./%<"
        endif
    elseif &filetype == "cpp" | "cc" | "cxx" | "cp" | "C"
        exec "call CompileGpp()"
        exec "! ./%<"
    elseif &filetype == "java"
        exec "!javac %"
        exec "!java %<"
    elseif &filetype == "sh"
        exec "!bash %"
    elseif &filetype == "python"
        exec "!python3 %"
    elseif &filetype == "html"
        exec "!firefox % &"
    elseif &filetype == "go"
        exec "!go build %<"
        exec "go run %"
    else
        echo "This file type has not been matched."
    endif
endfunc

function! CompileGcc()
    exec "w"
    let compilecmd="!gcc"
    let compileflag="-o %<"
    if search("mpi\.h") != 0
        let compilecmd = "!mpicc "
    endif
    if search("glut\.h") != 0
        let compileflag .= " -lglut -lGLU -lGL "
    endif
    if search("cv\.h") != 0
        let compileflag .= " -lcv -lhighgui -lcvaux "
    endif
    if search("omp\.h") != 0
        let compileflag .= " -fopenmp "
    endif
    if search("math\.h") != 0
        let compileflag .= " -lm "
    endif
    exec compilecmd." % ".compileflag
endfunc

function! CompileGpp()
    exec "w"
    let compilecmd="!g++"
    let compileflag="-o %<"
    if search("mpi\.h") != 0
        let compilecmd = "!mpic++ "
    endif
    if search("glut\.h") != 0
        let compileflag .= " -lglut -lGLU -lGL "
    endif
    if search("cv\.h") != 0
        let compileflag .= " -lcv -lhighgui -lcvaux "
    endif
    if search("omp\.h") != 0
        let compileflag .= " -fopenmp "
    endif
    if search("math\.h") != 0
        let compileflag .= " -lm "
    endif
    exec compilecmd." % ".compileflag
endfunc

" Async verison
function! CompileRun()
    execute "w"
    if (&filetype == 'c')
        execute "AsyncRun gcc % -o %<; time ./%<"
    endif
    if (&filetype == 'cpp')
        execute "AsyncRun g++ % -o %<; time ./%<"
    endif
    if (&filetype == 'python')
        execute "AsyncRun time python3 %"
    endif
    if (&filetype == 'php')
        execute "AsyncRun time php %"
    endif
    if (&filetype == 'javascript')
        execute "AsyncRun time node %"
    endif
    if (&filetype == 'java')
        execute "AsyncRun time javac Main.java; java Main;"
    endif
    if (&filetype == 'coffee')
        execute "AsyncRun time coffee %"
    endif
    if (&filetype == 'rust')
        execute "AsyncRun time rustc %; ./%< ;"
    endif
    if (&filetype == 'haskell' || &filetype == 'lhaskell')
        execute "AsyncRun time ghc %; ./%< ;"
    endif
    if (&filetype == 'tex')
        execute "AsyncRun time pdflatex %;"
    endif
    if (&filetype == 'markdown')
        execute "AsyncRun time pandoc -s -S -o %<.pdf %;"
    endif
endfunction
