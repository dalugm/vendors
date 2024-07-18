" File: misc.vim
" Author: dalu
" Description: 
" Last Modified: May 01, 2020

" Let 'tl' toggle between this and the last accessed tab
let b:lasttab = 1
nmap <Leader>tl :exe "tabn ".b:lasttab<CR>
au TabLeave * let b:lasttab = tabpagenr()

function! DeleteTillSlash()
    let g:cmd = getcmdline()

    if has("win16") || has("win32")
        let g:cmd_edited = substitute(g:cmd, "\\(.*\[\\\\]\\).*", "\\1", "")
    else
        let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*", "\\1", "")
    endif

    if g:cmd == g:cmd_edited
        if has("win16") || has("win32")
            let g:cmd_edited = substitute(g:cmd, "\\(.*\[\\\\\]\\).*\[\\\\\]", "\\1", "")
        else
            let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*/", "\\1", "")
        endif
    endif

    return g:cmd_edited
endfunction
