" File: wsbl.vim
" Author:
" Description:
" Last Modified: May 01, 2020

" Strip trailing spaces and blank lines of EOF when saving files
if !exists('g:rc_strip_wsbl')
    let g:rc_strip_wsbl = 1
else
    if g:rc_strip_wsbl == 0 | augroup! rc_strip_wsbl | endif
endif

augroup rc_strip_wsbl
    autocmd!
    autocmd BufWritePre * call RCStripWSBL()
augroup END

function! RCStripWSBL()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//ge
    %s/\(\n\)\+\%$//ge
    call cursor(l, c)
endfunction
