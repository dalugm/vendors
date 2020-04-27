" Filename: toggle-line-number.vim
" Author: dalu <mou.tong@qq.com>
" Maintainer: dalu <mou.tong@qq.com>
" Created: 2020-04-26 16:42
" Last Upated:
"          By:
" Keywords: line number
" Version: 0.1
" Changelog:
"     0.1 - initial version
" Commentary:

" Toggle line number in different state


" Code:

" Show line number by default
if !exists('g:rc_show_line_number')
    let g:rc_show_line_number = 1
else
    " If show_line_number is explicitly set to false,
    " events-driving UseAbsOrRelNum will be stopped.
    if g:rc_show_line_number == 0 | augroup! rc_line_number | endif
endif

" Toggle showing line number
let g:rc_linenr_switch = g:rc_show_line_number
nnoremap <silent> <Leader>n :call RCToggleLineNumber(g:rc_linenr_switch)<CR>

function! RCToggleLineNumber(switch)
    if a:switch
        set number relativenumber
        let g:rc_linenr_switch = 0
    else
        set nonumber norelativenumber
        let g:rc_linenr_switch = 1
    endif
endfunction

" Run once to show initial linenum
call RCToggleLineNumber(g:rc_show_line_number)

" Use absolute linenum in insert mode; relative linenum in normal mode
augroup rc_line_number
    autocmd FocusLost,InsertEnter * call RCUseAbsOrRelNum(1)
    autocmd FocusGained,InsertLeave * call RCUseAbsOrRelNum(0)
augroup END

function! RCUseAbsOrRelNum(switch)
    if g:rc_show_line_number == 0 || exists('#goyo')
        set nonumber norelativenumber
    else
        if a:switch
            set number norelativenumber
        else
            set number relativenumber
        endif
    endif
endfunction
