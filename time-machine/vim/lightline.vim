" Filename: lightline.vim
" Author: dalu <mou.tong@qq.com>
" Maintainer: maintainer
" Created: 2020-06-08 22:43
" Last Upated:
"          By:
" Keywords: lightline
" Version: 0.1
" Changelog:
"     0.1 - initial version
" Commentary:

" change lightline's colorscheme with vim's colorscheme

" Code:


augroup LightlineColorscheme
    autocmd!
    autocmd ColorScheme * call s:lightline_update()
augroup END

function! s:lightline_update()
    if !exists('g:loaded_lightline')
        return
    endif
    try
        if g:colors_name =~? 'wombat\|solarized\|jellybeans\|seoul256\|gruvbox\|dracula'
            let g:lightline.colorscheme =
                        \ substitute(substitute(g:colors_name, '-', '_', 'g'), '256.*', '', '')
            call lightline#init()
            call lightline#colorscheme()
            call lightline#update()
        else
            let g:lightline.colorscheme = 'minimal'
            call lightline#init()
            call lightline#colorscheme()
            call lightline#update()
        endif
    catch
    endtry
endfunction

if g:colors_name =~? 'gruvbox\|solarized\|tomorrow'
    autocmd OptionSet background
                \ execute 'source' globpath(&rtp, 'autoload/lightline/colorscheme/' . g:colors_name . '.vim')
                \ | call lightline#init() | call lightline#colorscheme() | call lightline#update()
endif
