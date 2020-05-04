" Filename: last-session.vim
" Author: dalu <mou.tong@qq.com>
" Maintainer: dalu <mou.tong@qq.com>
" Created: 2020-05-04 11:26
" Last Upated:
"          By:
" Keywords: restore last session
" Version: 0.2
" Changelog:
"     0.1 - initial version
"     0.2 - add a new version
"           use session dir instead of a file
" Commentary:

" restore last session when open vim

" Code:

" ----- v0.1 -----------------------------------------------
" Restore last session automatically (default off)
if !exists('g:rc_restore_last_session') | let g:rc_restore_last_session = 0 | endif

" Always save the last session
augroup save_session
    autocmd!
    autocmd VimLeave * exe ":mksession! ~/.vim/.last.session"
augroup END

" Try to restore last session
augroup restore_session
    autocmd!
    autocmd VimEnter * call RCRestoreLastSession()
augroup END

function! RCRestoreLastSession()
    if g:rc_restore_last_session
        if filereadable(expand("~/.vim/.last.session"))
           exe ":source ~/.vim/.last.session"
       endif
   endif
endfunction

" Restore the last session manually
if filereadable(expand("~/.vim/.last.session"))
    nmap <silent> <Leader>r :source ~/.vim/.last.session<CR>
endif

" ----- v0.2 -----------------------------------------------
function! MakeSession(overwrite)
  let b:sessiondir = $HOME . "/.vim/sessions" . getcwd()
  if (filewritable(b:sessiondir) != 2)
    exe 'silent !mkdir -p ' b:sessiondir
    redraw!
  endif
  let b:filename = b:sessiondir . '/session.vim'
  if a:overwrite == 0 && !empty(glob(b:filename))
    return
  endif
  exe "mksession! " . b:filename
endfunction

function! LoadSession()
  let b:sessiondir = $HOME . "/.vim/sessions" . getcwd()
  let b:sessionfile = b:sessiondir . "/session.vim"
  if (filereadable(b:sessionfile))
    exe 'source ' b:sessionfile
  else
    echo "No session loaded."
  endif
endfunction

" Adding automatons for when entering or leaving Vim
if(argc() == 0)
  au VimEnter * nested :call LoadSession()
  au VimLeave * :call MakeSession(1)
else
  au VimLeave * :call MakeSession(0)
endif
