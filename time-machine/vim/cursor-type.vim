if has ('nvim') || has('gui_running')
  set guicursor=n-v-sm:block
  set guicursor+=i-c-ci:ver25
  set guicursor+=ve-r-cr-o:hor20
  set guicursor+=a:blinkon0 " no cursor blink
else
  " Cursor Shape
  " SI = INSERT mode
  " SR = REPLACE mode
  " EI = NORMAL mode (ELSE)
  " Cursor settings:
  " 1 -> blinking block
  " 2 -> solid block
  " 3 -> blinking underscore
  " 4 -> solid underscore
  " 5 -> blinking vertical bar
  " 6 -> solid vertical bar
  " NOTE the value can be different in different terminals
  " @see https://vim.fandom.com/wiki/Change_cursor_shape_in_different_modes
  if $TERM_PROGRAM =~ "iTerm.app"
    if empty($TMUX)
      let &t_SI .= "\<Esc>]50;CursorShape=1\x7"
      let &t_SR .= "\<Esc>]50;CursorShape=2\x7"
      let &t_EI .= "\<Esc>]50;CursorShape=0\x7"
    else
      let &t_SI .= "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
      let &t_SR .= "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
      let &t_EI .= "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
    endif
  else
    let &t_SI .= "\e[5 q"
    let &t_SR .= "\e[4 q"
    let &t_EI .= "\e[1 q"
  endif
endif