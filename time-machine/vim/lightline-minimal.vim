" =============================================================================
" Filename: autoload/lightline/colorscheme/minimal.vim
" Author: dalu <mou.tong@qq.com>
" License: MIT
" Created: 2020/04/24 20:27:51
" Last Modified: 2022/09/21 11:34:24
" Commentary:
" 马鞭草紫	#ede3e7
" 远山紫	#ccccd6
" 芡食白	#e2e1e4
" =============================================================================

" common colors
let s:black  = [ "#36292f" , 238 ]
let s:white  = [ "#f8f4ed" , 51  ]
let s:blue   = [ "#1772b4" , 75  ]
let s:green  = [ "#1ba784" , 76  ]
let s:purple = [ "#7e1671" , 176 ]
let s:gray1  = [ "#ede3e7" , 238 ]
let s:gray2  = [ "#e2e1e4" , 235 ]
let s:gray3  = [ "#ccccd6" , 240 ]
let s:red    = [ "#de1c31" , 168 ]
let s:red1   = [ "#e06c75" , 168 ]
let s:red2   = [ "#f9723d" , 168 ]
let s:yellow = [ "#e2d849" , 180 ]
let s:none   = [ "NONE" , "NONE" ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

if lightline#colorscheme#background() ==# 'light'
  " Light variant
  let s:fg    = s:black
  let s:bg    = s:white
else
  " Dark variant
  let s:fg    = s:white
  let s:bg    = s:black
endif

" Common
let s:p.normal.left    = [ [ s:bg, s:green, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.normal.middle  = [ [ s:fg, s:gray2 ] ]
let s:p.normal.right   = [ [ s:bg, s:green, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.normal.error   = [ [ s:red2, s:bg ] ]
let s:p.normal.warning = [ [ s:yellow, s:bg ] ]

let s:p.inactive.left   = [ [ s:fg, s:none ], [ s:fg, s:none ] ]
let s:p.inactive.middle = [ [ s:purple, s:none ] ]
let s:p.inactive.right  = [ [ s:purple, s:none ], [ s:purple, s:none ] ]

let s:p.insert.right   = [ [ s:bg, s:blue, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.insert.left    = [ [ s:bg, s:blue, 'bold' ], [ s:fg, s:gray3 ] ]

let s:p.replace.right  = [ [ s:bg, s:red1, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.replace.left   = [ [ s:bg, s:red1, 'bold' ], [ s:fg, s:gray3 ] ]

let s:p.visual.right   = [ [ s:bg, s:yellow, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.visual.left    = [ [ s:bg, s:yellow, 'bold' ], [ s:fg, s:gray3 ] ]

let s:p.tabline.left   = [ [ s:purple, s:none ] ]
let s:p.tabline.middle = [ [ s:purple, s:none ] ]
let s:p.tabline.right  = [ [ s:purple, s:none ] ]
let s:p.tabline.tabsel = [ [ s:none, s:purple, 'bold' ] ]

let g:lightline#colorscheme#minimal#palette = lightline#colorscheme#flatten(s:p)
