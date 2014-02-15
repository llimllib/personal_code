set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
"colorscheme default
let g:colors_name = "llimllib"

" hardcoded colors :
" GUI Comment : #80a0ff = Light blue

" GUI
"highlight Normal     guifg=Grey80	guibg=Black
"highlight Search     guifg=Black	guibg=Red	gui=bold
"highlight Visual     guifg=#404040			gui=bold
"highlight Cursor     guifg=Black	guibg=Green	gui=bold
"highlight Special    guifg=Orange
"highlight Comment    guifg=#80a0ff
"highlight StatusLine guifg=blue		guibg=white
"highlight Statement  guifg=Yellow			gui=NONE
"highlight Type						gui=NONE

" Console
highlight Normal     ctermfg=LightGrey	ctermbg=Black
highlight Search     ctermfg=Black	ctermbg=Red	cterm=NONE
highlight Visual					cterm=reverse
highlight Cursor     ctermfg=Black	ctermbg=Green	cterm=bold
highlight Special    ctermfg=Brown
highlight Comment    ctermfg=Blue
highlight StatusLine ctermfg=blue	ctermbg=white
highlight Statement  ctermfg=Yellow			cterm=NONE
highlight Type						cterm=NONE

" only for vim 5
if has("unix")
  if v:version<600
    highlight Normal  ctermfg=Grey	ctermbg=Black	cterm=NONE	guifg=Grey80      guibg=Black	gui=NONE
    highlight Search  ctermfg=Black	ctermbg=Red	cterm=bold	guifg=Black       guibg=Red	gui=bold
    highlight Visual  ctermfg=Black	ctermbg=yellow	cterm=bold	guifg=#404040			gui=bold
    highlight Special ctermfg=LightBlue			cterm=NONE	guifg=LightBlue			gui=NONE
    highlight Comment ctermfg=Cyan			cterm=NONE	guifg=LightBlue			gui=NONE
  endif
endif

" from slate
:hi Statement guifg=CornflowerBlue ctermfg=lightblue
:hi Visual gui=none guifg=khaki guibg=olivedrab cterm=reverse
:hi WarningMsg guifg=salmon ctermfg=1
:hi String guifg=SkyBlue ctermfg=darkcyan
:hi Constant guifg=#ffa0a0 ctermfg=brown
:hi Special guifg=darkkhaki ctermfg=brown
:hi Identifier guifg=salmon ctermfg=red
:hi Include guifg=red ctermfg=red
:hi PreProc guifg=red guibg=white ctermfg=red
:hi Operator guifg=Red ctermfg=Red
:hi Define guifg=gold gui=bold ctermfg=yellow
:hi Type guifg=CornflowerBlue ctermfg=2
:hi Function guifg=navajowhite ctermfg=brown
:hi Structure guifg=green ctermfg=green
:hi LineNr guifg=grey50 ctermfg=3
:hi Ignore guifg=grey40 cterm=bold ctermfg=7
:hi Todo guifg=orangered guibg=yellow2
:hi Directory ctermfg=darkcyan
:hi ErrorMsg cterm=bold guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1
:hi VisualNOS cterm=bold,underline
:hi WildMenu ctermfg=0 ctermbg=3
:hi DiffAdd ctermbg=4
:hi DiffChange ctermbg=5
:hi DiffDelete cterm=bold ctermfg=4 ctermbg=6
:hi DiffText cterm=bold ctermbg=1
:hi Underlined cterm=underline ctermfg=5
:hi Error guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1
:hi SpellErrors guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1
