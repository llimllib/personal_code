set shiftwidth=4
set tabstop=4
set expandtab
set softtabstop=4

if has("gui_running")
    colorscheme breeze 
else
    colorscheme delek
endif

" this doesn't work on windaz
" set backspace=indent,eol,start
set backspace=indent,eol,start whichwrap+=<,>,[,]
" backspace in Visual mode deletes selection
vnoremap <BS> d
set foldmethod=indent
set foldlevel=999

" turn plugins on
:filetype plugin on

" Syntax Highlighting
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-H> <C-W>h
map <C-_> <C-W>_
imap <C-D> require 'ruby-debug'; debugger
nmap <C-D> orequire 'ruby-debug'; debugger

"try this again
set autoindent

"no toolbar
set guioptions-=T

"show line
set ruler

"don't highlight searches
set nohls

"insert one character
noremap <C-i> i<space><esc>r

"Load nemerle files as utf-8 by default
augroup nemerle
	au!
	autocmd BufNewfile,BufReadPre *.n
		\ set fencs=ucs-bom,utf-8,iso-8859-2 fenc=utf-8
augroup END

"Gui tabs only show the filename
set guitablabel=%t

"tab autocompletes
function! InsertTabWrapper()
      let col = col('.') - 1
      if !col || getline('.')[col - 1] !~ '\k'
          return "\<tab>"
      else
          return "\<c-p>"
      endif
endfunction 

inoremap <tab> <c-r>=InsertTabWrapper()<cr>

":set formatoptions=aw2tq to get reformatted paragraphs
set guifont=Inconsolata:h14.00

map <F8> iimport pdb; pdb.set_trace()
inoremap <F8> import pdb; pdb.set_trace()

map gn gt
map gN gT

"J deletes EOLs - very useful on mac keyboard!
"
"z10<CR> will set the current split to 10 lines. Ah ha!

"CTRL-W r				*CTRL-W_r* *CTRL-W_CTRL-R* *E443*
" rotate windows down or right

set vb t_vb=

let g:dbext_default_profile_PG = 'type=PGSQL:user=fuse_fuse:dbname=fusebox'

function! MyLabel(n)
  if exists('t:name')
    return t:name
  else
    return ''
  endif
endfunction

function! MyGuiTabLine()
 let s = '%{MyLabel(' . tabpagenr() . ')}'
 return s
endfunction

set guitablabel=%!MyGuiTabLine()

" vim -b : edit binary using xxd-format!
augroup Binary
  au!
  au BufReadPre   *.ttf,*.dfont let &bin=1
  au BufReadPost  *.ttf,*.dfont if &bin | %!xxd
  au BufReadPost  *.ttf,*.dfont set ft=xxd | endif
  au BufWritePre  *.ttf,*.dfont if &bin | %!xxd -r
  au BufWritePre  *.ttf,*.dfont endif
  au BufWritePost *.ttf,*.dfont if &bin | %!xxd
  au BufWritePost *.ttf,*.dfont set nomod | endif
augroup END

if has("spell")
  " turn spelling off by default
  set nospell

  " toggle spelling with F4 key
  map <F4> :set spell!<CR><Bar>:echo "Spell Check: " . strpart("OffOn", 3 * &spell, 3)<CR>

  " they were using white on white
  highlight PmenuSel ctermfg=black ctermbg=lightgray

  " limit it to just the top 10 items
  set sps=best,10                    
endif

