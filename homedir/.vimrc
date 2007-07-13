set shiftwidth=4
set tabstop=4
set expandtab
set softtabstop=4

colorscheme breeze

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
set guifont=Monaco:h14

map <F8> iimport pdb; pdb.set_trace()
inoremap <F8> import pdb; pdb.set_trace()

map gn gt
map gN gT
