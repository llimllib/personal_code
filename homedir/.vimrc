"don't be compatible with old vi
set nocompatible

set shiftwidth=4
set tabstop=4
set expandtab
set softtabstop=4

" Disable the F1 help key
map <F1> <Esc>
imap <F1> <Esc>

" http://vim.wikia.com/wiki/Move_cursor_by_display_lines_when_wrapping
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

" Automatically read files which have been changed outside of Vim, if we
" haven't changed it already.
set autoread

"keep the cursor in the middle of the screen
set scrolloff=999

"turn off swap files
set nobackup
set nowritebackup
set noswapfile

"this group is recommended by http://items.sjbach.com/319/configuring-vim-right
set ignorecase 
set smartcase
set title
set scrolloff=3
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set hidden
nnoremap ' `
nnoremap ` '
let mapleader = ","
set history=1000
runtime macros/matchit.vim

" Stolen from http://github.com/ask/ask-vimrc/blob/master/vimrc
set completeopt=menuone,longest,preview
set hidden
set complete+=k

if has("gui_running")
    " colorscheme breeze 
    colorscheme idleFingers
    set antialias
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
endif

map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-H> <C-W>h
map <C-_> <C-W>_
imap <C-D> require 'ruby-debug'; debugger
nmap <C-D> orequire 'ruby-debug'; debugger

"insert one character
noremap <C-i> i<space><esc>r

",t opens fuzzy file, ,vt and ,ht open with vertical or horiz split
map <silent> <leader>t :FufFile **/<CR>
map <silent> <leader>vt :vnew<CR>:FufFile **/<CR>
map <silent> <leader>ht :new<CR>:FufFile **/<CR>

"try this again
set autoindent

"no toolbar
set guioptions-=T

"show line
set ruler

"don't highlight searches
set nohls

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
"
"load better yaml syntax
au BufNewFile,BufRead *.yaml,*.yml so ~/.vim/syntax/yaml.vim

"load better ChuCK syntax
au BufNewFile,BufRead *.ck         setf ck 

function! MyLabel() 
  if exists('t:name') 
    return t:name 
  else 
    return '' 
  endif 
endfunction 
set guitablabel=%{MyLabel()} 

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

augroup myfiletypes
  "clear old autocmds in group
  autocmd!
  "for ruby, autoindent with two spaces, always expand tabs
  autocmd FileType ruby,haml,eruby,yaml set sw=2 sts=2 et
  autocmd FileType python set sw=4 sts=4 et
  autocmd FileType javascript set sw=2 sts=2 et
augroup END

" http://tim.theenchanter.com/2008/07/crontab-temp-file-must-be-edited-in.html ?
set backupskip=/tmp/*,/private/tmp/*" 

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

"source *.as as flash files
autocmd BufRead *.as set filetype=actionscript
autocmd BufRead *.mxml set filetype=mxml

" http://tim.theenchanter.com/2008/07/crontab-temp-file-must-be-edited-in.html ?
set backupskip=/tmp/*,/private/tmp/*" 

"If set to 1 then you can leave the Conque buffer using the <C-w> commands while you're still in insert mode. If set to 0 then the <C-w> character will be sent to the terminal. If both this option and ConqueTerm_InsertOnEnter are set you can go in and out of the terminal buffer while never leaving insert mode.
let g:ConqueTerm_CWInsert = 1

command PythonShell :set nolist | ConqueTermSplit ipython
