set nocompatible               " be iMproved
filetype off                   " required!

" Colorscheme
if has("gui_running")
    " colorscheme breeze
    colorscheme idleFingers
    set antialias
else
    colorscheme llimllib
    " torte highlights the bottom, I love that! but I want a diff between a #
    " and a string
endif

" default space and tab handling
set shiftwidth=4
set tabstop=4
set expandtab
set softtabstop=4

" Highlight EOL whitespace, http://vim.wikia.com/wiki/Highlight_unwanted_spaces
highlight ExtraWhitespace ctermbg=darkred guibg=#382424
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
" the above flashes annoyingly while typing, be calmer in insert mode
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/

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

" keep the cursor in the middle of the screen
set scrolloff=999

" turn off swap files
set nobackup
set nowritebackup
set noswapfile

" this group is recommended by http://items.sjbach.com/319/configuring-vim-right
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

" Syntax Highlighting
if &t_Co > 2 || has("gui_running")
  syntax on
endif

" C-j replaces <C-w>j and so on (split navigation)
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-H> <C-W>h
map <C-_> <C-W>_

"insert one character
noremap <C-i> i<space><esc>r

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

" better tab navigation mappings
map gn gt
map gN gT

"Specific commands for filetypes
augroup myfiletypes
  "clear old autocmds in group
  autocmd!
  "for ruby, autoindent with two spaces, always expand tabs
  autocmd FileType ruby,haml,eruby,yaml set sw=2 sts=2 et
  autocmd FileType html set sw=2 sts=2 et
  autocmd FileType python,c set sw=4 sts=4 et
  autocmd FileType javascript set sw=2 sts=2 et
  autocmd FileType go set ts=4 sw=4 sts=4 noet
augroup END

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" github repos

" Git bindings
Bundle 'tpope/vim-fugitive'

" easier HTML typing
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}

" NERDtree
Bundle 'scrooloose/nerdtree'

" disable slow features on large files
Bundle 'vim-scripts/LargeFile'

" database access
Bundle 'vim-scripts/dbext.vim'

" Surround things with other things
Bundle 'tpope/vim-surround'

" Buf explore things
Bundle 'corntrace/bufexplorer'

" Git gutter marks
Bundle 'airblade/vim-gitgutter'

"" vim-scripts repos
"Bundle 'L9'
"Bundle 'FuzzyFinder'

"" non github repos
Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..

"disable all bells
set visualbell t_vb=

"install go syntax and tools
" Some Linux distributions set filetype in /etc/vimrc.
" Clear filetype flags before changing runtimepath to force Vim to reload them.
filetype off
filetype plugin indent off
set runtimepath+=$GOROOT/misc/vim
filetype plugin indent on
syntax on

" http://statico.github.io/vim.html
" emacs-style command-line controls
cnoremap <C-a>  <Home>
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-d>  <Delete>
cnoremap <M-b>  <S-Left>
cnoremap <M-f>  <S-Right>
cnoremap <M-d>  <S-right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-g>  <C-c>

" ATM, only accept golang-formatted errors for quickfix. If I start using this
" more, I should set it to accept error types only for specific files
set errorformat=%f:%l:%c:\ %m

" Only run Classification tests right now. Is there a way to have it figure
" out what test file I'm currently in?
nnoremap <leader>m :copen<CR>:cex system('make test')<CR>
nnoremap <leader>n :cex system('make')<CR>
nnoremap <leader>l :cex system('make lint')<CR>

" plugin helpers
nnoremap <leader>t :NERDTreeToggle<CR>
nnoremap <leader>f :CommandT<CR>

" Format json
nnoremap <leader>pp :%!jsonpp<CR>
