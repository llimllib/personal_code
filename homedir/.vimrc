set nocompatible               " be iMproved
filetype off                   " required for vundle... we turn it back on later

" Colorscheme
if has("gui_running")
    " colorscheme breeze
    colorscheme idleFingers
    set antialias
else
    " colorscheme llimllib
    " colorscheme solarized
    colorscheme base16-twilight
endif

" turn on persistent undo, and store it in the vim dir
set undofile
set undodir=~/.vim/undodir

" default space and tab handling
set shiftwidth=4
set tabstop=4
set expandtab
set softtabstop=4

" use dark themes
set background=dark

" use the mouse in terminal mode
set mouse=a

" backspace over auto-indents, eols, start of lines
set backspace=indent,eol,start

" Highlight EOL whitespace, http://vim.wikia.com/wiki/Highlight_unwanted_spaces
set list listchars=tab:→\ ,trail:·
" autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
" autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
" the above flashes annoyingly while typing, be calmer in insert mode
" autocmd InsertLeave * match ExtraWhitespace /\s\+$/
" autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/

" Disable the F1 help key
map <F1> <Esc>
imap <F1> <Esc>

" http://vim.wikia.com/wiki/Move_cursor_by_display_lines_when_wrapping
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

" Ex mode is stupid
nnoremap Q <nop>

" Automatically read files which have been changed outside of Vim, if we
" haven't changed it already.
set autoread

" keep the cursor in the middle of the screen
set scrolloff=999

" turn off swap files
set nobackup
set nowritebackup
set noswapfile

" http://vim.wikia.com/wiki/Example_vimrc
set wildmenu
set showcmd
set confirm

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
  autocmd FileType go set ts=4 sw=4 sts=4 noet nolist
augroup END

"
" BEGIN VUNDLE SETUP
"

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Git bindings
Plugin 'tpope/vim-fugitive'

" easier HTML typing
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}

" NERDtree
Plugin 'scrooloose/nerdtree'

" disable slow features on large files
Plugin 'vim-scripts/LargeFile'

" database access
Plugin 'vim-scripts/dbext.vim'

" Surround things with other things
Plugin 'tpope/vim-surround'

" Buf explore things
Plugin 'corntrace/bufexplorer'

" Git gutter marks
Plugin 'airblade/vim-gitgutter'

" Match HTML tags
Plugin 'gregsexton/MatchTag'

" CoffeeScript support
Plugin 'kchmck/vim-coffee-script'

" Markdown support
Plugin 'hallison/vim-markdown'

" Rust support
Plugin 'wting/rust.vim'

" base 16 vim themes
Plugin 'chriskempson/base16-vim'

" XML support
Plugin 'othree/xml.vim'

" Handle ANSI escape codes
Plugin 'ponzellus/AnsiEsc'

" Golang
Plugin 'fatih/vim-go'

" NERD commenter
Plugin 'scrooloose/nerdcommenter'

" fzf fuzzy finder
Plugin 'junegunn/fzf'

"" vim-scripts repos
"Plugin 'L9'
"Plugin 'FuzzyFinder'

call vundle#end()            " required
filetype plugin indent on     " required for vundle
"
" Brief help
" :PluginList          - list configured plugins
" :PluginInstall(!)    - install (update) plugins
" :PluginSearch(!) foo - search (or refresh cache first) for foo
" :PluginClean(!)      - confirm (or auto-approve) removal of unused plugins
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Plugin commands are not allowed.
" Put your stuff after this line

"
" END VUNDLE SETUP
"

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

nnoremap <leader>l :!clear && rake<CR>

" run 'make'
nnoremap <leader>g :!clear && make<CR>

" plugin helpers
nnoremap <leader>t :NERDTreeToggle<CR>

" default fzf looks like:
" nnoremap <leader>f :FZF<CR>
"
" but instead, we'll call it with the "exact" option. This means that, for ex,
" 'servi aptc' matches 'services/aptc.js' but not
" 'app/settings/controllers/services.js'
" nnoremap <leader>f :call fzf#run({'options': '-e', 'sink': 'e', 'down': '40%'})<CR>
"
" ^^^ this doesn't handle c-x and c-v for split, so:

" I basically have no idea how this works. CnP from
" https://github.com/junegunn/fzf/issues/239#issuecomment-103081772
function! s:my_fzf_handler(lines) abort
  if empty(a:lines)
    return
  endif
  let cmd = get({ 'ctrl-x': 'split',
                \ 'ctrl-v': 'vsplit' }, remove(a:lines, 0), 'e')
  for item in a:lines
    execute cmd escape(item, ' %#\')
  endfor
endfunction

nnoremap <silent> <leader>f :call fzf#run({
  \ 'options': '-e --expect=ctrl-t,ctrl-x,ctrl-v',
  \ 'down':    '40%',
  \ 'sink*':   function('<sid>my_fzf_handler')})<cr>

" Format json
"nnoremap <leader>j :%!jsonpp<CR>
nnoremap <leader>j :%!jq ''<CR>

" Format xml
nnoremap <leader>x :%!xmllint --format --encode UTF-8 -<CR>

" Format html
nnoremap <leader>h :%!xmllint --format --encode UTF-8 --html -<CR>

" Ack search
nnoremap <leader>s :Ack -k -i

" ruby debugger
nnoremap <leader>d orequire 'byebug'; byebug<esc>
nnoremap <leader>D Orequire 'byebug'; byebug<esc>

" system paste and yank
nnoremap <leader>p :pu +<CR>
vnoremap <leader>y "*y

" edit vimrc and source vimrc, respectively
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>es :source $MYVIMRC<cr>

" Don't autocomplete html except with leader-a
let xml_tag_completion_map = "<leader>a"

let g:dbext_default_profile_VEC_HEROKU = 'type=pgsql:user=ubfp6mtntfbebm:dbname=d232n0hp1n58mk:host=ec2-54-83-62-176.compute-1.amazonaws.com:port=6002'
let g:dbext_default_profile_VEC_LOCAL = 'type=pgsql:user=llimllib:dbname=vec'
let g:dbext_default_profile_VEC_PROD_DUMP = 'type=pgsql:user=llimllib:dbname=prodvec'
