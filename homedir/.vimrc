set nocompatible               " be iMproved

colorscheme Base2Tone_SpaceDark
if (has("termguicolors"))
  set termguicolors
endif

set encoding=utf-8
set fileencoding=utf-8

" turn on persistent undo, and store it in the vim dir
set undofile
set undodir=~/.vim/undodir

" hopeful that one of these will fix my slow scrolling, but not really
set ttyfast
set synmaxcol=400
set ttyscroll=3
set lazyredraw " to avoid scrolling problems
set redrawtime=10000 " typescript sucks

" default space and tab handling
set shiftwidth=4
set tabstop=4
set expandtab
set softtabstop=4

" fold by indents unless specified otherwise
set foldmethod=indent
set foldlevel=9999

" use dark themes
set background=dark

" use the mouse in terminal mode
set mouse=a

" vim by default chooses the xterm2 mouse mode, which does not work past
" column 223. Set it to sgr, which works past 223 but may not be present
" in older vims
if has("mouse_sgr")
    set ttymouse=sgr
else
    set ttymouse=xterm2
end

" backspace over auto-indents, eols, start of lines
set backspace=indent,eol,start

" Highlight EOL whitespace, http://vim.wikia.com/wiki/Highlight_unwanted_spaces
set list listchars=tab:→\ ,trail:·

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

" " keep the cursor in the middle of the screen
set scrolloff=999

" turn off swap files
set nobackup
set nowritebackup
set noswapfile

" http://vim.wikia.com/wiki/Example_vimrc
set wildmenu
set showcmd
set confirm

"" this group is recommended by http://items.sjbach.com/319/configuring-vim-right
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

" Syntax Highlighting
syntax on

" Easier split moving
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-H> <C-W>h
map <C-_> <C-W>_

" terminal mode shortcuts.
tnoremap <silent> <C-h> <C-W>h
tnoremap <silent> <C-j> <C-W>j
tnoremap <silent> <C-k> <C-W>k
tnoremap <silent> <C-l> <C-W>l
" This seems to mess up the up key :(
"tnoremap <Esc> <C-w>N
tnoremap <leader><Esc> <C-w>N

" toggle the quickfix list
inoremap <leader>l :cw<CR>

" Open splits to the right and below by default. Feels more natural
set splitbelow
set splitright

"insert one character
noremap <C-i> i<space><esc>r

"no toolbar
set guioptions-=T

"show line
set ruler
set statusline=%f%m%r%h\ [%L]\ [%{&ff}]\ %y%=[%p%%]\ [line:%05l,col:%02v]
set laststatus=2


"don't highlight searches
set nohls

"Gui tabs only show the filename
set guitablabel=%t

"disable all bells
set visualbell t_vb=

set autoindent
set smartindent
filetype indent on

" system paste and yank
nnoremap <leader>p :pu +<CR>
vnoremap <leader>y "+y

" edit vimrc and source vimrc, respectively
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>es :source $MYVIMRC<cr>

" http://vim.wikia.com/wiki/VimTip102
function! CleverTab()
  if pumvisible()
    return "\<C-N>"
  endif
  if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
    return "\<Tab>"
  elseif exists('&omnifunc') && &omnifunc != ''
    return "\<C-X>\<C-O>"
  else
    return "\<C-N>"
  endif
endfunction
inoremap <Tab> <C-R>=CleverTab()<CR>

" better tab navigation mappings
map gn gt
map gN gT

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

"Specific commands for filetypes
augroup myfiletypes
  "clear old autocmds in group
  autocmd!
  "for ruby, autoindent with two spaces, always expand tabs
  autocmd FileType ruby,haml,eruby,yaml set sw=2 sts=2 et
  autocmd FileType html set sw=2 sts=2 et
  autocmd FileType python,c set sw=4 sts=4 et
  autocmd FileType javascript set sw=2 sts=2 et
  autocmd FileType typescript set sw=2 sts=2 et
  autocmd FileType go 
                \ set ts=4 sw=4 sts=4 noet nolist  " |
                \ nnoremap <leader>r :GOVIMRename<CR>
  autocmd FileType smarty set syntax=gotexttmpl
  autocmd FileType proto set ts=2 sts=2 sw=2
augroup END

" ALE Automatic completion implementation replaces |completeopt| before opening
" the omnicomplete menu with <C-x><C-o>. In some versions of Vim, the value set
" for the option will not be respected. If you experience issues with Vim
" automatically inserting text while you type, set the following option in
" vimrc, and your issues should go away. >
set completeopt=menu,menuone,popup,noselect,noinsert

" ,t to open up a terminal in a vertical split
nnoremap <leader>t :vert term<CR>

call plug#begin('~/.vim/plugged')

" NERDtree
Plug 'scrooloose/nerdtree'

" database access
Plug 'tpope/vim-dadbod'

" Git gutter marks
Plug 'airblade/vim-gitgutter'

" Markdown support
Plug 'hallison/vim-markdown'

" Rust support
Plug 'rust-lang/rust.vim'

" Handle ANSI escape codes
Plug 'ponzellus/AnsiEsc'

" linting and completion for many languages
Plug 'dense-analysis/ale'

" fzf fuzzy finder
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" writing mode :Goyo
Plug 'junegunn/goyo.vim'

" Go
" Plug 'govim/govim'

Plug 'liuchengxu/vim-clap'

" Initialize plugin system
call plug#end()

" ,g to bring up the Clap popup
nnoremap <silent> <leader>g :Clap grep<CR>

" Format json
nnoremap <leader>j :%!jq ''<CR>
" Format xml
nnoremap <leader>x :%!xmllint --format --encode UTF-8 -<CR>
" Format html
nnoremap <leader>h :%!xmllint --format --encode UTF-8 --html -<CR>
nnoremap <leader>h :%!tidy -utf8 -m -i %<CR>

nnoremap <leader>f :Clap files<CR>

""""""""""""""""""""""""""""""""""
" vim-ale
" 
" If ale is loaded, I want to bind <leader>m to :ALENextWrap. If govim is
" loaded, I'd rather bind it to my own Cnext function (see below)
if &rtp =~ '/ale'
    nnoremap <leader>m :ALENextWrap<cr>
    nnoremap gd :ALEGoToDefinition<cr>

    let g:ale_echo_msg_error_str = 'E'
    let g:ale_echo_msg_warning_str = 'W'
    let g:ale_echo_msg_format = '[%linter%] %code: %%s [%severity%]'

    " don't run ale on every text change, just on every save
    let g:ale_lint_on_text_changed = 'never'

    " https://github.com/prettier/prettier/tree/master/editors/vim#ale
    " enable prettier on save
    let g:ale_fixers = {}
    let g:ale_fixers['javascript'] = ['prettier']
    let g:ale_fixers['typescript'] = ['prettier']
    let g:ale_fixers['typescriptreact'] = ['prettier']
    let g:ale_fixers['python'] = ['black']
    let g:ale_fixers['c'] = ['clang-format']
    let g:ale_fixers['go'] = ['goimports']

    " I think these might be messing the LSP client up?
    let g:ale_fixers['java'] = []

    " to disable on a particular buffer:
    " :let b:ale_fix_on_save=0
    let g:ale_fix_on_save = 1

    " There's no way to only disable typescript, and ale was doing tsserver on all
    " javascript files which is super annoying
    let g:ale_linters = {}
    let g:ale_linters['javascript'] = ['eslint', 'flow', 'prettier', 'prettier-eslint']
    let g:ale_linters['typescript'] = []
    let g:ale_linters['go'] = ['gopls']
    let g:ale_linters['python'] = ['pylint', 'pyls']

    " https://github.com/dense-analysis/ale/blob/d6d2a0c77010db6a75a8942e2af9606971738c23/doc/ale-typescript.txt#L57
    let g:ale_linters_ignore = {'typescript': ['tslint']}

    let g:ale_python_pyls_config = {'pyls': 
            \ {'plugins': 
            \   {'pycodestyle': {'enabled': v:false},
            \    'pydocstyle':  {'enabled': v:false},
            \    'pyflakes':    {'enabled': v:false},
            \    'pylint':      {'enabled': v:false},
            \    'mccabe':      {'enabled': v:false}
            \ }}}

    let g:ale_sign_error = '🔥'

    " Enable completion where available.
    " This setting must be set before ALE is loaded.
    "
    " You should not turn this setting on if you wish to use ALE as a completion
    " source for other completion plugins, like Deoplete.
    " To get ALE to do completion, enable these:
    let g:ale_completion_enabled = 1
    set omnifunc=ale#completion#OmniFunc
endif
"""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""
" begin govim recommended settings
" https://github.com/myitcv/govim/blob/master/cmd/govim/config/minimal.vimrc
"
" Suggestion: By default, govim populates the quickfix window with diagnostics
" reported by gopls after a period of inactivity, the time period being
" defined by updatetime (help updatetime). Here we suggest a short updatetime
" time in order that govim/Vim are more responsive/IDE-like
set updatetime=500

" Suggestion: To make govim/Vim more responsive/IDE-like, we suggest a short
" balloondelay
set balloondelay=250

" Suggestion: Turn on the sign column so you can see error marks on lines
" where there are quickfix errors. Some users who already show line number
" might prefer to instead have the signs shown in the number column; in which
" case set signcolumn=number
"
" Problem: this causes an extra column to pup up when you enter terninal
" normal mode (leader-esc here). Instead of figuring out how to disable it,
" I'm just commenting this out for now
" set signcolumn=yes

" Suggestion: show info for completion candidates in a popup menu
if has("patch-8.1.1904")
  set completepopup=align:menu,highlight:Pmenu
endif


if &rtp =~ 'govim'
    let g:ale_pattern_options = {
    \   '.*\.go$': {
    \       'ale_enabled': 0,
    \        'ale_completion_enabled': 0,
    \   },
    \}

    let g:ale_fixers['go'] = []
    let g:ale_linters['go'] = []

    nnoremap gd :GOVIMGoToDef<cr>

    call govim#config#Set("Staticcheck", 0)
    call govim#config#Set("CompleteUnimported", 1)
    call govim#config#Set("HighlightReferences", 0)

    " Cnext is a modification of:
    " https://vi.stackexchange.com/questions/8534/make-cnext-and-cprevious-loop-back-to-the-begining
    " which is like :cbelow (go to the next error from the quickfix window in the
    " current file) but wraps around to the top of the file when complete
    command! Cnext try | cbelow | catch | cabove 9999 | catch | endtry
    nnoremap <leader>m :Cnext<CR>

    " https://github.com/govim/govim/pull/643
    call sign_define("GOVIMSignErr",{"text":"🔥","texthl":"GOVIMSignErr"})
    call sign_define("GOVIMSignWarn",{"text":"⚠️","texthl":"GOVIMSignWarn"})
    call sign_define("GOVIMSignInfo",{"text":"⁉️","texthl":"GOVIMSignInfo"})
    call sign_define("GOVIMSignHint",{"text":"⁉️","texthl":"GOVIMSignHint"})
endif
" End govim
""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""
" vim-dadbod
"
" To set a default database, the best way is to set the DATABASE_URL
" environment variable. Next best is to set b:db (for a buffer) or g:db
" (globally).
"
" ,se to run a query in visual mode. This command is not documented, but I got
" it from https://github.com/tpope/vim-dadbod/issues/33
vnoremap <expr> <leader>se db#op_exec()

" I don't use this directly, but rather through <leader>se in normal mode. I'm
" not particularly happy with its performance, especially because it does not
" correctly find postgres slash commands correctly, but I wasn't smart enough
" to fix it.
"
" from: https://github.com/tpope/vim-dadbod/issues/33
"
" How it works:
"
" The first search finds the next semicolon. The second marks the spot and
" moves backwards to the previous semicolon or the start of the buffer. The
" third moves forward to the next select|with|insert|update|delete|create.
" Finally, it changes to visual mode and selects to the previous mark.
"
" \v: very magic
" \c: ignore case
" c: include current char in search
" s: set the ' mark at the previous location of the cursor
" W: do not wrap
" z: start search at the cursor column instead of zero
vnoremap <leader>aq <esc>:call search(";", "cWz")<cr>:call search(";\\<bar>\\%^", "bsWz")<cr>:call search("\\v\\c^(select<bar>with<bar>insert<bar>update<bar>delete<bar>create)\>", "Wz")<cr>vg`'

" use ,se in normal mode to try to attempt to guess the current sql statement
" and run it. May not quite select the proper area. I would like to figure out
" how to replace :DB with db#op_exec() but I don't know how
nnoremap <leader>se :normal v<leader>aq<cr>:DB<cr>
"
" end vim-dadbod
""""""""""""""""""""""""""""""""""""
