colorscheme Base2Tone_SpaceDark
if (has("termguicolors"))
  set termguicolors
endif

let mapleader = ","

" Easier split moving
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-H> <C-W>h
map <C-_> <C-W>_

" terminal mode shortcuts.
" https://neovim.io/doc/user/nvim_terminal_emulator.html
tnoremap <C-h> <C-\><C-N><C-w>h
tnoremap <C-j> <C-\><C-N><C-w>j
tnoremap <C-k> <C-\><C-N><C-w>k
tnoremap <C-l> <C-\><C-N><C-w>l
inoremap <C-h> <C-\><C-N><C-w>h
inoremap <C-j> <C-\><C-N><C-w>j
inoremap <C-k> <C-\><C-N><C-w>k
inoremap <C-l> <C-\><C-N><C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
tnoremap <Esc> <C-\><C-n>

nnoremap <leader>t :vsplit<CR>:terminal<CR>a

"don't highlight searches
set nohls

" Open splits to the right and below by default. Feels more natural
set splitbelow
set splitright

"insert one character
noremap <C-i> i<space><esc>r

"show line
set ruler
set statusline=%f%m%r%h\ [%L]\ [%{&ff}]\ %y%=[%p%%]\ [line:%05l,col:%02v]
set laststatus=2

" system paste and yank
nnoremap <leader>p :pu +<CR>
vnoremap <leader>y "+y

" edit vimrc and source vimrc, respectively
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>es :source $MYVIMRC<cr>

" turn on persistent undo, and store it in the vim dir
set undofile
set undodir=~/.config/nvim/undodir

" default space and tab handling
set shiftwidth=4
set tabstop=4
set expandtab
set softtabstop=4

" fold by indents unless specified otherwise
set foldmethod=indent
set foldlevel=9999

" use the mouse in terminal mode
set mouse=a

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

" keep the cursor from going all the way to the bottom
set scrolloff=5

" Automatically read files which have been changed outside of Vim, if we
" haven't changed it already.
set autoread

" turn off swap files
set nobackup
set nowritebackup
set noswapfile

"" this group is recommended by http://items.sjbach.com/319/configuring-vim-right
set ignorecase
set smartcase
set title
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
runtime macros/matchit.vim

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

" Neovim doesn't seem to save the buffer input mode status, and I don't like
" having to enter insert mode almost every time, so automatically go into
" insert mode when I enter a terminal. Ideally I could just navigate around
" without exiting insert mode, but it is what it is.
au BufEnter * if &buftype == 'terminal' | :startinsert | endif

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" requires nvim 0.5.0 (unreleased as of Nov 4 2020)
Plug 'neovim/nvim-lspconfig'

Plug 'nvim-lua/completion-nvim'

" Initialize plugin system
call plug#end()

""""
"""" Configure LSP
""""
" https://github.com/neovim/nvim-lspconfig
" https://github.com/nvim-lua/completion-nvim
"
" Configure nvim's native LSP client. Choose which languages are enabled, and
" configure them
" Pretty good docs on using lua in neovim:
" https://github.com/nanotee/nvim-lua-guide#where-to-put-lua-files
lua require('config_lsp')

" Wait 100ms before running cursorhold (shows diagnostic messages)
set updatetime=100

" Auto-format *.py files prior to saving them
autocmd BufWritePre *.py lua vim.lsp.buf.formatting_sync(nil, 1000)
autocmd BufWritePre *.go lua vim.lsp.buf.formatting_sync(nil, 1000)
autocmd BufWritePre *.ts lua vim.lsp.buf.formatting_sync(nil, 1000)
autocmd BufWritePre *.js lua vim.lsp.buf.formatting_sync(nil, 1000)

" Create a shortcut to run the formatter
nnoremap <leader>f :lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>

nnoremap <leader>m <cmd>lua vim.lsp.diagnostic.goto_next()<CR>
nnoremap <leader>M <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>

" from :help set_signs
sign define LspDiagnosticsSignError text=‡ texthl=LspDiagnosticsSignError linehl= numhl=
sign define LspDiagnosticsSignWarning text=† texthl=LspDiagnosticsSignWarning linehl= numhl=
sign define LspDiagnosticsSignInformation text=※ texthl=LspDiagnosticsSignInformation linehl= numhl=
sign define LspDiagnosticsSignHint text=⁂ texthl=LspDiagnosticsSignHint linehl= numhl=

" From the vim-lsp docs:
" - Q: How to force-reload LSP?
"   A: Stop all clients, then reload the buffer.
"
"   :lua vim.lsp.stop_client(vim.lsp.get_active_clients())
"   :edit
"
" Unfortunately, this does not work when you have a dirty buffer, so you have
" to save or undo to reload the LSP server. I'm not quite sure what's the best
" way to handle this - it works better in vim
nnoremap <leader>r <cmd>lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR> :edit<CR>

nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gh    <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> gs    <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
""""" End LSP Config

""""
"""" completion-nvim config
"""" https://github.com/nvim-lua/completion-nvim#configuration
""""
" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

" disable auto popup
let g:completion_enable_auto_popup = 0

" use <tab> to trigger popup
nmap <tab> <Plug>(completion_smart_tab)
nmap <s-tab> <Plug>(completion_smart_s_tab)
imap <tab> <Plug>(completion_smart_tab)
imap <s-tab> <Plug>(completion_smart_s_tab)
"""" end completion-nvim config

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
  autocmd FileType go set ts=4 sw=4 sts=4 noet nolist
  autocmd FileType smarty set syntax=gotexttmpl
  autocmd FileType proto set ts=2 sts=2 sw=2
augroup END

" Format json
nnoremap <leader>j :%!jq ''<CR>
" Format xml
nnoremap <leader>x :%!xmllint --format --encode UTF-8 -<CR>
" Format html
nnoremap <leader>h :%!tidy -utf8 -m -i %<CR>
