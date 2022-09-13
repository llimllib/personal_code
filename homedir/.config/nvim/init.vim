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

" edit vimrc, source vimrc and edit lua config
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>es :source $MYVIMRC<cr>
nnoremap <leader>el :vsplit ~/.config/nvim/lua/config_lsp.lua<cr>

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
set list listchars=tab:→\ ,trail:·,nbsp:⎵

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
"
" ~/.local/share/nvim/plugged
call plug#begin(stdpath('data') . '/plugged')

Plug 'neovim/nvim-lspconfig'

Plug 'junegunn/goyo.vim'

" fancy icons
Plug 'kyazdani42/nvim-web-devicons'

" handy lua functions for nvim;
" required for telescope.vim
Plug 'nvim-lua/plenary.nvim'

" better file name matching for telescope
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }

" fancy fuzzy finder
Plug 'nvim-telescope/telescope.nvim'

" terraform syntax highlighting. Includes :Terraform command I don't use
Plug 'hashivim/vim-terraform'

" Elixir highlighting
Plug 'elixir-editors/vim-elixir'

" install nvim-cmp, for code completion
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'

" I don't use snippets or know what they are, but nvim-cmp requires this, so
" whatever I guess
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'

" zig language syntax
Plug 'ziglang/zig.vim'

" https://github.com/sainnhe/everforest/blob/master/doc/everforest.txt
Plug 'sainnhe/everforest'

" null-ls is an attempt to bridge that gap and simplify the process of
" creating, sharing, and setting up LSP sources using pure Lua.
"
" depends on plenary
Plug 'jose-elias-alvarez/null-ls.nvim'

Plug 'simrat39/symbols-outline.nvim'

" Initialize plugin system
call plug#end()

""""
"""" Configure colors. Must come after vim-plug since that installs the color
""""                   scheme
""""
if (has("termguicolors"))
  set termguicolors
endif
colorscheme everforest


""""
"""" Configure LSP
""""

" load my lua lsp config
lua require('config_lsp')

" Wait 100ms before running cursorhold (shows diagnostic messages)
set updatetime=100

" Auto-format files prior to saving them
" TODO: these should probably be in an augroup, for reasons that I do not
" remember?
" TODO: set up a command shortcut to disable auto-formatting on write
" TODO: possibly set up a shortcut in the lua config that just always calls
" lsp.buf.formatting_sync if it's enabled? cf:
" https://www.mitchellhanberg.com/how-to-set-up-neovim-for-elixir-development/
" , which does it in the on_complete function with:
" map("n", "df", "<cmd>lua vim.lsp.buf.formatting()<cr>", map_opts)
"
" to write a file without running the formatter, do:
" :noautocmd w
" autocmd BufWritePre *.py lua vim.lsp.buf.formatting_sync(nil, 1000)
" autocmd BufWritePre *.ts lua vim.lsp.buf.formatting_sync(nil, 1000)
autocmd BufWritePre *.ex lua vim.lsp.buf.formatting_sync(nil, 1000)
" autocmd BufWritePre *.go lua vim.lsp.buf.formatting_sync(nil, 1000)
" for javascript, try to use prettier instead of tsserver's built-in formatter

" Create a shortcut to run the formatter
nnoremap <leader>fa :lua vim.lsp.buf.formatting_sync(nil, 1000)<CR>
" let's try using Neoformat instead
" nnoremap <leader>f :Neoformat<CR>

" To enable auto-formatting, uncomment this, but I'm going to run with manual
" formatting for now:
" augroup fmt
"   autocmd!
"   autocmd BufWritePre * undojoin | Neoformat
" augroup END

nnoremap <leader>m <cmd>lua vim.diagnostic.goto_next()<CR>
nnoremap <leader>M <cmd>lua vim.diagnostic.goto_prev()<CR>

" from :help set_signs
"
" The following are deprecated without replacement. These functions are moved
" internally and are no longer exposed as part of the API. Instead, use
" |vim.diagnostic.config()| and |vim.diagnostic.show()|.
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
"
" TODO: move this into lua?
nnoremap <leader>r <cmd>lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR> :edit<CR>

nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
" c-t means "up the tag stack" and is the opposite of gd, so map it to
" something more convenient
nnoremap <silent> gh    <c-t>
nnoremap <silent> gt    <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> gs    <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gR    <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
""""" End LSP Config

""""" configure nvim-cmp (code completion)
set completeopt=menu,menuone,noselect
""""" end nvim-cmp

" use neovim's lua file type detection, which should improve startup time
" https://gpanders.com/blog/whats-new-in-neovim-0-7/#filetypelua
let g:do_filetype_lua = 1 
let g:did_load_filetypes = 0

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
nnoremap <leader>h :%!tidy -utf8 -q --show-body-only true -f /tmp/tidyerrors -i %<CR>
" format code generally

autocmd! User GoyoEnter nested set linebreak
autocmd! User GoyoLeave nested set nolinebreak

" configure telescope
" https://github.com/nvim-telescope/telescope.nvim#usage
" Find files using Telescope command-line sugar.
"
" <c-x> opens in a split
" <c-v> opens in a vsplit
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

lua << EOF
require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}

-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')
EOF
