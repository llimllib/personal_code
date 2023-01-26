-- Specify a directory for plugins
-- - For Neovim: stdpath('data') . '/plugged'
-- - Avoid using standard Vim directory names like 'plugin'
-- 
-- ~/.local/share/nvim/plugged
local Plug = vim.fn['plug#']
local data = vim.fn.stdpath('data')
vim.call('plug#begin', data .. '/plugged')

Plug 'neovim/nvim-lspconfig'
Plug 'junegunn/goyo.vim' -- focus mode
Plug 'kyazdani42/nvim-web-devicons' -- fancy icons
Plug 'hashivim/vim-terraform' -- terraform highlighting
Plug 'elixir-editors/vim-elixir' -- elixir highlighting
Plug 'ziglang/zig.vim' -- zig highlighting
Plug 'gpanders/editorconfig.nvim' -- .editorconfig reading
Plug 'tpope/vim-dadbod' -- query databases
Plug 'lewis6991/gitsigns.nvim' -- git gutter signs

-- handy lua functions for nvim; required for telescope.vim
Plug 'nvim-lua/plenary.nvim'

-- better file name matching for telescope
Plug('nvim-telescope/telescope-fzf-native.nvim', {['do'] = 'make'})
Plug 'nvim-telescope/telescope.nvim' -- fancy fuzzy finder

-- install nvim-cmp, for code completion
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'

-- I don't use snippets or know what they are, but nvim-cmp
-- requires this, so whatever I guess
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'

-- colorschemes
Plug 'sainnhe/everforest'
-- https://github.com/Shatur/neovim-ayu
-- is a neovim rewrite of
-- https://github.com/ayu-theme/ayu-vim
Plug 'Shatur/neovim-ayu'
-- interesting looking one, nordic:
--  https://github.com/AlexvZyl/nordic.nvim
--
-- /colorschemes

-- null-ls is an attempt to simplify the process of creating,
-- sharing, and setting up LSP sources using pure Lua.
--
-- depends on plenary
Plug 'jose-elias-alvarez/null-ls.nvim'

vim.call('plug#end')
