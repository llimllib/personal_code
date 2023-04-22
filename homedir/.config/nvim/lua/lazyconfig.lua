local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- installs to ~/.local/share/nvim/lazy/
require("lazy").setup({
	spec = {
		"neovim/nvim-lspconfig",
		"junegunn/goyo.vim", -- focus mode
		"kyazdani42/nvim-web-devicons", -- fancy icons
		"hashivim/vim-terraform", -- terraform highlighting
		"elixir-editors/vim-elixir", -- elixir highlighting
		"ziglang/zig.vim", -- zig highlighting
		"cappyzawa/starlark.vim", -- starlark highlighting
		"gpanders/editorconfig.nvim", -- .editorconfig reading
		"tpope/vim-dadbod", -- query databases
		"lewis6991/gitsigns.nvim", -- git gutter signs

		-- handy lua functions for nvim; required for telescope.vim
		"nvim-telescope/telescope-fzf-native.nvim", -- fancy fuzzy finder
		{
			"nvim-telescope/telescope.nvim",
			dependencies = {
				"nvim-lua/plenary.nvim",
				-- better file name matching for telescope
				{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			},
		},

		-- install nvim-cmp, for code completion
		{
			"hrsh7th/nvim-cmp",
			dependencies = {
				"hrsh7th/cmp-nvim-lsp",
				"hrsh7th/cmp-buffer",
				"hrsh7th/cmp-path",
				"hrsh7th/cmp-cmdline",
				-- I don't use snippets or know what they are, but nvim-cmp
				-- requires this, so whatever I guess
				"hrsh7th/cmp-vsnip",
				"hrsh7th/vim-vsnip",
			},
		},

		-- colorschemes
		"sainnhe/everforest",
		-- https://github.com/Shatur/neovim-ayu
		-- is a neovim rewrite of
		-- https://github.com/ayu-theme/ayu-vim
		"Shatur/neovim-ayu",

		-- interesting looking one, nordic:
		--  https://github.com/AlexvZyl/nordic.nvim
		"AlexvZyl/nordic.nvim",
		"folke/tokyonight.nvim",

		{ dir = "~/code/adhoc-nvim-colors" },
		-- /colorschemes

		-- includes a whole bunch of stuff, I'm using it for base16 experiments
		-- at the moment. List of included modules:
		-- https://github.com/echasnovski/mini.nvim/tree/main#modules
		"echasnovski/mini.nvim",

		-- null-ls is an attempt to simplify the process of creating,
		-- sharing, and setting up LSP sources using pure Lua.
		{
			"jose-elias-alvarez/null-ls.nvim",
			dependencies = { "nvim-lua/plenary.nvim" },
		},

		-- hex colors
		{
			"norcalli/nvim-colorizer.lua",
			config = function()
				require("colorizer").setup()
			end,
		},
	},
	defaults = {
		-- By default, only LazyVim plugins will be lazy-loaded. Your custom
		-- plugins will load during startup. If you know what you're doing, you
		-- can set this to `true` to have all your custom plugins lazy-loaded
		-- by default.
		lazy = false,

		-- It's recommended to leave version=false for now, since a lot the
		-- plugin that support versioning, have outdated releases, which may
		-- break your Neovim install.
		version = false,
	},
})
