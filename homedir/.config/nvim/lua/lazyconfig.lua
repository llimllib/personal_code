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
		"preservim/vim-markdown", -- markdown mode: fenced code blocks etc
		"tpope/vim-fugitive", -- git integration
		"tpope/vim-rhubarb", -- github integration
		"digitaltoad/vim-pug", -- jade/pug highlighting
		"lepture/vim-jinja", -- jinja
		"fatih/vim-go", -- go dev. Mainly useful for template syntax
		"NoahTheDuke/vim-just", -- highlighting Justfiles

		-- center a buffer with :NoNeckPain
		-- examples of things you can do:
		-- https://github.com/shortcuts/no-neck-pain.nvim/wiki/Showcase
		{ "shortcuts/no-neck-pain.nvim", version = "*" },

		-- scope highlighting
		{ "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} },

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

		-- otter.nvim for autocomplete in nvim code blocks
		{
			"jmbuhr/otter.nvim",
			dependencies = {
				"hrsh7th/nvim-cmp",
				"neovim/nvim-lspconfig",
				"nvim-treesitter/nvim-treesitter",
			},
		},

		-- colorschemes
		-- "sainnhe/everforest",
		-- https://github.com/Shatur/neovim-ayu
		-- is a neovim rewrite of
		-- https://github.com/ayu-theme/ayu-vim
		-- "Shatur/neovim-ayu",

		-- interesting looking one, nordic:
		--  https://github.com/AlexvZyl/nordic.nvim
		-- "AlexvZyl/nordic.nvim",
		-- "folke/tokyonight.nvim",
		"sainnhe/sonokai", -- the one I currently use, with a custom palette
		"Aliqyan-21/darkvoid.nvim",
		-- "projekt0n/github-nvim-theme",
		-- "0xstepit/flow.nvim",
		-- "habamax/vim-habamax",
		-- /colorschemes

		-- includes a whole bunch of stuff, I'm using it for base16 experiments
		-- at the moment. List of included modules:
		-- https://github.com/echasnovski/mini.nvim/tree/main#modules
		"echasnovski/mini.nvim",

		-- Use Neovim as a language server to inject LSP diagnostics, code
		-- actions, and more via Lua.
		--
		-- allows you to run command line programs like gofmt as if they were
		-- LSP servers
		{
			"nvimtools/none-ls.nvim",
			dependencies = { "nvim-lua/plenary.nvim" },
		},

		-- hex colors
		{
			"norcalli/nvim-colorizer.lua",
			config = function()
				require("colorizer").setup()
			end,
		},

		-- somehow, disabling indentation here makes my indentation go crazy. I
		-- have no idea why.
		--
		-- https://github.com/nvim-treesitter/nvim-treesitter/wiki/Installation#lazynvim
		{
			"nvim-treesitter/nvim-treesitter",
			build = ":TSUpdate",
			event = { "BufReadPre", "BufNewFile" },
			main = "nvim-treesitter.configs",
			config = function()
				require("nvim-treesitter.configs").setup({
					auto_install = false,
					ensure_installed = {
						"bash",
						"c",
						"c_sharp",
						"comment",
						"cpp",
						"css",
						"diff",
						"elixir",
						"erlang",
						"glsl",
						"go",
						"gomod",
						"gosum",
						"gowork",
						"html",
						"htmldjango",
						"ini",
						"java",
						"javascript",
						"json",
						"json5",
						"julia",
						"latex",
						"lua",
						"make",
						"markdown",
						"markdown_inline",
						"prisma",
						"python",
						"query",
						"ruby",
						"rust",
						"scss",
						"sql",
						"starlark",
						"swift",
						"terraform",
						"toml",
						"tsx",
						"typescript",
						"vim",
						"vimdoc",
						"vimdoc",
						"yaml",
						"zig",
					},
					highlight = { enable = true },
					indent = { enable = true },
				})
			end,
		},
		-- https://github.com/LazyVim/LazyVim/blob/d6561fd27c17806ca972cbfc18573ca81d13e346/lua/lazyvim/plugins/ui.lua#L96
		{
			"nvim-lualine/lualine.nvim",
			event = "VeryLazy",
			init = function()
				vim.g.lualine_laststatus = vim.o.laststatus
				if vim.fn.argc(-1) > 0 then
					-- set an empty statusline till lualine loads
					vim.o.statusline = " "
				else
					-- hide the statusline on the starter page
					vim.o.laststatus = 0
				end
			end,
			opts = function()
				local lualine_require = require("lualine_require")
				lualine_require.require = require
				local icons = {
					misc = {
						dots = "󰇘",
					},
					ft = {
						octo = "",
					},
					dap = {
						Stopped = { "󰁕 ", "DiagnosticWarn", "DapStoppedLine" },
						Breakpoint = " ",
						BreakpointCondition = " ",
						BreakpointRejected = { " ", "DiagnosticError" },
						LogPoint = ".>",
					},
					diagnostics = {
						Error = " ",
						Warn = " ",
						Hint = " ",
						Info = " ",
					},
					git = {
						added = " ",
						modified = " ",
						removed = " ",
					},
					kinds = {
						Array = " ",
						Boolean = "󰨙 ",
						Class = " ",
						Codeium = "󰘦 ",
						Color = " ",
						Control = " ",
						Collapsed = " ",
						Constant = "󰏿 ",
						Constructor = " ",
						Copilot = " ",
						Enum = " ",
						EnumMember = " ",
						Event = " ",
						Field = " ",
						File = " ",
						Folder = " ",
						Function = "󰊕 ",
						Interface = " ",
						Key = " ",
						Keyword = " ",
						Method = "󰊕 ",
						Module = " ",
						Namespace = "󰦮 ",
						Null = " ",
						Number = "󰎠 ",
						Object = " ",
						Operator = " ",
						Package = " ",
						Property = " ",
						Reference = " ",
						Snippet = " ",
						String = " ",
						Struct = "󰆼 ",
						TabNine = "󰏚 ",
						Text = " ",
						TypeParameter = " ",
						Unit = " ",
						Value = " ",
						Variable = "󰀫 ",
					},
				}

				vim.o.laststatus = vim.g.lualine_laststatus

				local opts = {
					options = {
						theme = "auto",
						globalstatus = vim.o.laststatus == 3,
						disabled_filetypes = { statusline = { "dashboard", "alpha", "ministarter" } },
					},
					sections = {
						lualine_a = { "mode" },
						lualine_b = { "branch" },

						lualine_c = {
							{ "filename", path = 1 },
							{
								"diagnostics",
								symbols = {
									error = icons.diagnostics.Error,
									warn = icons.diagnostics.Warn,
									info = icons.diagnostics.Info,
									hint = icons.diagnostics.Hint,
								},
							},
							{ "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 } },
						},
						lualine_x = {
							{
								"diff",
								symbols = {
									added = icons.git.added,
									modified = icons.git.modified,
									removed = icons.git.removed,
								},
								source = function()
									local gitsigns = vim.b.gitsigns_status_dict
									if gitsigns then
										return {
											added = gitsigns.added,
											modified = gitsigns.changed,
											removed = gitsigns.removed,
										}
									end
								end,
							},
						},
						lualine_y = {
							{ "progress", separator = " ", padding = { left = 1, right = 0 } },
							{ "location", padding = { left = 0, right = 1 } },
						},
						lualine_z = {
							function()
								return " " .. os.date("%R")
							end,
						},
					},
					inactive_sections = {
						lualine_a = {},
						lualine_b = {},
						lualine_c = { { "filename", path = 1 } },
						lualine_x = { "location" },
						lualine_y = {},
						lualine_z = {},
					},
					extensions = { "lazy" },
				}

				return opts
			end,
		},
		-- https://codecompanion.olimorris.dev/installation.html
		{
			"olimorris/codecompanion.nvim",
			dependencies = {
				"nvim-lua/plenary.nvim",
				"nvim-treesitter/nvim-treesitter",
				"ravitemer/codecompanion-history.nvim",
				"ravitemer/mcphub.nvim",
			},
			config = true,
		},
		{
			"ravitemer/mcphub.nvim",
			dependencies = {
				"nvim-lua/plenary.nvim",
			},
			build = "npm install -g mcp-hub@latest", -- Installs `mcp-hub` node binary globally
			config = function()
				require("mcphub").setup()
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
