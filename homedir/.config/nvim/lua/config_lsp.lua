-- config docs: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
--
-- To install all prerequisite language servers and formatters:
-- brew install efm-langserver gleam rust-analyzer shellcheck stylua taplo
-- dotnet tool install --global csharp-ls
-- gem install solargraph
-- go install golang.org/x/tools/gopls@latest
-- go install mvdan.cc/gofumpt@latest
-- npm install -g typescript typescript-language-server prettier bash-language-server vscode-langservers-extracted
-- pip install pyright black
-- build zls manually: https://github.com/zigtools/zls/wiki/Downloading-and-Building-ZLS#cloning-with-git
--
-- add each npm package to ~/.config/asdf/default-npm-packages so they get
-- installed in each npm version globally; see bashrc for where I set that
-- filename, it doesn't check XDG dirs by default
--
-- same for python, ~/.config/asdf/default-python-packages

local lsp = require("lspconfig")
local util = require("lspconfig.util")
local cmp = require("cmp")
local none_ls = require("none_ls")

-- https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings
local has_words_before = function()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

-- https://github.com/hrsh7th/nvim-cmp/wiki/Menu-Appearance
local kind_icons = {
	Text = "",
	Method = "󰆧",
	Function = "󰊕",
	Constructor = "",
	Field = "󰇽",
	Variable = "󰂡",
	Class = "󰠱",
	Interface = "",
	Module = "",
	Property = "󰜢",
	Unit = "",
	Value = "󰎠",
	Enum = "",
	Keyword = "󰌋",
	Snippet = "",
	Color = "󰏘",
	File = "󰈙",
	Reference = "",
	Folder = "󰉋",
	EnumMember = "",
	Constant = "󰏿",
	Struct = "",
	Event = "",
	Operator = "󰆕",
	TypeParameter = "󰅲",
}

-- setup code completion
--
-- copied and modified from
-- https://github.com/tomaskallup/dotfiles/blob/ceb58fec84126c3764776672b4649ecb1a0f76a5/nvim/lua/plugins/nvim-cmp.lua
cmp.setup({
	completion = { autocomplete = false },
	-- I don't use snippets or know what they are, but nvim-cmp requires this, so
	-- whatever I guess
	snippet = {
		expand = function(args)
			vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
		end,
	},
	mapping = {
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-y>"] = cmp.mapping.confirm({
			select = true,
			behavior = cmp.ConfirmBehavior.Insert,
		}),
		["<C-Space>"] = cmp.mapping.complete(),
		["<Tab>"] = function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif has_words_before() then
				cmp.complete()
			else
				fallback()
			end
		end,
		["<S-Tab>"] = function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			else
				fallback()
			end
		end,
		["<CR>"] = cmp.mapping.confirm({
			select = true,
			behavior = cmp.ConfirmBehavior.Insert,
		}),
		["<up>"] = function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			else
				fallback()
			end
		end,
		["<down>"] = function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			else
				fallback()
			end
		end,
	},
	-- sources = { { name = "nvim_lsp" }, { name = "buffer" }, { name = "path" }, { name = "otter" } },
	sources = { { name = "nvim_lsp" }, { name = "path" }, { name = "otter" } },
	formatting = {
		format = function(entry, vim_item)
			-- Kind icons
			vim_item.kind = string.format("%s %s", kind_icons[vim_item.kind], vim_item.kind)
			-- Source
			vim_item.menu = ({
				buffer = "[Buffer]",
				nvim_lsp = "[LSP]",
				nvim_lua = "[Lua]",
				latex_symbols = "[LaTeX]",
			})[entry.source.name]
			return vim_item
		end,
	},
	preselect = { cmp.PreselectMode.None },
	-- enable if you like a border around the completion window
	-- window = {
	-- 	completion = cmp.config.window.bordered(),
	-- 	documentation = cmp.config.window.bordered(),
	-- },
})
local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

-- I might want to use vim.lsp.buf.formatting_seq_sync() ? read about that
-- here:
-- https://www.reddit.com/r/neovim/comments/vvtltr/remove_the_message_select_a_language_server/iflxdv8/
local on_attach = function(client, bufnr, opts)
	opts = opts or {}

	-- https://github.com/nvim-lua/diagnostic-nvim/issues/29#issuecomment-638040064
	-- If you want to have the diagnostic information come up on hover, uncomment this:
	-- vim.api.nvim_command('autocmd CursorHold <buffer> lua vim.lsp.util.show_line_diagnostics()')
	--
	-- I thought I wanted that, but it turned out to be a bit annoying. Do
	-- <leader>m instead to go to the next error. What I really want is text in
	-- the command bar on hover only, instead of virtual text, I think? That's
	-- what ALE has... not sure if I'm just being habitual or I actually like
	-- that
	--
	-- the next three lines are from https://github.com/hrsh7th/nvim-cmp/wiki/Language-Server-Specific-Samples#golang-gopls
	local function buf_set_keymap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end
	local function buf_set_option(...)
		vim.api.nvim_buf_set_option(bufnr, ...)
	end

	buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

	-- use none-ls instead of any LSP to do document formatting, unless an LSP
	-- explicitly opts in with document_formatting=true
	if opts.document_formatting ~= true then
		client.server_capabilities.document_formatting = false
	end
end

-- https://github.com/yioneko/vtsls
-- npm install -g @vtsls/language-server
vim.lsp.enable("vtsls", {
	on_attach = function(client, bufnr)
		-- don't format files, I prefer using prettier
		client.server_capabilities.document_formatting = false

		-- lsp sets formatexpr here even though I tell it not to use prettier
		-- for document formatting. Unset it so that `gq` works.
		--
		-- https://github.com/jose-elias-alvarez/null-ls.nvim/issues/1131
		vim.api.nvim_buf_set_option(bufnr, "formatexpr", "")

		on_attach(client, bufnr)
	end,
	-- don't format files, I prefer using prettier (I'm not even sure that's a
	-- thing vtsls does?)
	settings = {
		-- These make vtsls try to go to the actual source code instead of just
		-- showing the .d.ts
		-- https://github.com/yioneko/vtsls/blob/bbe6d6f3b/packages/service/configuration.schema.json#L1025-L1029
		--
		-- I like having these turned on, but with them on I don't know how to
		-- get to the types instead! More research needed so I can get around
		-- my code like I want.
		-- javascript = {
		-- 	preferGoToSourceDefinition = true,
		-- },
		-- typescript = {
		-- 	preferGoToSourceDefinition = true,
		-- },
		documentFormatting = false,
	},
	capabilities = capabilities,
})

-- npm install -g @biomejs/biome
-- For now, the projects I use that use biome use it as a formatter as well
-- instead of prettier, so enable formatting from it. See none_ls.lua
vim.lsp.enable("biome")
vim.lsp.config("biome", {
	capabilities = capabilities,
	on_attach = function(client, bufnr)
		on_attach(client, bufnr, { document_formatting = true })
	end,
	root_dir = function(bufnr, on_dir)
		-- run biome if we're in a git repo and biome.json[c] is present
		local fname = vim.fn.bufname(bufnr)
		if util.root_pattern("biome.json", "biome.jsonc")(fname) then
			on_dir(vim.fn.getcwd())
		end
	end,
	settings = { documentFormatting = false },
})

-- npm install vscode-langservers-extracted
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#eslint
vim.lsp.enable("eslint")
vim.lsp.config("eslint", {
	on_attach = on_attach,
	-- don't format files, this is delegated to none_ls
	settings = {
		documentFormatting = false,
		format = true,
	},
	capabilities = capabilities,
})

-- Disable vim-go's LSP-like features but keep syntax highlighting; we'll set
-- up gopls right after
vim.g.go_def_mapping_enabled = 0 -- Disable go to definition mapping
vim.g.go_gopls_enabled = 0 -- Disable gopls integration in vim-go
vim.g.go_code_completion_enabled = 0 -- Disable code completion
vim.g.go_doc_keywordprg_enabled = 0 -- Disable K for documentation
vim.g.go_implements_mode = "" -- Disable :GoImplements
vim.g.go_referrers_mode = "" -- Disable :GoReferrers

-- go install golang.org/x/tools/gopls@latest
vim.lsp.config("gopls", {
	on_attach = function(client, bufnr)
		on_attach(client, bufnr)
	end,
	capabilities = capabilities,
	settings = {
		gopls = {
			analyses = { unusedparams = true, shadow = true },
			staticcheck = true,
			-- use null-ls for this instead
			gofumpt = false,
		},
	},
	init_options = { usePlaceholders = false },
})
vim.lsp.enable("gopls")

-- gem install solargraph
vim.lsp.config("solargraph", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("solargraph")

-- use the proper pyright version. From:
-- https://github.com/neovim/nvim-lspconfig/issues/500#issuecomment-851247107
-- https://github.com/neovim/nvim-lspconfig/issues/500#issuecomment-876700701
-- https://github.com/ecly/dotfiles/blob/f2ad429f3ee2c75b4726ce803d8a7293b6aa29c5/.vim/lua/core/plugins/lsp/utils.lua#L13
local function get_python_path(workspace)
	workspace = workspace or vim.fn.getcwd()

	-- Use activated virtualenv.
	if vim.env.VIRTUAL_ENV then
		print(vim.env.VIRTUAL_ENV)
		return util.path.join(vim.env.VIRTUAL_ENV, "bin", "python")
	end

	-- if a .venv exists in the dir, use that as the virtualenv even if it's
	-- not activated
	local match = vim.fn.glob(util.path.join(workspace, ".venv"))
	if match ~= "" then
		return util.path.join(match, "bin", "python")
	end

	-- Fallback to system Python.
	return vim.fn.exepath("python3") or vim.fn.exepath("python") or "python"
end

-- pip install pyright
vim.lsp.config("pyright", {
	on_attach = on_attach,
	capabilities = capabilities,
	before_init = function(_, config)
		config.settings.python.pythonPath = get_python_path(config.root_dir)
	end,
})
vim.lsp.enable("pyright")

-- https://docs.astral.sh/ty/editors/
-- uv tool install ty@latest
--
-- I tried this out, but it failed to give me typings on the pandas file
-- I tried it on - everything was just "unknown". Try again later maybe
-- vim.lsp.config("ty", {
-- 	settings = {
-- 		ty = {
-- 			-- ty language server settings go here
-- 		},
-- 	},
-- })
--
-- -- Required: Enable the language server
-- vim.lsp.enable("ty")

vim.lsp.config("elixirls", {
	on_attach = on_attach,
	cmd = { "~/.local/share/elixir-ls/language_server.sh" },
	capabilities = capabilities,
})
vim.lsp.enable("elixirls")

vim.lsp.config("clangd", {
	on_attach = on_attach,
	-- fixes multiple encodings error. taken from:
	-- https://github.com/LazyVim/LazyVim/blob/530e94a9/lua/lazyvim/plugins/extras/lang/clangd.lua#L67C1-L69C13
	capabilities = {
		offsetEncoding = { "utf-16" },
	},
	cmd = {
		"clangd",
		"-log=verbose",
		"-pretty",
		"--header-insertion=iwyu",
		"--suggest-missing-includes",
		"-j=4",
		"--all-scopes-completion",
		"--background-index=0",
		"--clang-tidy",
	},
})
vim.lsp.enable("clangd")

vim.lsp.config("zls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("zls")

vim.lsp.config("terraformls", {
	on_attach = function(client, bufnr)
		on_attach(client, bufnr)
	end,
	cmd = { "terraform-ls", "serve" },
})
vim.lsp.enable("terraformls")

vim.lsp.config("tflint", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("tflint")

vim.lsp.config("bashls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("bashls")

vim.lsp.config("cssls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("cssls")

-- swift lsp. executes "sourcekit-lsp"
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
vim.lsp.config("sourcekit", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("sourcekit", { on_attach = on_attach, capabilities = capabilities })

vim.lsp.config("texlab", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("texlab", { on_attach = on_attach, capabilities = capabilities })

vim.lsp.config("html", {
	on_attach = function(client, bufnr)
		on_attach(client, bufnr)
	end,
	capabilities = capabilities,
})
vim.lsp.enable("html")

vim.lsp.config("csharp_ls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("csharp_ls")

vim.lsp.config("rust_analyzer", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("rust_analyzer")

-- for TOML files
vim.lsp.config("taplo", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.enable("taplo")

-- gleam: https://gleam.run/language-server/#neovim
vim.lsp.config("gleam", {
	on_attach = function(client, bufnr)
		on_attach(client, bufnr, { document_formatting = true })
	end,
	capabilities = capabilities,
})
vim.lsp.enable("gleam")

vim.lsp.config("rust_analyzer", {
	on_attach = function(client, bufnr)
		on_attach(client, bufnr, { document_formatting = true })
	end,
	capabilities = capabilities,
})
vim.lsp.enable("rust_analyzer")

-- configure diagnostics
vim.diagnostic.config({
	-- set to false to disable virtual text displays
	virtual_text = {
		-- the default is '■' but I find that too distracting
		prefix = "•",
	},

	-- configured with the `sign` command in init.vim, as suggested in :help
	-- set_signs
	signs = true,

	-- don't update diagnostics while we're in insert mode
	-- (Not sure if I'd rather wait until save, or if that's possible)
	update_in_insert = false,
})

-- There are many config options, let's roll with the default until it annoys
-- us:
-- https://github.com/lewis6991/gitsigns.nvim/tree/addd6e174a85fc1c4007ab0b65d77e6555b417bf#usage
require("gitsigns").setup()
