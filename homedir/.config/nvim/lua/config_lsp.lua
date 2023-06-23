-- config docs: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
--
-- To install all prerequisite language servers and formatters:
-- brew install efm-langserver shellcheck stylua
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
local cmp = require("cmp")

-- https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings
local has_words_before = function()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

-- https://github.com/hrsh7th/nvim-cmp/wiki/Menu-Appearance
local kind_icons = {
	Text = "",
	Method = "",
	Function = "",
	Constructor = "",
	Field = "",
	Variable = "",
	Class = "ﴯ",
	Interface = "",
	Module = "",
	Property = "ﰠ",
	Unit = "",
	Value = "",
	Enum = "",
	Keyword = "",
	Snippet = "",
	Color = "",
	File = "",
	Reference = "",
	Folder = "",
	EnumMember = "",
	Constant = "",
	Struct = "",
	Event = "",
	Operator = "",
	TypeParameter = "",
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
	sources = { { name = "nvim_lsp" }, { name = "buffer" }, { name = "path" } },
	formatting = {
		format = function(entry, vim_item)
			-- Kind icons
			vim_item.kind = string.format("%s %s", kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
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
})
local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

-- I might want to use vim.lsp.buf.formatting_seq_sync() ? read about that
-- here:
-- https://www.reddit.com/r/neovim/comments/vvtltr/remove_the_message_select_a_language_server/iflxdv8/
local on_attach = function(client, bufnr)
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
end

-- XXX: how to switch between the two as necesssary?
-- npm install -g typescript typescript-language-server
lsp.tsserver.setup({
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
	-- don't format files, I prefer using prettier
	settings = { documentFormatting = false },
	capabilities = capabilities,
})

-- npm install vscode-langservers-extracted
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#eslint
lsp.eslint.setup({
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
	-- don't format files, I prefer using prettier
	settings = { documentFormatting = false },
	capabilities = capabilities,
})

-- go install golang.org/x/tools/gopls@latest
lsp.gopls.setup({
	on_attach = function(client, bufnr)
		-- don't format files, I prefer using null-ls for this
		client.server_capabilities.document_formatting = false

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

-- gem install solargraph
lsp.solargraph.setup({ on_attach = on_attach, capabilities = capabilities })

-- use the proper pyright version. From:
-- https://github.com/neovim/nvim-lspconfig/issues/500#issuecomment-851247107
-- https://github.com/neovim/nvim-lspconfig/issues/500#issuecomment-876700701
-- https://github.com/ecly/dotfiles/blob/f2ad429f3ee2c75b4726ce803d8a7293b6aa29c5/.vim/lua/core/plugins/lsp/utils.lua#L13
local util = require("lspconfig/util")

local function get_python_path(workspace)
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
lsp.pyright.setup({
	on_attach = on_attach,
	capabilities = capabilities,
	before_init = function(_, config)
		config.settings.python.pythonPath = get_python_path(config.root_dir)
	end,
})

lsp.elixirls.setup({
	on_attach = on_attach,
	cmd = { "/opt/elixir-ls/language_server.sh" },
	capabilities = capabilities,
})

lsp.clangd.setup({
	on_attach = on_attach,
	capabilities = capabilities,
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

lsp.zls.setup({ on_attach = on_attach, capabilities = capabilities })

lsp.terraformls.setup({
	on_attach = function(client, bufnr)
		-- terraformls doesn't seem to do formatting? So use null_ls instead
		client.server_capabilities.document_formatting = false

		on_attach(client, bufnr)
	end,
	cmd = { "terraform-ls", "serve" },
})

lsp.tflint.setup({ on_attach = on_attach, capabilities = capabilities })

lsp.bashls.setup({ on_attach = on_attach, capabilities = capabilities })

lsp.cssls.setup({ on_attach = on_attach, capabilities = capabilities })

lsp.html.setup({
	on_attach = function(client, bufnr)
		-- don't format files, I prefer using prettier
		client.server_capabilities.document_formatting = false

		on_attach(client, bufnr)
	end,
	capabilities = capabilities,
})

lsp.csharp_ls.setup({ on_attach = on_attach, capabilities = capabilities })

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

-- you can skip formatting on a single write by using :noautocmd w (abbreviated
-- :noa w)
null_ls = require("null-ls")
null_ls.setup({
	-- this on_attach function sets null-ls to do document formatting on save
	--
	-- see:
	-- https://github.com/jose-elias-alvarez/null-ls.nvim/wiki/Formatting-on-save
	on_attach = function(client, bufnr)
		if client.supports_method("textDocument/formatting") then
			vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
			vim.api.nvim_create_autocmd("BufWritePre", {
				group = augroup,
				buffer = bufnr,
				callback = function()
					-- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
					-- (note: I'm currently on 0.7.2, 7/7/22)
					vim.lsp.buf.format()
				end,
			})

			-- null-ls messes with formatexpr for some reason, which messes up `gq`
			-- https://github.com/jose-elias-alvarez/null-ls.nvim/issues/1131
			vim.api.nvim_buf_set_option(bufnr, "formatexpr", "")
		end
	end,
	-- documentaton on using local executables:
	-- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/a138b14099e9623832027ea12b4631ddd2a49256/doc/BUILTIN_CONFIG.md?plain=1#L358-L392
	sources = {
		null_ls.builtins.formatting.black.with({
			prefer_local = ".venv/bin",
		}),
		null_ls.builtins.formatting.clang_format,
		null_ls.builtins.formatting.goimports,
		null_ls.builtins.formatting.gofumpt,
		null_ls.builtins.formatting.prettier.with({
			prefer_local = "node_modules/.bin",
		}),
		null_ls.builtins.formatting.stylua,
		null_ls.builtins.formatting.terraform_fmt,
	},
	root_dir = lsp.util.root_pattern("yarn.lock", ".git"),
	-- enable this and run :NullLsLog to see a detailed log
	-- debug = true,
})

-- There are many config options, let's roll with the default until it annoys
-- us:
-- https://github.com/lewis6991/gitsigns.nvim/tree/addd6e174a85fc1c4007ab0b65d77e6555b417bf#usage
require("gitsigns").setup()
