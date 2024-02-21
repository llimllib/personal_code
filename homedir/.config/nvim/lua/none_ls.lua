-- you can skip formatting on a single write by using :noautocmd w (abbreviated
-- :noa w)
--
-- we're using "none-ls", the replacement for null-ls, but it still gets
-- imported as null-ls, confusingly
local lsp = require("lspconfig")
local null_ls = require("null-ls")

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
					vim.lsp.buf.format({ bufnr = bufnr })
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
