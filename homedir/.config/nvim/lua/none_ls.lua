-- you can skip formatting on a single write by using :noautocmd w (abbreviated
-- :noa w)
--
-- we're using "none-ls", the replacement for null-ls, but it still gets
-- imported as null-ls, confusingly
local lsp = require("lspconfig")
local null_ls = require("null-ls")
local util = require("lspconfig.util")

local is_dprint_available = function(fname)
	local root = util.find_git_ancestor(fname) or util.root_pattern("package.json", "tsconfig.json")(fname)
	if not root then
		return false
	end

	local dprint_path = util.path.join(root, "node_modules", ".bin", "dprint")
	return vim.fn.executable(dprint_path) == 1 or vim.fn.executable(dprint_path .. ".cmd") == 1
end

local is_biome_available = function(fname)
	local root = util.find_git_ancestor(fname) or util.root_pattern("package.json", "tsconfig.json")(fname)
	if not root then
		return false
	end

	local dprint_path = util.path.join(root, "node_modules", ".bin", "biome")
	return vim.fn.executable(dprint_path) == 1 or vim.fn.executable(dprint_path .. ".cmd") == 1
end

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
	-- https://github.com/nvimtools/none-ls.nvim/blob/72e25ed4/doc/BUILTIN_CONFIG.md?plain=1#L351-L385
	sources = {
		-- formatters
		null_ls.builtins.formatting.black.with({
			prefer_local = ".venv/bin",
		}),
		null_ls.builtins.formatting.clang_format,
		null_ls.builtins.formatting.goimports,
		null_ls.builtins.formatting.gofumpt,
		null_ls.builtins.formatting.prettier.with({
			prefer_local = "node_modules/.bin",
			-- Skip prettier if this is a dprint-supported filetype and dprint is available
			condition = function(utils)
				local dprint_filetypes =
					{ "javascript", "typescript", "typescriptreact", "javascriptreact", "json", "markdown" }
				if vim.tbl_contains(dprint_filetypes, vim.bo.filetype) and is_dprint_available(utils.bufname) then
					return false
				end
				return true
			end,
		}),
		null_ls.builtins.formatting.stylua,
		null_ls.builtins.formatting.terraform_fmt,

		-- diagnostics
		null_ls.builtins.diagnostics.actionlint.with({
			condition = function()
				local cwd = vim.fn.expand("%:p:.")
				return cwd:find(".github/workflows")
			end,
		}),
		null_ls.builtins.diagnostics.golangci_lint,
	},
	root_dir = lsp.util.root_pattern("yarn.lock", ".git"),
	-- enable this and run :NullLsLog to see a detailed log
	-- debug = true,
})

-- used in config_lsp.lua
return {
	is_biome_available = is_biome_available,
}
