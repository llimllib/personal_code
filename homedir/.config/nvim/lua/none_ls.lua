-- you can skip formatting on a single write by using :noautocmd w (abbreviated
-- :noa w)
--
-- we're using "none-ls", the replacement for null-ls, but it still gets
-- imported as null-ls, confusingly
local lsp = require("lspconfig")
local null_ls = require("null-ls")
local util = require("lspconfig.util")

local is_in_node_bin = function(fname, program)
	local root = util.find_git_ancestor(fname) or util.root_pattern("package.json", "tsconfig.json")(fname)
	if not root then
		return false
	end

	local binpath = util.path.join(root, "node_modules", ".bin", program)
	return vim.fn.executable(binpath) == 1
end

-- Format python on save. Prefer ruff, fall back to black. A project can
-- override the choice with NVIM_PYTHON_FORMATTER
--
-- This is outside the null_ls setup because null_ls requires a version of
-- black that supports --stdin-filename, but my company uses an older version.
-- https://github.com/nvimtools/none-ls.nvim/blob/68ed8b9b/lua/null-ls/builtins/formatting/black.lua#L21
local py_format = vim.api.nvim_create_augroup("py_format", { clear = true })

local py_formatter_priority = { "ruff", "black" }

-- Subcommand needed to make a formatter format; bare name is assumed
-- otherwise.
local py_formatter_args = { ruff = "format" }

local function py_format_cmd()
	local prog = vim.env.NVIM_PYTHON_FORMATTER
	if not prog or prog == "" then
		for _, candidate in ipairs(py_formatter_priority) do
			if vim.fn.executable(candidate) == 1 then
				prog = candidate
				break
			end
		end
	end
	if not prog then
		return nil
	end

	local args = py_formatter_args[vim.fs.basename(prog)]
	return args and (prog .. " " .. args) or prog
end

vim.api.nvim_create_autocmd("BufWritePost", {
	group = py_format,
	pattern = "*.py",
	callback = function()
		local cmd = py_format_cmd()
		if not cmd then
			return
		end
		vim.fn.system(cmd .. " " .. vim.fn.shellescape(vim.fn.expand("%")))
		vim.cmd("edit")
	end,
})

null_ls.setup({
	-- enable this and run :NullLsLog to see a detailed log
	-- debug = true,
	--
	-- this on_attach function sets null-ls to do document formatting on save
	--
	-- see:
	-- https://github.com/jose-elias-alvarez/null-ls.nvim/wiki/Formatting-on-save
	on_attach = function(client, bufnr)
		if client:supports_method("textDocument/formatting") then
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
		null_ls.builtins.formatting.biome.with({
			prefer_local = "node_modules/.bin",
			-- if prettier is in node bin, configure it as the formatter.
			-- if dprint or biome is available in the bin, use it as the formatter
			-- if neither dprint nor biome is available, use the global prettier
			condition = function(utils)
				local js_filetypes =
					{ "javascript", "typescript", "typescriptreact", "javascriptreact", "json", "jsonc", "markdown" }
				if vim.tbl_contains(js_filetypes, vim.bo.filetype) then
					return not is_in_node_bin(utils.bufname, "prettier")
				end
			end,
		}),
		-- black 19.10b0 doesn't support --stdin-filename, which none-ls requires
		-- null_ls.builtins.formatting.black,
		null_ls.builtins.formatting.clang_format,
		null_ls.builtins.formatting.gleam_format,
		null_ls.builtins.formatting.goimports,
		null_ls.builtins.formatting.gofumpt,
		null_ls.builtins.formatting.prettier.with({
			prefer_local = "node_modules/.bin",
			-- if prettier is in node bin, configure it as the formatter.
			-- if dprint or biome is available in the bin, use it as the formatter
			-- if neither dprint nor biome is available, use the global prettier
			condition = function(utils)
				local js_filetypes =
					{ "javascript", "typescript", "typescriptreact", "javascriptreact", "json", "jsonc", "markdown" }
				if vim.tbl_contains(js_filetypes, vim.bo.filetype) then
					if is_in_node_bin(utils.bufname, "prettier") then
						return true
					elseif is_in_node_bin(utils.bufname, "dprint") or is_in_node_bin(utils.bufname, "biome") then
						vim.notify("using dprint or biome", vim.log.levels.INFO)
						return false
					end
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
})

-- used in config_lsp.lua
return {
	is_in_node_bin = is_in_node_bin,
}
