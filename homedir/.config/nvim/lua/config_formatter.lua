-- Utilities for creating configurations
local util = require("formatter.util")

-- stolen and modified from:
-- https://github.com/rockerBOO/dotfiles/blob/71b316a98162b9fed3ed075dc8e97512941d66b8/config/nvim/lua/plugin/formatter.lua#L152
local local_prettier = function(parser)
	if not parser then
		return {
			exe = "npx",
			args = {
				"prettier",
				"--stdin-filepath",
				util.escape_path(util.get_current_buffer_file_path()),
			},
			stdin = true,
			try_node_modules = true,
		}
	end
	return {
		exe = "npx",
		args = {
			"prettier",
			"--stdin-filepath",
			util.escape_path(util.get_current_buffer_file_path()),
			"--parser",
			parser,
		},
		stdin = true,
		try_node_modules = true,
	}
end

-- Provides the Format, FormatWrite, FormatLock, and FormatWriteLock commands
require("formatter").setup({
	-- Enable or disable logging
	logging = true,
	-- Set the log level
	log_level = vim.log.levels.WARN,
	-- All formatter configurations are opt-in
	filetype = {
		c = { require("formatter.filetypes.c").clangformat },
		go = {
			-- I'm not sure that this is how you compose formatters properly?
			require("formatter.filetypes.go").goimports,
			require("formatter.filetypes.go").gofumpt,
		},
		javascript = { local_prettier },
		json = { local_prettier },
		lua = { require("formatter.filetypes.lua").stylua },
		markdown = { require("formatter.filetypes.markdown").prettier },
		python = { require("formatter.filetypes.python").black },
		typescriptreact = { local_prettier },
		typescript = { local_prettier },
		yaml = { local_prettier },
	},
})

vim.keymap.set("n", "<leader>aa", "<Cmd>Format<CR>", { desc = "Format file" })
vim.keymap.set("v", "<leader>aa", "<Cmd>Format<CR>", { desc = "Format visual selection" })

-- format on save
-- diabling for now, to see if I can live with manual formatting.
vim.api.nvim_exec(
	[[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.js,*.lua,*.py,*.json,*.ts,*.jsx,*.tsx,*.yaml,*.go,*.c FormatWrite
augroup END
]],
	true
)
