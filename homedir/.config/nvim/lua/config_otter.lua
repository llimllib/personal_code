-- otter enables completion in markdown fenced code blocks
-- https://github.com/jmbuhr/otter.nvim
-- config stolen from:
-- https://github.com/willothy/nvim-config/blob/80d82e0/lua/configs/editor/otter.lua#L16

local otter = require("otter")

otter.setup({
	set_filetype = true,
})

vim.g.markdown_fenced_languages = {
	"js",
}

vim.api.nvim_create_autocmd({ "BufEnter" }, {
	pattern = { "*.md" },
	callback = function()
		otter.activate({ "javascript", "typescript", "js", "ts" }, true, true)
		vim.api.nvim_buf_set_keymap(0, "n", "gd", ":lua require'otter'.ask_definition()<cr>", { silent = true })
		vim.api.nvim_buf_set_keymap(0, "n", "K", ":lua require'otter'.ask_hover()<cr>", { silent = true })
	end,
})
