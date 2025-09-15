local augroup = vim.api.nvim_create_augroup("goyo_cmds", { clear = true })
local autocmd = vim.api.nvim_create_autocmd

-- configure Goyo
local function enter()
	vim.opt.wrap = true
	vim.opt.linebreak = true
end

local function leave()
	vim.opt.wrap = false
	vim.opt.linebreak = false
end

autocmd("User", { pattern = "GoyoEnter", group = augroup, callback = enter })
autocmd("User", { pattern = "GoyoLeave", group = augroup, callback = leave })

local filetypes = vim.api.nvim_create_augroup("filetypes", { clear = true })
-- languages that use 2 spaces and expandtab
autocmd("FileType", {
	group = filetypes,
	pattern = { "haml", "html", "javascript", "json", "markdown", "proto", "ruby", "sh", "typescript", "yaml" },
	callback = function()
		vim.opt_local.shiftwidth = 2
		vim.opt_local.tabstop = 2
		vim.opt_local.softtabstop = 2
		vim.opt_local.expandtab = true
	end,
})

-- python gets 4 spaces and expandtab
autocmd("FileType", {
	group = filetypes,
	pattern = { "python" },
	callback = function()
		vim.opt_local.shiftwidth = 4
		vim.opt_local.tabstop = 4
		vim.opt_local.softtabstop = 4
		vim.opt_local.expandtab = true
	end,
})

-- go gets 4 spaces and noexpandtab
autocmd("FileType", {
	group = filetypes,
	pattern = { "go" },
	callback = function()
		vim.opt_local.shiftwidth = 4
		vim.opt_local.tabstop = 4
		vim.opt_local.softtabstop = 4
		vim.opt_local.expandtab = false
	end,
})

-- set *.glsl and *.frag to glsl file types
autocmd({ "BufRead", "BufNewFile" }, {
	group = filetypes,
	pattern = { "*.glsl", "*.frag" },
	callback = function()
		vim.api.nvim_buf_set_option(buffer, "filetype", "glsl")
	end,
})

vim.cmd([[
" Neovim doesn't seem to save the buffer input mode status, and I don't like
" having to enter insert mode almost every time, so automatically go into
" insert mode when I enter a terminal. Ideally I could just navigate around
" without exiting insert mode, but it is what it is.
au BufEnter * if &buftype == 'terminal' | :startinsert | endif
]])

-- Add Scratch command for temporary buffer
vim.api.nvim_create_user_command("Scratch", function(opts)
	-- Default to vertical split unless horizontal specified
	local split_cmd = "vnew"
	if opts.args == "horizontal" then
		split_cmd = "new"
	end

	vim.cmd(split_cmd)

	-- Set buffer options for scratch buffer
	vim.bo.buftype = "nofile"
	vim.bo.bufhidden = "wipe"
	vim.bo.buflisted = false
	vim.bo.swapfile = false

	-- set a name for the buffer
	vim.api.nvim_buf_set_name(0, "[Scratch]")
end, {
	nargs = "?",
	complete = function()
		return { "vertical", "horizontal" }
	end,
})
