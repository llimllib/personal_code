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

-- TODO: get this stuff into proper lua
vim.cmd([[
augroup myfiletypes
  "clear old autocmds in group
  autocmd!
  autocmd FileType ruby,haml,eruby,yaml set sw=2 sts=2 et
  autocmd FileType html set sw=2 sts=2 et
  autocmd FileType python,c set sw=4 sts=4 et
  autocmd FileType javascript set sw=2 sts=2 et
  autocmd FileType typescript set sw=2 sts=2 et
  autocmd FileType go set ts=4 sw=4 sts=4 noet nolist
  autocmd FileType smarty set syntax=gotexttmpl
  autocmd FileType proto set ts=2 sts=2 sw=2
  autocmd BufRead,BufNewFile *.glsl,*.frag setfiletype glsl
augroup END

" Neovim doesn't seem to save the buffer input mode status, and I don't like
" having to enter insert mode almost every time, so automatically go into
" insert mode when I enter a terminal. Ideally I could just navigate around
" without exiting insert mode, but it is what it is.
au BufEnter * if &buftype == 'terminal' | :startinsert | endif
]])
