-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.g.mapleader = ","

-- don't show line numbers
vim.opt.number = false
vim.opt.relativenumber = false

-- don't hide quotes
vim.opt.conceallevel = 0

-- turn on persistent undo, and store it in the vim dir
vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("config") .. "/undodir"

-- Automatically read files which have been changed outside of Vim, if we
-- haven't changed it already.
vim.opt.autoread = true

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.title = true

vim.opt.splitbelow = true
vim.opt.splitright = true

-- turn off swap files
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

-- Automatically read files which have been changed outside of Vim, if we
-- haven't changed it already.
vim.opt.autoread = true

vim.opt.wrap = true
