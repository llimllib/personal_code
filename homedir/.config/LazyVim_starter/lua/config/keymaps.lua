-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local keymaps = require("lazyvim.plugins.lsp.keymaps")

vim.keymap.set("n", "<leader>m", keymaps.diagnostic_goto(true), { desc = "Next Diagnostic" })
