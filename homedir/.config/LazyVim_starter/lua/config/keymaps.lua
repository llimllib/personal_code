-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local diagnostic_goto = function(next, severity)
  local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = severity })
  end
end
vim.keymap.set("n", "<leader>m", diagnostic_goto(true), { desc = "Next Diagnostic" })

-- c-t means "up the tag stack" and is the opposite of gd, so map it to
-- something more convenient
local bufopts = { noremap = true, silent = true }
vim.keymap.set("n", "gh", "<c-t>", bufopts)
vim.keymap.set("n", "gR", vim.lsp.buf.rename, bufopts)
vim.keymap.set("n", "gt", vim.lsp.buf.hover, bufopts)

-- more lsp mappings from lazyvim default:
-- K -> hover
-- gK -> signature help (hover + more info)
-- <leader>cr -> rename
--
-- source: https://github.com/jdhao/nvim-config/blob/1608a36d/lua/config/lsp.lua#L30

-- Format json
vim.keymap.set("n", "<leader>j", ":%!jq ''<CR>", { noremap = true })
-- Format xml
vim.keymap.set("n", "<leader>x", ":%!xmllint --format --encode UTF-8 -<CR>", { noremap = true })
-- Format html
vim.keymap.set(
  "n",
  "<leader>h",
  ":%!tidy -utf8 -q --show-body-only true -f /tmp/tidyerrors -i %<CR>",
  { noremap = true }
)

-- more shortcuts provided by lazyvim:
-- <leader>ft -> floating terminal

-- open a terminal in a vertical split
vim.keymap.set("n", "<leader>t", ":vsplit<CR>:terminal<CR>a")

-- emacs-style command-line controls
vim.keymap.set("c", "<C-a>", "<Home>")
vim.keymap.set("c", "<C-b>", "<Left>")
vim.keymap.set("c", "<C-f>", "<Right>")
vim.keymap.set("c", "<C-d>", "<Delete>")
vim.keymap.set("c", "<M-b>", "<S-Left>")
vim.keymap.set("c", "<M-f>", "<S-Right>")
vim.keymap.set("c", "<M-d>", "<S-right><Delete>")
vim.keymap.set("c", "<Esc>b", "<S-Left>")
vim.keymap.set("c", "<Esc>f", "<S-Right>")
vim.keymap.set("c", "<Esc>d", "<S-right><Delete>")
vim.keymap.set("c", "<C-g>", "<C-c>")

-- From the vim-lsp docs:
-- - Q: How to force-reload LSP?
--   A: Stop all clients, then reload the buffer.
--
--   :lua vim.lsp.stop_client(vim.lsp.get_active_clients())
--   :edit
--
-- Unfortunately, this does not work when you have a dirty buffer, so you have
-- to save or undo to reload the LSP server. I'm not quite sure what's the best
-- way to handle this - it works better in vim
vim.keymap.set("n", "<leader>r", "<cmd>lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR> :edit<CR>")

-- unset these keymaps:
--
-- https://github.com/LazyVim/LazyVim/blob/142e6be/lua/lazyvim/config/keymaps.lua#L90-L92
--
-- which mess up indent repetition. See this discussion:
--
-- https://github.com/LazyVim/LazyVim/discussions/1239
vim.keymap.set("v", "<", "<")
vim.keymap.set("v", ">", ">")
