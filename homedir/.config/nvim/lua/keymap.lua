vim.cmd("runtime macros/matchit.vim")

-- better split navigation
vim.keymap.set("", "<C-h>", "<C-w>h")
vim.keymap.set("", "<C-j>", "<C-w>j")
vim.keymap.set("", "<C-k>", "<C-w>k")
vim.keymap.set("", "<C-l>", "<C-w>l")

-- go up/down by visual lines
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("v", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set("v", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- disable Q for ex mode
vim.keymap.set("n", "Q", "<nop>")

-- terminal mode shortcuts.
-- https://neovim.io/doc/user/nvim_terminal_emulator.html
vim.keymap.set("t", "<C-h>", "<C-\\><C-N><C-w>h")
vim.keymap.set("t", "<C-j>", "<C-\\><C-N><C-w>j")
vim.keymap.set("t", "<C-k>", "<C-\\><C-N><C-w>k")
vim.keymap.set("t", "<C-l>", "<C-\\><C-N><C-w>l")
vim.keymap.set("i", "<C-h>", "<C-\\><C-N><C-w>h")
vim.keymap.set("i", "<C-j>", "<C-\\><C-N><C-w>j")
vim.keymap.set("i", "<C-k>", "<C-\\><C-N><C-w>k")
vim.keymap.set("i", "<C-l>", "<C-\\><C-N><C-w>l")
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-l>", "<C-w>l")
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")

vim.keymap.set("n", "<leader>t", ":vsplit<CR>:terminal<CR>a")

-- insert one character
vim.keymap.set("", "<leader>i", "i<space><esc>r")

-- system paste and yank
vim.keymap.set("n", "<leader>p", ":pu +<CR>")
vim.keymap.set("v", "<leader>y", '"+y')

-- edit vimrc, source vimrc and edit lua config
vim.keymap.set("n", "<leader>ev", ":vsplit $MYVIMRC<cr>")
vim.keymap.set("n", "<leader>es", ":source $MYVIMRC<cr>")
vim.keymap.set("n", "<leader>el", ":vsplit ~/.config/nvim/lua/config_lsp.lua<cr>")

-- http://statico.github.io/vim.html
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

-- navigate diagnostics
vim.keymap.set("n", "<leader>m", "<cmd>lua vim.diagnostic.goto_next()<CR>")
vim.keymap.set("n", "<leader>M", "<cmd>lua vim.diagnostic.goto_prev()<CR>")

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

-- See `:help vim.lsp.*` for documentation on any of the below functions
local bufopts = { noremap = true, silent = true }
-- c-t means "up the tag stack" and is the opposite of gd, so map it to
-- something more convenient
vim.keymap.set("n", "gh", "<c-t>", bufopts)
vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
vim.keymap.set("n", "gt", vim.lsp.buf.hover, bufopts)
vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, bufopts)
vim.keymap.set("n", "<leader>d", vim.lsp.buf.type_definition, bufopts)
vim.keymap.set("n", "gR", vim.lsp.buf.rename, bufopts)
vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
vim.keymap.set("n", "<leader>fa", function()
	vim.lsp.buf.format({ async = true })
end, bufopts)

-- Format json
vim.keymap.set("n", "<leader>j", ":%!jq '.'<CR>", { noremap = true })
-- Format xml
vim.keymap.set("n", "<leader>x", ":%!xmllint --format --encode UTF-8 -<CR>", { noremap = true })
-- Format html
vim.keymap.set(
	"n",
	"<leader>h",
	":%!tidy -utf8 -q --show-body-only true -f /tmp/tidyerrors -i %<CR>",
	{ noremap = true }
)

-- configure telescope
-- https://github.com/nvim-telescope/telescope.nvim#usage
-- Find files using Telescope command-line sugar.
--
-- <c-x> opens in a split
-- <c-v> opens in a vsplit
vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files hidden=true<cr>", { noremap = true })
vim.keymap.set("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { noremap = true })
vim.keymap.set("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { noremap = true })
vim.keymap.set("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { noremap = true })

-- dadbod shortcuts
-- I can't figure out why this doesn't work?
-- vim.keymap.set("v", "<leader>db", "<cmd>DB<cr>", {noremap = true})
-- vim.keymap.set("x", "<leader>q", "db#op_exec()", { noremap = true })
-- vim.keymap.set("n", "<leader>q", "<cmd>DB<cr>", { noremap = true })
vim.keymap.set({ "n", "x" }, "<leader>db", "db#op_exec()", { expr = true })

-- from: https://github.com/tpope/vim-dadbod/issues/33
--
-- How it works:
--
-- The first search finds the next semicolon. The second marks the spot and
-- moves backwards to the previous semicolon or the start of the buffer. The
-- third moves forward to the next select|with|insert|update|delete|create.
-- Finally, it changes to visual mode and selects to the previous mark.
--
-- \v: very magic
-- \c: ignore case
-- c: include current char in search
-- s: set the ' mark at the previous location of the cursor
-- W: do not wrap
-- z: start search at the cursor column instead of zero
--
-- TODO: test this... not sure if the conversion to lua worked
vim.keymap.set(
	"v",
	"<leader>aq",
	[[<esc>:call search(";", "cWz")<cr>:call search(";\\<bar>\\%^", "bsWz")<cr>:call search("\\v\\c^(select<bar>with<bar>insert<bar>update<bar>delete<bar>create)\>", "Wz")<cr>vg`']],
	{ noremap = true }
)

-- use ,se in normal mode to try to attempt to guess the current sql statement
-- and run it. May not quite select the proper area. I would like to figure out
-- how to replace :DB with db#op_exec() but I don't know how
vim.keymap.set("n", "<leader>se", ":normal v<leader>aq<cr>:DB<cr>", { noremap = true })

-- end vim-dadbod

vim.keymap.set("n", "<leader>gb", ":Git blame<cr>")
vim.keymap.set("n", "<leader>gs", ":Git<cr>")
