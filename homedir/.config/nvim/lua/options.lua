vim.g.mapleader = ','

-- use neovim's lua file type detection, which should improve startup time
-- https://gpanders.com/blog/whats-new-in-neovim-0-7/#filetypelua
vim.g.do_filetype_lua = 1

-- set is_bash by default.
-- Vim will highlight a file as posix shell, unless it's named *.bash or has
-- /bin/bash on the shebang line. However, I write my scripts with
-- "/usr/bin/env bash" instead of "/bin/bash", so it doesn't pick up on the
-- shebang properly. Just assume a shell script should be highlighted like
-- bash.
vim.g.is_bash = 1

-- backspace over auto-indents, eols, start of lines
vim.opt.backspace = {"indent", "eol", "start"}

-- show invisible characters
vim.opt.list = true
vim.opt.listchars = {
    tab = "→ ",
    eol = "¬",
    trail = "⋅",
    extends = "❯",
    precedes = "❮"
}

-- turn on persistent undo, and store it in the vim dir
vim.opt.undofile = true
vim.opt.undodir = "~/.config/nvim/undodir"

-- default space and tab handling
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.expandtab = true
vim.opt.softtabstop = 4

-- don't highlight searches
vim.opt.hls = false

-- fold by indents unless specified otherwise
vim.opt.foldmethod = "indent"
vim.opt.foldlevel = 9999

-- use the mouse in terminal mode
vim.opt.mouse = "a"

-- keep the cursor from going all the way to the bottom
vim.opt.scrolloff = 5

-- Automatically read files which have been changed outside of Vim, if we
-- haven't changed it already.
vim.opt.autoread = true

-- turn off swap files
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

-- this group is recommended by http://items.sjbach.com/319/configuring-vim-right
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.title = true
vim.opt.backupdir = {"~/.vim-tmp", "~/.tmp", "~/tmp", "/var/tmp", "/tmp"}
vim.opt.directory = {"~/.vim-tmp", "~/.tmp", "~/tmp", "/var/tmp", "/tmp"}

-- Wait 100ms before running cursorhold (shows diagnostic messages)
vim.opt.updatetime = 100

-- recommended by nvim-cmp
vim.opt.completeopt = {'menu', 'menuone', 'noselect'}

vim.opt.splitbelow = true
vim.opt.splitright = true

-- show line
vim.opt.ruler = true
vim.opt.statusline = '%f%m%r%h [%L] [%{&ff}] %y%=[%p%%] [line:%05l,col:%02v]'
vim.opt.laststatus = 2
