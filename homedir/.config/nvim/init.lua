-- the order is important here. First the global options, then my key mappings:
require("options")
require("keymap")

-- next up, lazy.nvim manages plugins
require("lazyconfig")

require("autocmds")
require("config_lsp")
require("config_otter")
require("none_ls")
require("colorscheme")
require("telescope-cfg")
require("config_codecompanion")
