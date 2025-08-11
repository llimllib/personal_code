-- worked from the base default sonokai colorscheme:
-- https://github.com/sainnhe/sonokai/blob/27a71a6/autoload/sonokai.vim#L34-L60
vim.g.sonokai_colors_override = {
	black = { "#181a1c", "232" },
	bg_dim = { "#24272e", "232" },
	bg0 = { "#252A39", "235" },
	bg1 = { "#2a3041", "236" },
	bg2 = { "#2f3548", "236" },
	bg3 = { "#343b50", "237" },
	bg4 = { "#394158", "237" },
	bg_red = { "#ff6d7e", "203" },
	bg_green = { "#3a8168", "107" },
	bg_blue = { "#7ad5f1", "110" },
	bg_purple = { "#d9b1a2", "54" },
	-- In sonokai, these seem to be slightly brighter variants, but I haven't
	-- bothered
	filled_red = { "#F47648", "203" },
	filled_green = { "#40BA93", "107" },
	filled_blue = { "#73D0FF", "110" },
	fg = { "#e1e3e4", "250" },
	red = { "#F47648", "203" },
	orange = { "#8ED0B2", "215" },
	yellow = { "#8ED0B2", "179" },
	green = { "#40BA93", "107" },
	blue = { "#73D0FF", "110" },
	purple = { "#fca07f", "176" },
	grey = { "#828a9a", "246" },
	grey_dim = { "#5a6477", "240" },
	none = { "NONE", "NONE" },
}
vim.cmd("colorscheme sonokai")
