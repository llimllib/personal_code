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
	fg = { "#e1e3e4", "250" },
	none = { "NONE", "NONE" },

	-- oklch(0.71 0.122 21.36) https://oklch.com/#0.71,0.122,21.36,100
	red = { "#e18665", "203" },
	purple = { "#e18665", "176" },
	-- oklch(0.7882 0.062 21.36) https://oklch.com/#0.7882,0.062,21.36,100
	bg_purple = { "#dfaba9", "54" },

	-- oklch(0.71 0.1625 21.36) https://oklch.com/#0.71,0.1625,21.36,100
	filled_red = { "#f67274", "203" },
	bg_red = { "#f67274", "203" },

	-- oklch(0.8427 0.1225 167.95) https://oklch.com/#0.8427,0.1225,167.95,100
	orange = { "#70e5bc", "215" },
	yellow = { "#70e5bc", "179" },

	-- oklch(0.71, 0.122, 167.95) https://oklch.com/#0.711,0.1225,167.95,100
	green = { "#40BA93", "107" },
	filled_green = { "#40BA93", "107" },
	-- oklch(0.6059 0.1225 167.95) https://oklch.com/#0.6059,0.1225,167.95,100
	bg_green = { "#019974", "107" },

	-- oklch(0.7095 0.122 229.35) https://oklch.com/#0.7095,0.122,229.35,100
	blue = { "#3aafde", "110" },
	filled_blue = { "#3aafde", "110" },
	-- oklch(0.8073 0.0966 229.35) https://oklch.com/#0.8073,0.0966,229.35,100
	bg_blue = { "#7accf2", "110" },

	-- oklch(0.7095 0.0346 229.35) https://oklch.com/#0.7095,0.0346,229.35,100
	grey = { "#8ca6b3", "246" },

	-- oklch(0.546 0.0346 229.35) https://oklch.com/#0.546,0.0346,229.35,100
	grey_dim = { "#5c7581", "240" },
}
vim.cmd("colorscheme sonokai")
