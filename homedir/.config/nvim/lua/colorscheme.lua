-- configure colorscheme
if vim.fn.has("termguicolors") == 1 then
    vim.opt.termguicolors = true

    ayu = require('ayu')
    ayu.setup({
        mirage = true,
        overrides = {
            -- a few shades brighter than the default #5c6773
            Comment = {fg = "#adb3b9", italic = true}
        }
    })
    ayu.colorscheme()

    -- to set a colorscheme that's not lua-based:
    -- vim.cmd('colorscheme everforest')
end