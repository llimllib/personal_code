-- To install all prerequisite language servers and formatters:
-- brew install efm-langserver shellcheck
-- (cd /tmp && GO111MODULE=on go get golang.org/x/tools/gopls@latest)
-- npm install -g typescript typescript-language-server prettier
-- gem install solargraph
-- pip install pyright black
-- luarocks install --server=https://luarocks.org/dev luaformatter
-- build zls manually: https://github.com/zigtools/zls/wiki/Downloading-and-Building-ZLS#cloning-with-git

local lsp = require('lspconfig')
local cmp = require('cmp')

-- https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings
local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

-- https://github.com/hrsh7th/nvim-cmp/wiki/Menu-Appearance
local kind_icons = {
  Text = "",
  Method = "",
  Function = "",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "ﴯ",
  Interface = "",
  Module = "",
  Property = "ﰠ",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = ""
}

-- setup code completion
--
-- copied and modified from
-- https://github.com/tomaskallup/dotfiles/blob/ceb58fec84126c3764776672b4649ecb1a0f76a5/nvim/lua/plugins/nvim-cmp.lua
cmp.setup({
    completion = {
        autocomplete = false,
    },
    -- I don't use snippets or know what they are, but nvim-cmp requires this, so
    -- whatever I guess
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      end,
    },
    mapping = {
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-y>'] = cmp.mapping.confirm({
            select = true,
            behavior = cmp.ConfirmBehavior.Insert,
        }),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end,
        ['<S-Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            else
                fallback()
            end
        end,
        ['<CR>'] = cmp.mapping.confirm({
            select = true,
            behavior = cmp.ConfirmBehavior.Insert,
        }),
    },
    sources = {
        {name = 'nvim_lsp'},
        {name = 'buffer'},
        {name = 'path'}
    },
    formatting = {
      format = function(entry, vim_item)
        -- Kind icons
        vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
        -- Source
        vim_item.menu = ({
          buffer = "[Buffer]",
          nvim_lsp = "[LSP]",
          nvim_lua = "[Lua]",
          latex_symbols = "[LaTeX]",
        })[entry.source.name]
        return vim_item
      end
    },
    preselect = {
      cmp.PreselectMode.None
    }
})
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

local on_attach = function(client, bufnr)
  -- https://github.com/nvim-lua/diagnostic-nvim/issues/29#issuecomment-638040064
  -- If you want to have the diagnostic information come up on hover, uncomment this:
  -- vim.api.nvim_command('autocmd CursorHold <buffer> lua vim.lsp.util.show_line_diagnostics()')
  --
  -- I thought I wanted that, but it turned out to be a bit annoying. Do
  -- <leader>m instead to go to the next error. What I really want is text in
  -- the command bar on hover only, instead of virtual text, I think? That's
  -- what ALE has... not sure if I'm just being habitual or I actually like
  -- that
  --
  -- the next three lines are from https://github.com/hrsh7th/nvim-cmp/wiki/Language-Server-Specific-Samples#golang-gopls
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
end

-- XXX: how to switch between the two as necesssary?
-- npm install -g typescript typescript-language-server
lsp.tsserver.setup{
    on_attach=function(client, bufnr)
        -- don't format files, I prefer using prettier
        client.resolved_capabilities.document_formatting = false

        on_attach(client, bufnr)
    end,
    -- don't format files, I prefer using prettier
    settings = {
        documentFormatting = false
    },
    capabilities = capabilities
}
-- lsp.flow.setup{ on_attach=on_attach }

-- (cd /tmp && go install golang.org/x/tools/gopls@latest)
lsp.gopls.setup{
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      gopls = {
        analyses = {
          unusedparams = true,
          shadow = true,
        },
        staticcheck = true,
        gofumpt = true,
      },
    },
    init_options = {
      usePlaceholders = false,
    }
}

-- gem install solargraph
lsp.solargraph.setup{
    on_attach=on_attach,
    capabilities = capabilities
}

-- now I don't have formatting set up
-- pip install pyright
lsp.pyright.setup {
   on_attach = on_attach,
   capabilities = capabilities
}

lsp.elixirls.setup{
    on_attach = on_attach,
    cmd = { "/opt/elixir-ls/language_server.sh" },
    capabilities = capabilities,
}

lsp.clangd.setup{
    on_attach = on_attach,
    capabilities = capabilities,
}

lsp.zls.setup{
    on_attach = on_attach,
    capabilities = capabilities,
}

lsp.terraformls.setup{
    on_attach = on_attach,
    cmd = { 'terraform-ls', 'serve' },
}

lsp.tflint.setup{}

-- configure diagnostics
-- vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
--   vim.lsp.diagnostic.on_publish_diagnostics, {
--     -- set to false to disable virtual text displays
--     virtual_text = {
--         -- the default is '■' but I find that too distracting
--         prefix = '•',
--     },
-- 
--     -- configured with the `sign` command in init.vim, as suggested in :help
--     -- set_signs
--     signs = true,
-- 
--     -- don't update diagnostics while we're in insert mode
--     -- (Not sure if I'd rather wait until save, or if that's possible)
--     update_in_insert = false,
--   }
-- )
vim.diagnostic.config({
    -- set to false to disable virtual text displays
    virtual_text = {
        -- the default is '■' but I find that too distracting
        prefix = '•',
    },

    -- configured with the `sign` command in init.vim, as suggested in :help
    -- set_signs
    signs = true,

    -- don't update diagnostics while we're in insert mode
    -- (Not sure if I'd rather wait until save, or if that's possible)
    update_in_insert = false,
})


-- Formatting via efm
-- `brew install efm-langserver`
local prettier = {
  formatCommand = "./node_modules/.bin/prettier --stdin-filepath ${INPUT}",
  formatStdin = true
}

-- `luarocks install --server=https://luarocks.org/dev luaformatter`
local luafmt = {
  formatCommand = "lua-format -i",
  formatStdin = true
}

local shellcheck = {
  lintCommand = "shellcheck -f gcc -x -",
  lintStdin = true,
  lintFormats = { "%f:%l:%c: %trror: %m", "%f:%l:%c: %tarning: %m", "%f:%l:%c: %tote: %m" },
  lintSource = "shellcheck"
}

-- pip install black
local black = {
  formatCommand = 'black --quiet -',
  formatStdin = true
}

local terraform_fmt = {
  formatCommand = 'terraform fmt -',
  formatStdin = true
}

local languages = {
    css = {prettier},
    html = {prettier},
    javascript = {prettier},
    javascriptreact = {prettier},
    json = {prettier},
    markdown = {prettier},
    lua = {luafmt},
    python = {black},
    scss = {prettier},
    sh = {shellcheck},
    typescript = {prettier},
    typescriptreact = {prettier},
    yaml = {prettier},
    terraform = {terraform_fmt},
}

lsp.efm.setup {
    root_dir = lsp.util.root_pattern(".git"),
    filetypes = vim.tbl_keys(languages),
    init_options = {
        documentFormatting = true,
        codeAction = true
    },
    settings = {
        languages = languages,
        log_level = 1,
        log_file = '/tmp/efm.log'
    },
    on_attach = function(client, bufnr)
        client.resolved_capabilities.document_formatting = true
        client.resolved_capabilities.goto_definition = false
        on_attach(client, bufnr)
    end,
}
