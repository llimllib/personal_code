local completion = require('completion')
local lsp = require('lspconfig')

local on_attach = function(client, bufnr)
  completion.on_attach(client, bufnr)

  -- remove once diagnostic is deprecated
  -- https://github.com/neovim/neovim/pull/12655 (seems like it will go in
  -- soon)
  -- diagnostic.on_attach(client, bufnr)

  -- https://github.com/nvim-lua/diagnostic-nvim/issues/29#issuecomment-638040064
  -- If you want to have the diagnostic information come up on hover, uncomment this:
  -- vim.api.nvim_command('autocmd CursorHold <buffer> lua vim.lsp.util.show_line_diagnostics()')
  --
  -- I thought I wanted that, but it turned out to be a bit annoying. Do
  -- <leader>m instead to go to the next error. What I really want is text in
  -- the command bar on hover only, instead of virtual text, I think? That's
  -- what ALE has... not sure if I'm just being habitual or I actually like
  -- that
end

-- npm install -g typescript typescript-language-server
lsp.tsserver.setup{ on_attach=on_attach }
lsp.gopls.setup{ on_attach=on_attach }
lsp.solargraph.setup{ on_attach=on_attach }

-- installing pyls, and you need a specific plugin for black. Docs:
-- https://github.com/palantir/python-language-server/tree/7a98c2c5f9de193a02c2a53405fb951ff7b3ae6b#3rd-party-plugins
-- 
-- pip install 'python-language-server[all]'
-- pip install pyls-black
lsp.pyls.setup {
  on_attach = on_attach,
  settings = {
    pyls = {
      configurationSources = {'flake8'},
      plugins = {
          autopep8 = {enabled = false},
          black = {enabled = true},
          flake8 = {enabled = true},
          mccabe = {enabled = false},
          pycodestyle = {enabled = false},
          pyflakes = {enabled = false},
          yapf = {enabled = false}
      }
    }
  }
}


-- uncomment this once https://github.com/neovim/neovim/pull/12655 gets merged
--
-- nvim-diagnostics has been deprecated:
-- https://github.com/nvim-lua/diagnostic-nvim/issues/73
--
-- so let's configure diagnostics here
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
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
  }
)
