local lsp = require('lspconfig')

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
end

-- npm install -g typescript typescript-language-server
lsp.tsserver.setup{ on_attach=on_attach }

-- (cd /tmp && GO111MODULE=on go get golang.org/x/tools/gopls@latest)
lsp.gopls.setup{ on_attach=on_attach }

-- gem install solargraph
lsp.solargraph.setup{ on_attach=on_attach }

-- npm i -g bash-language-server
-- This doesn't seem to be working rn
lsp.bashls.setup{ on_attach=on_attach }

-- now I don't have formatting set up
-- TODO install https://github.com/mattn/efm-langserver
-- pip install pyright
lsp.pyright.setup {
   on_attach = on_attach,
}

lsp.elixirls.setup{
    on_attach = on_attach,
    cmd = { "/opt/elixir-ls/language_server.sh" };
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
