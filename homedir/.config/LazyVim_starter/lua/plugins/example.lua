-- The lazyvim plugins directory you're inheriting from is here:
-- https://github.com/LazyVim/LazyVim/tree/142e6bec209704210db23b3424b4d51896bb206a/lua/lazyvim/plugins

-- every spec file under the "plugins" directory will be loaded automatically by lazy.nvim
--
-- In your plugin files, you can:
-- * add extra plugins
-- * disable/enabled LazyVim plugins
-- * override the configuration of LazyVim plugins
return {
  -- my custom sonokai theme
  {
    "sainnhe/sonokai",
    config = function()
      vim.g.sonokai_colors_override = {
        black = { "#181a1c", "232" },
        bg_dim = { "#24272e", "232" },
        bg0 = { "#252A39", "235" },
        bg1 = { "#2a3041", "236" },
        bg2 = { "#2f3548", "236" },
        bg3 = { "#343b50", "237" },
        bg4 = { "#394158", "237" },
        bg_red = { "#ff6d7e", "203" },
        diff_red = { "#55393d", "52" },
        bg_green = { "#a5e179", "107" },
        diff_green = { "#394634", "22" },
        bg_blue = { "#7ad5f1", "110" },
        diff_blue = { "#354157", "17" },
        diff_yellow = { "#4e432f", "54" },
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
    end,
  },
  { "digitaltoad/vim-pug" }, -- pug and jade syntax highlighting
  { "lepture/vim-jinja" }, -- jinja
  { "fatih/vim-go" }, -- go dev. Mainly useful for template syntax
  { "vmchale/just-vim" }, -- highlighting Justfiles

  -- Configure LazyVim to load gruvbox
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "sonokai",
    },
  },

  -- change trouble config
  {
    "folke/trouble.nvim",
    -- opts will be merged with the parent spec
    opts = { use_diagnostic_signs = true },
  },

  -- disable plugins:
  { "echasnovski/mini.pairs", enabled = false }, -- automatically pair quotes/braces
  { "echasnovski/mini.starter", enabled = false }, -- the startup screen
  { "echasnovski/mini.indentscope", enabled = false }, -- weird indent stuff I don't want
  { "folke/flash.nvim", enabled = false }, -- fancy searching
  { "folke/noice.nvim", enabled = false }, -- fancy UI for vim
  { "folke/which-key.nvim", enabled = false }, -- key bindings popup
  { "goolord/alpha-nvim", enabled = false }, -- the startup screen
  { "rafamadriz/friendly-snippets", enabled = false }, -- community source of snippets
  { "stevearc/dressing.nvim", enabled = false }, -- fancy input boxes
  { "williamboman/mason.nvim", enabled = false }, -- automatic lsp server installation
  { "RRethy/vim-illuminate", enabled = false }, -- fancy highlighting of search matches

  -- add symbols-outline
  {
    "simrat39/symbols-outline.nvim",
    cmd = "SymbolsOutline",
    keys = { { "<leader>cs", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" } },
    config = true,
  },

  -- change some telescope options and a keymap to browse plugin files
  {
    "nvim-telescope/telescope.nvim",
    keys = {
      -- add a keymap to browse plugin files
      -- stylua: ignore
      {
        "<leader>fp",
        function() require("telescope.builtin").find_files({ cwd = require("lazy.core.config").options.root }) end,
        desc = "Find Plugin File",
      },
      {
        "<leader>ff",
        "<cmd>Telescope find_files hidden=true<cr>",
        desc = "Find files",
      },
      {
        "<leader>fg",
        "<cmd>Telescope live_grep<cr>",
        desc = "Find files",
      },
    },
    -- change some options
    opts = {
      defaults = {
        layout_strategy = "horizontal",
        layout_config = { prompt_position = "top" },
        sorting_strategy = "ascending",
        winblend = 0,
      },
    },
  },

  -- add telescope-fzf-native
  {
    "telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
  },

  -- add tsserver and setup with typescript.nvim instead of lspconfig
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "jose-elias-alvarez/typescript.nvim",
      init = function()
        require("lazyvim.util").lsp.on_attach(function(_, buffer)
          -- stylua: ignore
          vim.keymap.set("n", "<leader>co", "TypescriptOrganizeImports", { buffer = buffer, desc = "Organize Imports" })
          vim.keymap.set("n", "<leader>cR", "TypescriptRenameFile", { desc = "Rename File", buffer = buffer })
        end)
      end,
      opts = {
        setup = {
          tsserver = function()
            require("lazyvim.util").lsp.on_attach(function(client)
              if client.name == "tsserver" then
                client.server_capabilities.documentFormattingProvider = false
              end
              if client.name == "eslint" then
                client.server_capabilities.documentFormattingProvider = false
              end
            end)
          end,
        },
      },
    },
    opts = {
      servers = {
        bashls = {},
        clangd = {},
        cssls = {},
        denols = {},
        elixirls = {},
        gopls = {},
        html = {},
        pyright = {},
        tsserver = {},
        zls = {},
      },
      -- you can do any additional lsp server setup here
      -- return true if you don't want this server to be setup with lspconfig
      setup = {
        -- example to setup with typescript.nvim
        tsserver = function(_, opts)
          require("typescript").setup({ server = opts })
          return true
        end,
        -- Specify * to use this function as a fallback for any server
        -- ["*"] = function(server, opts) end,
      },
    },
  },

  -- for typescript, LazyVim also includes extra specs to properly setup lspconfig,
  -- treesitter, mason and typescript.nvim. So instead of the above, you can use:
  { import = "lazyvim.plugins.extras.lang.typescript" },
  { import = "lazyvim.plugins.extras.linting.eslint" },
  -- { import = "lazyvim.plugins.extra.formatting.prettier" },

  -- since `vim.tbl_deep_extend`, can only merge tables and not lists, the code above
  -- would overwrite `ensure_installed` with the new value.
  -- If you'd rather extend the default config, use the code below instead:
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      opts.indent = { enabled = false }

      vim.list_extend(opts.ensure_installed, {
        "bash",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "regex",
        "tsx",
        "typescript",
        "vim",
        "yaml",
      })
    end,
  },

  -- add jsonls and schemastore packages, and setup treesitter for json, json5 and jsonc
  { import = "lazyvim.plugins.extras.lang.json" },

  -- Use <tab> for completion and snippets (supertab)
  -- first: disable default <tab> and <s-tab> behavior in LuaSnip
  {
    "L3MON4D3/LuaSnip",
    keys = function()
      return {}
    end,
  },

  -- then: setup supertab in cmp
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-emoji",
    },
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      -- local luasnip = require("luasnip")
      local cmp = require("cmp")

      opts.mapping = vim.tbl_extend("force", opts.mapping, {
        ["<C-d>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-y>"] = cmp.mapping.confirm({
          select = true,
          behavior = cmp.ConfirmBehavior.Insert,
        }),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<Tab>"] = function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end,
        ["<S-Tab>"] = function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            fallback()
          end
        end,
        ["<CR>"] = cmp.mapping.confirm({
          select = true,
          behavior = cmp.ConfirmBehavior.Insert,
        }),
        ["<up>"] = function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            fallback()
          end
        end,
        ["<down>"] = function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          else
            fallback()
          end
        end,
      })
      -- order of the sources shown in autocomplete - I don't want snippets so
      -- cut them out
      opts.sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "buffer" },
        { name = "path" },
      })
      opts.completion = {
        autocomplete = false,
        completeopt = "menu,menuone,noinsert,noselect",
      }
      opts.preselect = cmp.PreselectMode.None
    end,
  },

  {
    "jose-elias-alvarez/null-ls.nvim",
    event = { "BufReadPre", "BufNewFile" },
    -- unclear to me why this doesn't work. See keymaps.lua for how I
    -- eventually managed to unset this.
    -- https://github.com/jose-elias-alvarez/null-ls.nvim/issues/1131
    -- config = function()
    --   require("null-ls").setup({
    --     on_attach = function(_, bufnr)
    --       vim.api.nvim_buf_set_option(bufnr, "formatexpr", "")
    --     end,
    --   })
    -- end,
    opts = function()
      local null_ls = require("null-ls")
      return {
        root_dir = require("null-ls.utils").root_pattern(".null-ls-root", ".neoconf.json", "Makefile", ".git"),
        sources = {
          null_ls.builtins.formatting.black.with({
            prefer_local = ".venv/bin",
          }),
          null_ls.builtins.formatting.clang_format,
          null_ls.builtins.formatting.goimports,
          null_ls.builtins.formatting.gofumpt,
          null_ls.builtins.formatting.prettier.with({
            prefer_local = "node_modules/.bin",
          }),
          null_ls.builtins.formatting.stylua,
          null_ls.builtins.formatting.terraform_fmt,
        },
      }
    end,
  },

  -- prevent vim from trying to use all these plugins on big files
  {
    "LunarVim/bigfile.nvim",
    -- -- default config
    -- require("bigfile").setup {
    --   filesize = 2, -- size of the file in MiB, the plugin round file sizes to the closest MiB
    --   pattern = { "*" }, -- autocmd pattern or function see <### Overriding the detection of big files>
    --   features = { -- features to disable
    --     "indent_blankline",
    --     "illuminate",
    --     "lsp",
    --     "treesitter",
    --     "syntax",
    --     "matchparen",
    --     "vimopts",
    --     "filetype",
    --   },
    -- }
  },

  -- display messages in compact form, and on the bottom right instead of top right
  {
    "rcarriga/nvim-notify",
    opts = {
      top_down = false,
      render = "compact",
      timeout = 2000,
    },
  },

  {
    "linux-cultist/venv-selector.nvim",
    cmd = "VenvSelect",
    opts = {
      name = { "venv", ".venv" },
    },
    keys = { { "<leader>cv", "<cmd>:VenvSelect<cr>", desc = "Select VirtualEnv" } },
  },
  -- colorize hex colors
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
  },
  -- {
  --   "folke/flash.nvim",
  --   -- If you have search integration in flash.nvim installed and you're me,
  --   -- you often end up jumping to a point in your file that you weren't
  --   -- intending to go to and making edits to your file. I think I probably
  --   -- should just wholly disable the plugin, but I'll give it a chance and
  --   -- just disable the search integration.
  --   -- https://github.com/LazyVim/LazyVim/issues/1109
  --   opts = { modes = { search = { enabled = false } } },
  -- },
}
