-- https://codecompanion.olimorris.dev/getting-started.html
require("codecompanion").setup({
	strategies = {
		chat = {
			adapter = "anthropic",
			-- for all options: https://github.com/olimorris/codecompanion.nvim/blob/main/lua/codecompanion/config.lua#L42-L392
			keymaps = {
				options = {
					modes = {
						-- replace the default ? shortcut with g?, so that I
						-- can never use it and have access to reverse search
						n = "g?",
					},
					callback = "keymaps.options",
					description = "Options",
					hide = true,
				},
			},
		},
		inline = {
			adapter = "anthropic",
		},
	},
	adapters = {
		anthropic = function()
			return require("codecompanion.adapters").extend("anthropic", {
				env = {
					-- saved key with:
					-- $ security add-generic-password -s 'anthropic' -a 'llimllib' -w 'sk-ant-<key-goes-here>'
					-- retrieve with:
					-- $ security find-generic-password -s 'anthropic' -a 'llimllib' -w
					api_key = "cmd:security find-generic-password -w -s 'anthropic' | tr -d '\n'",
				},
				schema = {
					model = {
						default = "claude-3-7-sonnet-20250219",
					},
				},
			})
		end,
	},
	extensions = {
		-- https://codecompanion.olimorris.dev/extensions/history.html
		history = {
			enabled = true,
			opts = {
				-- Keymap to open history from chat buffer (default: gh)
				keymap = "gh",
				-- Keymap to save the current chat manually (when auto_save is disabled)
				save_chat_keymap = "sc",
				-- Save all chats by default (disable to save only manually using 'sc')
				auto_save = true,
				-- Number of days after which chats are automatically deleted (0 to disable)
				expiration_days = 0,
				-- Picker interface ("telescope" or "snacks" or "fzf-lua" or "default")
				picker = "telescope",
				---Automatically generate titles for new chats
				auto_generate_title = true,
				title_generation_opts = {
					---Adapter for generating titles (defaults to active chat's adapter)
					adapter = nil, -- e.g "copilot"
					---Model for generating titles (defaults to active chat's model)
					model = nil, -- e.g "gpt-4o"
				},
				---On exiting and entering neovim, loads the last chat on opening chat
				continue_last_chat = false,
				---When chat is cleared with `gx` delete the chat from history
				delete_on_clearing_chat = false,
				---Directory path to save the chats
				dir_to_save = vim.fn.stdpath("data") .. "/codecompanion-history",
				---Enable detailed logging for history extension
				enable_logging = false,
			},
		},
	},
})
