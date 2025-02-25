-- https://codecompanion.olimorris.dev/getting-started.html
require("codecompanion").setup({
	strategies = {
		chat = {
			adapter = "anthropic",
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
})
