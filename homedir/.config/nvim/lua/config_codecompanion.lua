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
					api_key = "cmd:security find-generic-password -w -s 'anthropic' | tr -d '\n'",
				},
			})
		end,
	},
})
