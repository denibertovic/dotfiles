require("avante_lib").load()
require("avante").setup({
	-- add any opts here
	-- for example
	providers = {
		openai = {
			endpoint = "https://api.anthropic.com/v1/messages",
			model = "claude-3-5-sonnet-20241022", -- your desired model
			timeout = 30000, -- Timeout in milliseconds, increase this for reasoning models
			temperature = 0,
			max_completion_tokens = 8192, -- Increase this to include reasoning tokens (for reasoning models)
			--reasoning_effort = "medium", -- low|medium|high, only used for reasoning models
		},
	},
})
