local lspconfig = require("lspconfig")
lspconfig.hls.setup({})
lspconfig.nixd.setup({})
lspconfig.pyright.setup({})
-- lspconfig.pylsp.setup{}
lspconfig.tsserver.setup({})
lspconfig.yamlls.setup({})
lspconfig.lua_ls.setup({
	settings = {
		Lua = {
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
			},
		},
	},
})
lspconfig.terraformls.setup({})
