require("tokyonight").setup({style = "night"})
vim.cmd([[colorscheme tokyonight]])

-- require("catppuccin").setup({ flavour = "macchiato" })
-- vim.cmd([[colorscheme catppuccin]])

vim.opt.termguicolors = true
-- require("bufferline").setup({})
require("lualine").setup()
require("ibl").setup({
    exclude = {
      filetypes = {
        "help",
        "alpha",
        "dashboard",
        "neo-tree",
        "Trouble",
        "trouble",
        "lazy",
        "mason",
        "notify",
        "toggleterm",
        "lazyterm",
      },
    }
})

require("notify").setup({
    stages = "static",
    timeout = 3000,
    max_height = function()
      return math.floor(vim.o.lines * 0.75)
    end,
    max_width = function()
      return math.floor(vim.o.columns * 0.75)
    end,
    on_open = function(win)
      vim.api.nvim_win_set_config(win, { zindex = 100 })
    end,
  })

require("noice").setup({
	lsp = {
		-- override markdown rendering so that **cmp** and other plugins use **Treesitter**
		override = {
			["vim.lsp.util.convert_input_to_markdown_lines"] = true,
			["vim.lsp.util.stylize_markdown"] = true,
			["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
		},
	},
	-- you can enable a preset for easier configuration
	presets = {
		bottom_search = true, -- use a classic bottom cmdline for search
		command_palette = true, -- position the cmdline and popupmenu together
		long_message_to_split = true, -- long messages will be sent to a split
		inc_rename = false, -- enables an input dialog for inc-rename.nvim
		lsp_doc_border = false, -- add a border to hover docs and signature help
	},
	routes = {
		{
			filter = {
				event = "msg_show",
				any = {
					{ find = "%d+L, %d+B" },
					{ find = "; after #%d+" },
					{ find = "; before #%d+" },
				},
			},
			view = "mini",
		},
	},
})




require('which-key').setup({
    preset = "helix",
    defaults = {},
    spec = {
        {
            mode = { "n", "v" },
            { "<leader><tab>", group = "tabs" },
            { "<leader>c", group = "code" },
            { "<leader>d", group = "debug" },
            { "<leader>dp", group = "profiler" },
            { "<leader>f", group = "file/find" },
            { "<leader>g", group = "git" },
            { "<leader>gh", group = "hunks" },
            { "<leader>q", group = "quit/session" },
            { "<leader>s", group = "search" },
            { "<leader>u", group = "ui", icon = { icon = "󰙵 ", color = "cyan" } },
            { "<leader>x", group = "diagnostics/quickfix", icon = { icon = "󱖫 ", color = "green" } },
            { "[", group = "prev" },
            { "]", group = "next" },
            { "g", group = "goto" },
            { "gs", group = "surround" },
            { "z", group = "fold" },
            {
                "<leader>b",
                group = "buffer",
                expand = function()
                    return require("which-key.extras").expand.buf()
                end,
            },
            {
                "<leader>w",
                group = "windows",
                proxy = "<c-w>",
                expand = function()
                    return require("which-key.extras").expand.win()
                end,
            },
            -- better descriptions
            { "gx", desc = "Open with system app" },
        },
    },
})

require('neoscroll').setup({})
