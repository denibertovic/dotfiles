local linter = require("lint")
linter.events = { "BufWritePost", "BufReadPost", "InsertLeave" }
linter.linters_by_ft = {
	fish = { "fish" },
	-- Use the "*" filetype to run linters on all filetypes.
	-- ['*'] = { 'global linter' },
	-- Use the "_" filetype to run linters on filetypes that don't have other linters configured.
	-- ['_'] = { 'fallback linter' },
	-- ["*"] = { "typos" },
}
