vim.g.mapleader = " "
vim.g.localleader = "\\"

-- Modules
vim.cmd('source ~/.config/nvim/vim/mappings.vim')
vim.cmd('source ~/.config/nvim/old_config.vim')
require("plugins")
require("completion")
require("lsp")
require("treesitter")
require("options")
require("repl")

vim.cmd("colorscheme zephyr")

require('rust-tools').setup({})
