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
require("searching")

vim.cmd("colorscheme zephyr")

-- Format on save using LSP
vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]

require("luasnip.loaders.from_vscode").lazy_load()
require('telescope').load_extension('projects')
require('telescope').load_extension('media_files')
require('rust-tools').setup({})
require("null-ls").setup({
    sources = {
        require("null-ls").builtins.formatting.stylua,
        require("null-ls").builtins.diagnostics.eslint,
        require("null-ls").builtins.completion.spell,
    },
})
require('nu').setup{}
