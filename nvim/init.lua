vim.cmd('source ~/.config/nvim/old_config.vim')

require("zephyr") --colorscheme

-- Packages
require "paq" {
    "savq/paq-nvim";                  -- Let Paq manage itself

    "neovim/nvim-lspconfig";          -- LSP configurations
    "hrsh7th/nvim-compe";             -- completion
    "simrat39/rust-tools.nvim";

    -- Optional dependencies for rust-tools
    "nvim-lua/popup.nvim";
    "nvim-lua/plenary.nvim";
    "nvim-telescope/telescope.nvim";

    {"lervag/vimtex", opt=true};      -- Use braces when passing options

    "editorconfig/editorconfig-vim";
}

-- Modules
require("completion")
require("lsp")
require("options")
