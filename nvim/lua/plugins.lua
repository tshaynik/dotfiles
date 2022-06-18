return require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  --------------
  -- Language --
  --------------
  use 'neovim/nvim-lspconfig'
  -- completion
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'
  use 'hrsh7th/nvim-cmp'

  -- snippets
  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'
  use 'rafamadriz/friendly-snippets'

  -- Load on a combination of conditions: specific filetypes or commands
  -- Also run code after load (see the "config" key)
  use {
    'w0rp/ale',
    ft = {'sh', 'zsh', 'bash', 'c', 'cpp', 'cmake', 'html', 'markdown', 'racket', 'vim', 'tex'},
    cmd = 'ALEEnable',
    config = 'vim.cmd[[ALEEnable]]'
  }

  -- Post-install/update hook with neovim command
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  -----------------
  -- Searching ----
  -----------------
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      }
    end
  }

  use {'datwaft/bubbly.nvim', config = function()
    -- Here you can add the configuration for the plugin
    vim.g.bubbly_palette = {
      background = "#34343c",
      foreground = "#c5cdd9",
      black = "#3e4249",
      red = "#ec7279",
      green = "#a0c980",
      yellow = "#deb974",
      blue = "#6cb6eb",
      purple = "#d38aea",
      cyan = "#5dbbc1",
      white = "#c5cdd9",
      lightgrey = "#57595e",
      darkgrey = "#404247",
    }
    vim.g.bubbly_statusline = {
      'mode',

      'truncate',

      'path',
      'branch',
      'signify',
      'gitsigns',
      'coc',

      'divisor',

      'filetype',
      'progress',
    }
  end}

  -- Use dependency and run lua function after load
  use {
    'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' },
    config = function() require('gitsigns').setup() end
  }

  use 'hkupty/iron.nvim'
  use 'editorconfig/editorconfig-vim'

  ----------------------------
  -- Language/Tool Specific --
  ----------------------------
  use 'hashivim/vim-terraform'
  use 'LnL7/vim-nix'
  use 'simrat39/rust-tools.nvim'

  ---------------------
  -- From old config --
  ---------------------
  use 'tpope/vim-surround'
  use 'tpope/vim-repeat'
  use 'Raimondi/delimitMate'
  use 'AndrewRadev/tagalong.vim'

  --Files
  --use 'airblade/vim-rooter'
  -- File Navigation
  use 'scrooloose/nerdcommenter'


  --------------------
  -- Colourscheme  ---
  --------------------

  use 'glepnir/zephyr-nvim'
  -- You can alias plugin names
  use {'dracula/vim', as = 'dracula'}
end)
