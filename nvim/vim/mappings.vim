set foldmethod=syntax
set number                   " add line numbers
set nocompatible
set hidden
set autoread
set showmatch               " Show matching brackets.
filetype plugin indent on
set mouse=a
set termguicolors

" Telescope: https://github.com/nvim-telescope/telescope.nvim#pickers
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
nnoremap <leader>fc <cmd>lua require('telescope.builtin').git_commits()<cr>
nnoremap <leader>ft <cmd>lua require('telescope.builtin').treesitter()<cr>

" File Explorer
nnoremap <Tab> :Rexplore<CR>

" Use system clipboard
vnoremap <leader>y "+y
vnoremap <C-c> "+y
vnoremap <leader>p "+p
nnoremap <leader>y "+y
nnoremap <C-c> "+y
nnoremap <leader>p "+p
