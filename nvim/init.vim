" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')

" Core editing
Plug 'tpope/vim-surround'
Plug 'Raimondi/delimitMate'

" Files
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'
"Plug 'Shougo/denite.nvim'
Plug 'kien/ctrlp.vim'
"Plug 'vim-ctrlspace/vim-ctrlspace'
"
" Nerdtree
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

" Completion
Plug 'ervandew/supertab'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Appearance
Plug 'rafi/awesome-vim-colorschemes'
"Plug 'dracula/vim', { 'as': 'dracula' }
"Plug 'tyrannicaltoucan/vim-deep-space'
Plug 'AlessandroYorba/Sierra'
Plug 'vim-airline/vim-airline'

" Linting
Plug 'neomake/neomake'

" Repl
Plug 'hkupty/iron.nvim', { 'do': ':UpdateRemotePlugins' }

"""""""""""""""""""""""""""""""
" Language Specific Plugins   "
"""""""""""""""""""""""""""""""
"
" Golang
Plug 'fatih/vim-go'
Plug 'ncm2/ncm2-go'
Plug 'majutsushi/tagbar'
" Plug 'zchee/deoplete-go', { 'do': 'make'}

" Python
Plug 'davidhalter/jedi-vim'
Plug 'zchee/deoplete-jedi'

" Haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'parsonsmatt/intero-neovim'
Plug 'eagletmt/neco-ghc'

" Bash

""""""""""""""""""""""""""""""""
" Tool Specific Plugins        "
""""""""""""""""""""""""""""""""
" Salt
Plug 'saltstack/salt-vim'
" Terraform
Plug 'hashivim/vim-terraform'

" Other
Plug 'vim-pandoc/vim-pandoc'
Plug 'stephpy/vim-yaml'
Plug 'tpope/vim-markdown'

call plug#end()

""""""""""""""""""""""""""""""""""
" General Keybindings            "
""""""""""""""""""""""""""""""""""
let mapleader=" "

" `set`tings
set foldmethod=syntax
set number                   " add line numbers
set nocompatible
set hidden
set autoread
"set showtabline=0
"set showmatch               " Show matching brackets.
"set ignorecase              " Do case insensitive matching
"set mouse=v                 " middle-click paste with mouse
"set hlsearch                " highlight search results
"set tabstop=4               " number of columns occupied by a tab character
"set softtabstop=4           " see multiple spaces as tabstops so <BS> does the right thing
"set expandtab               " converts tabs to white space
"set shiftwidth=4            " width for autoindents
"set autoindent              " indent a new line the same amount as the line just typed
"set wildmode=longest,list   " get bash-like tab completions
"set cc=100                   " set an 80 column border for good coding style
"set invspell
"set showmode
"set smartcase
"set smarttab
"set smartindent
"set autoindent
"set background=dark
"set laststatus=0
filetype off
filetype plugin indent on
set mouse=a


" `let`tings
let NERDTreeShowHidden=1
let g:deoplete#enable_at_startup = 1

"HASKELL
" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
" let g:haskell_classic_highlighting = 1
au Filetype haskell nnoremap ,s :InteroOpen<CR>
au Filetype haskell nnoremap ,S :InteroOpen<CR><C-w>ja
au Filetype haskell tnoremap <C-s> <C-\><C-n>:InteroHide<CR>
au Filetype haskell nnoremap ,h :InteroHide<CR>
au Filetype haskell nnoremap ,b :InteroLoadCurrentFile<CR>
au Filetype haskell nnoremap ,B :InteroOpen<CR>:InteroLoadCurrentFile<CR><C-w>ja
au Filetype haskell 
au Filetype haskell nnoremap ,tt :InteroType<CR>
au Filetype haskell nnoremap ,ti :InteroTypeInsert<CR>
au Filetype haskell nnoremap <Leader>d :InteroGoToDef<CR>
au Filetype haskell autocmd BufNewFile,BufRead *.hs setlocal tabstop=2 softtabstop=2 shiftwidth=2 smarttab expandtab

" Golang
let g:go_auto_type_info = 1
let g:go_fmt_command = "goimports"
let g:deoplete#sources#go#gocode_binary = '/home/danielkt/go/bin/gocode'
let g:go_snippet_engine = "neosnippet"

au FileType go nnoremap <Leader>d <Plug>(go-def)
au FileType go nnoremap <Leader>ms <Plug>(go-def-split)
au FileType go nnoremap <Leader>mv <Plug>(go-def-vertical)
au FileType go nnoremap <Leader>mi <Plug>(go-info)
au FileType go nnoremap <Leader>ml <Plug>(go-metalinter)
au FileType go nnoremap <leader>mr  <Plug>(go-run)
au FileType go nnoremap <leader>mb  <Plug>(go-build)
au FileType go nnoremap <leader>mt  <Plug>(go-test)
au FileType go nnoremap <leader>tc  <Plug>(go-test-compile)
au FileType go nnoremap <Leader>mm <Plug>(go-doc)
au FileType go nnoremap <Leader>me <Plug>(go-rename)
au FileType go nnoremap <leader>rt <Plug>(go-run-tab)
au FileType go nnoremap <Leader>rs <Plug>(go-run-split)
au FileType go nnoremap <Leader>rv <Plug>(go-run-vertical)

autocmd BufNewFile,BufRead *.go setlocal noet ts=4 sw=4 sts=4

" Terraform
let g:terraform_fmt_on_save=1

" Markdown
autocmd BufNewFile,BufReadPost *.md setl ts=4 sw=4 sts=4 expandtab

" should probably use leader
nmap <leader>s :NERDTreeToggle<CR>

" Files
nnoremap <leader>fs :w<CR>
nnoremap <leader>fc :e~/.config/nvim/init.vim<CR>
nnoremap <leader>cd :cd<CR>

" this kind of works, but only because of 
" vim-rooter
nmap <leader>/ :Ag<CR>

nnoremap <leader>nh :noh<CR>

" what was I just doing
"nnoremap <leader>pf :Files<CR>
"nnoremap <C-leader> :Ctrlleader<CR>
nnoremap <leader>rf :History<CR>
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>bn :bn<CR>
nnoremap <leader>bp :bp<CR>
nnoremap <leader><Tab> :b#<CR>

" windows
nnoremap <leader>w <C-w>
nnoremap <A-w> <C-w>
nnoremap <leader>w/ :vsp<CR><C-w>l
nnoremap <leader>w. :sp<CR><C-w>h
nnoremap <leader>wd :close<CR>

"" terminal support
map <leader>' :term<CR>
tnoremap <C-x> <C-\><C-n>
tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
tnoremap <A-+> <C-\><C-N><C-w><C-w>+
tnoremap <A--> <C-\><C-N><C-w><C-w>-
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l
nnoremap <A-+> <C-w>+
nnoremap <A--> <C-w>-
nnoremap <A-<> <C-w><
nnoremap <A->> <C-w>>
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-+> <C-\><C-N><C-w>+

"" tabs
nnoremap <leader>tn :tabnew<CR>
nnoremap <leader>td :tabclose<CR>
nnoremap <leader>th gT
nnoremap <leader>tl gt
nnoremap <C-h> gT
nnoremap <C-l> gt
inoremap <C-h> gT
inoremap <C-l> gt

" Use system clipboard
vnoremap <leader>y "+y 
vnoremap <leader>p "+p 
nnoremap <leader>y "+y 
nnoremap <leader>p "+p 

" colors!
" color dracula
set termguicolors
let g:sierra_Sunset = 1
colorscheme sierra 

" Run Neomake
call neomake#configure#automake('nw', 1000)

" Git bindings
nnoremap <leader>ga :Git add %:p<CR><CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>ggt :GitGutterToggle<CR>

