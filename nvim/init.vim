" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')

" Core editing
Plug 'tpope/vim-surround'
Plug 'Raimondi/delimitMate'

" Files
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'
"Plug 'Shougo/denite.nvim'
Plug 'kien/ctrlp.vim'
"Plug 'vim-ctrlspace/vim-ctrlspace'
"
" File Navigation
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
"Plug 'Xuyuanp/nerdtree-git-plugin'
"Plug 'justinmk/vim-dirvish'
"Plug 'kristijanhusak/vim-dirvish-git'

" Completion
Plug 'ervandew/supertab'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neco-syntax'
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Theme
"Plug 'dracula/vim', { 'as': 'dracula' }
"Plug 'tyrannicaltoucan/vim-deep-space'
"Plug 'AlessandroYorba/Sierra'
"Plug 'ajmwagar/vim-deus'
Plug 'AlessandroYorba/Alduin'


" Appearance
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
Plug 'psf/black', { 'branch': 'stable' }

" Haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'parsonsmatt/intero-neovim'
Plug 'eagletmt/neco-ghc'

Plug 'tidalcycles/vim-tidal'

" Rust
Plug 'rust-lang/rust.vim'
Plug 'sebastianmarkow/deoplete-rust'

" Nix
Plug 'LnL7/vim-nix'

" Dhall
Plug 'vmchale/dhall-vim'

" Lilypond
Plug 'gisraptor/vim-lilypond-integrator'

" ABC notation
"Plug 'leesavide/abc-vim'

" SQL
Plug 'vim-scripts/dbext.vim'

""""""""""""""""""""""""""""""""
" Tool Specific Plugins        "
""""""""""""""""""""""""""""""""
" Puppet
Plug 'rodjek/vim-puppet'
" Salt
Plug 'saltstack/salt-vim'
" Terraform
Plug 'hashivim/vim-terraform'
"hledger
Plug 'ledger/vim-ledger'

" Other
"Plug 'vim-pandoc/vim-pandoc'
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
set showmatch               " Show matching brackets.
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
filetype plugin indent on
set mouse=a


" `let`tings
let NERDTreeShowHidden=1
let g:deoplete#enable_at_startup = 1

"HASKELL
setlocal omnifunc=necoghc#omnifunc
let g:intero_type_on_hover = 1
let g:intero_backend = {
	\ 'command': 'cabal repl',
	\ 'options': '',
	\ 'cwd': expand('%:p:h'),
	\}

let g:tidal_target = "terminal"

augroup filetype_haskell
  autocmd!
  autocmd FileType haskell nnoremap <buffer> <leader>ho :InteroOpen<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>hO :InteroOpen<CR><C-w>ja
  autocmd FileType haskell nnoremap <buffer> <leader>hh :InteroHide<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>hl :InteroLoadCurrentFile<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>hL :InteroOpen<CR>:InteroLoadCurrentFile<CR><C-w>ja
  autocmd FileType haskell nnoremap <buffer> <leader>ht :InteroType<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>hT :InteroTypeInsert<CR>
  autocmd FileType haskell nnoremap <buffer> <Leader>hd :InteroGoToDef<CR>
  autocmd FileType haskell tnoremap <buffer> <C-h> <C-\><C-n>:InteroHide<CR>
  autocmd FileType haskell setlocal tabstop=2 softtabstop=2 shiftwidth=2 smarttab expandtab
augroup end

" Rust
let g:rustfmt_autosave = 1
let g:deoplete#sources#rust#racer_binary='/home/mandolin/.cargo/bin/racer'
let g:deoplete#sources#rust#rust_source_path='$HOME/ref/rust/src'

augroup rust-mapping
  autocmd!
  autocmd filetype rust nnoremap <buffer> gD <Plug>DeopleteRustGoToDefinitionTab
augroup end


" Golang
let g:go_auto_type_info = 1
let g:go_fmt_command = "goimports"
let g:deoplete#sources#go#gocode_binary = '/home/danielkt/go/bin/gocode'
let g:go_snippet_engine = "neosnippet"
let g:go_version_warning = 0

augroup filetype_go
  autocmd!
  autocmd FileType go nnoremap <buffer> <Leader>d <Plug>(go-def)
  autocmd FileType go nnoremap <buffer> <Leader>ms <Plug>(go-def-split)
  autocmd FileType go nnoremap <buffer> <Leader>mv <Plug>(go-def-vertical)
  autocmd FileType go nnoremap <buffer> <Leader>mi <Plug>(go-info)
  autocmd FileType go nnoremap <buffer> <Leader>ml <Plug>(go-metalinter)
  autocmd FileType go nnoremap <buffer> <leader>mr <Plug>(go-run)
  autocmd FileType go nnoremap <buffer> <leader>mb <Plug>(go-build)
  autocmd FileType go nnoremap <buffer> <leader>mt <Plug>(go-test)
  autocmd FileType go nnoremap <buffer> <leader>tc <Plug>(go-test-compile)
  autocmd FileType go nnoremap <buffer> <Leader>mm <Plug>(go-doc)
  autocmd FileType go nnoremap <buffer> <Leader>me <Plug>(go-rename)
  autocmd FileType go nnoremap <buffer> <leader>rt <Plug>(go-run-tab)
  autocmd FileType go nnoremap <buffer> <Leader>rs <Plug>(go-run-split)
  autocmd FileType go nnoremap <buffer> <Leader>rv <Plug>(go-run-vertical)
  autocmd FileType go setlocal noet ts=4 sw=4 sts=4
augroup end

augroup filetype_sql
  autocmd FileType sql setlocal noet ts=4 sw=4 sts=4
augroup end

" Terraform
let g:terraform_fmt_on_save=1

" Markdown
augroup filetype_markdown
  autocmd!
  autocmd FileType markdown setl ts=4 sw=4 sts=4 expandtab
  autocmd FileType markdown nnoremap <buffer> <Leader>b :w<CR>:!pandoc % -o %:r.pdf --pdf-engine=xelatex --variable mainfont="DejaVu Serif" --variable monofont="Fira Code" --variable mathfont="Fira Code"<CR>
augroup end

augroup nonvim
   au!
   au BufRead *.png,*.jpg,*.pdf,*.gif,*.xls*,*.ppt*,*.doc*,*.rtf sil exe "!xdg-open " . shellescape(expand("%:p")) | bd | let &ft=&ft
augroup end

augroup filetype_lilypond
  autocmd!
  autocmd FileType lilypond nnoremap <buffer> <Leader>b :w<CR>:!lilypond %<CR>
  "autocmd FileType lilypond nnoremap <buffer> <Leader>o :!xdg-open out/%:t:r.pdf<CR>
  "autocmd FileType lilypond nnoremap <buffer> <Leader>p :!timidity out/%:t:r.midi<CR>
  autocmd FileType lilypond nnoremap <buffer> <Leader>o :!xdg-open %:t:r.pdf<CR>
  autocmd FileType lilypond nnoremap <buffer> <Leader>p :!timidity %:t:r.midi<CR>
augroup end

" Files
nnoremap <leader>fs :w<CR>
nnoremap <leader>fc :tabnew ~/.config/nvim/init.vim<CR>
nnoremap <leader>cd :cd<CR>

"nnoremap <leader>ss :e.<CR>
nnoremap <leader>ss :NERDTreeToggle<CR>
nnoremap <Tab> :NERDTreeToggle<CR>
nnoremap <leader>sS :tabnew.<CR>
nnoremap <leader>sp :sp.<CR>
nnoremap <leader>sv :vs.<CR>
nnoremap <leader>vs :vs.<CR>
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

" Colours!
let g:alduin_Shout_Dragon_Aspect = 1
let g:alduin_Shout_Fire_Breath = 1
colorscheme alduin


" Run Neomake
call neomake#configure#automake('nw', 1000)

" Git bindings
nnoremap <leader>ga :Git add %:p<CR><CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>ggt :GitGutterToggle<CR>

" Spell checking
nnoremap <leader>ce :setlocal spell spelllang=en_ca<CR>
nnoremap <leader>cf :setlocal spell spelllang=fr<CR>
nnoremap <leader>cn :setlocal nospell<CR>
