" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')

" Core editing
Plug 'tpope/vim-surround'
Plug 'Raimondi/delimitMate'

" Files
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'
Plug 'kien/ctrlp.vim'
"
" File Navigation
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'christoomey/vim-tmux-navigator'

" Language
Plug 'dense-analysis/ale'

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
Plug 'morhetz/gruvbox'


" Appearance
Plug 'vim-airline/vim-airline'

" Linting
Plug 'neomake/neomake'

" Repl
Plug 'hkupty/iron.nvim', {'commit': '16c52eaf18f2b4ffd986d5a4b36fcab47a4a9f90'}

"""""""""""""""""""""""""""""""
" Language Specific Plugins   "
"""""""""""""""""""""""""""""""
"

" Golang
Plug 'fatih/vim-go'
Plug 'ncm2/ncm2-go'
Plug 'majutsushi/tagbar'

" Python
Plug 'numirias/semshi'

" Haskell
Plug 'neovimhaskell/haskell-vim'

Plug 'tidalcycles/vim-tidal'

" Rust
Plug 'rust-lang/rust.vim'
Plug 'timonv/vim-cargo'

" Nix
Plug 'LnL7/vim-nix'

" Dhall
Plug 'vmchale/dhall-vim'

" Lilypond
Plug 'gisraptor/vim-lilypond-integrator'

" SQL
Plug 'vim-scripts/dbext.vim'

""""""""""""""""""""""""""""""""
" Tool Specific Plugins        "
""""""""""""""""""""""""""""""""
" Terraform
Plug 'hashivim/vim-terraform'
"hledger
Plug 'ledger/vim-ledger'

" Other
Plug 'vim-pandoc/vim-pandoc'
Plug 'stephpy/vim-yaml'
Plug 'tpope/vim-markdown'

call plug#end()

let mapleader=" "
let maplocalleader = "\\"

" `set`tings
set foldmethod=syntax
set number                   " add line numbers
set nocompatible
set hidden
set autoread
set showmatch               " Show matching brackets.
filetype plugin indent on
set mouse=a

" `let`tings
let NERDTreeShowHidden=1

"let g:deoplete#enable_at_startup = 1

set rtp+=~/.fzf/


"ale (language server protocol)
let g:ale_fix_on_save = 1
let g:ale_linters = {
\ 'rust': ['analyzer']
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['black', 'isort'],
\   'nix': ['nixpkgs-fmt'],
\   'rust': ['rustfmt'],
\}

nnoremap <Leader>ag :ALEGoToDefinition<CR>
nnoremap <Leader>at :ALEGoToTypeDefinition<CR>
nnoremap <Leader>ad :ALEDetail<CR>
nnoremap <Leader>ah :ALEHover<CR>
nnoremap <Leader>ar :ALERename<CR>

"nnoremap <Leader>ro :IronRepl<CR><Esc>:IronFocus<CR>
"nnoremap <Leader>rh :IronReplHere<CR>
"nnoremap <Leader>rf :IronFocus<CR>
"nmap <Leader>rs <Plug>(iron-send-motion)
"nmap <Leader>rl <Plug>(iron-send-line)
"vmap <Leader>rs <Plug>(iron-visual-motion)
"nmap <Leader>rr <Plug>(iron-repeat-cmd)
"nmap <Leader>rc <Plug>(iron-clear)
"nmap <Leader>ri <Plug>(iron-interrupt)
"nmap <Leader>r<CR> <Plug>(iron-cr)

" fzf
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>fb :Buffers<CR>
nnoremap <Leader>fr :Rg<CR>
nnoremap <Leader>fc :Commits<CR>
nnoremap <Leader>fm :Maps<CR>
nnoremap <leader>fh :History<CR>
nmap <leader>/ :Rg<CR>

"" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
"if has('conceal')
"  set conceallevel=2 concealcursor=niv
"endif

" Enable snipMate compatibility feature.
"let g:neosnippet#enable_snipmate_compatibility = 1

" Tell Neosnippet about the other snippets
"let g:neosnippet#snippets_directory='~/Documents/OpenSource/vim-snippets/snippets'
let g:neosnippet#snippets_directory='~/Documents/OpenSource/neosnippet-snippets/neosnippets'

let g:NERDDefaultAlign = 'left'

" Terraform
let g:terraform_fmt_on_save=1

" Markdown
augroup filetype_markdown
autocmd!
autocmd FileType markdown setl ts=4 sw=4 sts=4 expandtab
autocmd FileType markdown nnoremap <buffer> <Leader>b :w<CR>:!pandoc % -o %:r.pdf --pdf-engine=xelatex --variable mainfont="DejaVu Serif" --variable monofont="Fira Code" --variable mathfont="Fira Code"<CR>
augroup end

augroup filetype_haskell
autocmd!
autocmd FileType haskell set tabstop=4 expandtab smarttab
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
nnoremap <leader>cd :cd<CR>

"nnoremap <leader>ss :e.<CR>
nnoremap <Tab> :NERDTreeToggle<CR>
nnoremap <leader>sS :tabnew.<CR>
nnoremap <leader>sp :sp.<CR>
nnoremap <leader>sv :vs.<CR>
nnoremap <leader>vs :vs.<CR>
" this kind of works, but only because of
" vim-rooter

nnoremap <leader>nh :noh<CR>

"nnoremap <leader>pf :Files<CR>
"nnoremap <C-leader> :Ctrlleader<CR>
nnoremap <leader>bn :bn<CR>
nnoremap <leader>bp :bp<CR>
nnoremap <leader><Tab> :b#<CR>

" windows
nnoremap <leader>w <C-w>
nnoremap <A-w> <C-w>
nnoremap <leader>\| :vsp<CR><C-w>l
nnoremap <leader>- :sp<CR><C-w>h
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
nnoremap <leader>tt :tabnew<CR>:terminal<CR>a
nnoremap <C-h> gT
nnoremap <C-l> gt
inoremap <C-h> gT
inoremap <C-l> gt

" Use system clipboard
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>y "+y
nnoremap <leader>p "+p

" Git bindings
nnoremap <leader>ga :Git add %:p<CR><CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>ggt :GitGutterToggle<CR>

" Spell checking
nnoremap <leader>ce :setlocal spell spelllang=en_ca<CR>
nnoremap <leader>cf :setlocal spell spelllang=fr<CR>
nnoremap <leader>cn :setlocal nospell<CR>

au! BufNewFile,BufRead *.ttl,*.nt,*.nq  set filetype=turtle
au! BufNewFile,BufRead *.trig  set filetype=trig
au! BufNewFile,BufRead *.n3  set filetype=n3
au! BufNewFile,BufRead *.jsonld  set filetype=jsonld

colorscheme gruvbox

" for iron.nvim

luafile $HOME/.config/nvim/plugins.lua
