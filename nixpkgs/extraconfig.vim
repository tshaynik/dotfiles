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
let g:deoplete#enable_at_startup = 1

let g:ale_fix_on_save = 1
let g:ale_linters = {
\ 'rust': ['rls']
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['black', 'isort'],
\   'nix': ['nixpkgs-fmt'],
\   'rust': ['rustfmt'],
\}

let g:UltiSnipsExpandTrigger="<F2>"
let g:UltiSnipsJumpForwardTrigger="<c-]>"
let g:UltiSnipsJumpBackwardTrigger="<c-[>"

let g:NERDDefaultAlign = 'left'

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
