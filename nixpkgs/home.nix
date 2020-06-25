{ pkgs, ... }:

{
  home.packages = [
    pkgs.htop
    pkgs.fortune
  ];

  programs.home-manager = {
    enable = true;
  };

  programs.neovim = {
    enable = true;
    extraPython3Packages = (ps: with ps; [ python-language-server black ]);
    viAlias = true;
    vimAlias = true;

    plugins = with pkgs; [
      # General
      vimPlugins.delimitMate
      #vimPlugins.fzf.vim
      vimPlugins.vim-surround
      vimPlugins.vim-rooter
      vimPlugins.vim-tmux-navigator
      vimPlugins.nerdtree
      vimPlugins.nerdcommenter
      vimPlugins.supertab
      vimPlugins.vim-airline
      vimPlugins.neomake

      # Language
      vimPlugins.vim-markdown
      vimPlugins.vim-nix
      vimPlugins.vim-terraform
      vimPlugins.vim-yaml

      #Theme
      vimPlugins.gruvbox

    ];
    extraConfig = ''
      let mapleader=" "

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

     colorscheme gruvbox

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
    '';
  };

  programs.tmux = {
    enable = true;
    keyMode = "vi";
    terminal = "screen-256color";
    extraConfig = ''
      bind | split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"
    '';

    plugins = with pkgs; [
      tmuxPlugins.cpu
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.gruvbox
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
      {
        plugin = tmuxPlugins.open;
        extraConfig = ''
          set -g @open-S 'https://www.duckduckgo.com/'
        '';
      }
    ];
  };
}
