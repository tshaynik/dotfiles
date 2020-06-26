{ pkgs, ... }:

{
  home.packages = with pkgs; [
    htop
    fortune
  ];

  programs.home-manager = {
    enable = true;
  };

  programs.neovim = {
    enable = true;
    extraPython3Packages = (ps: with ps; [ python-language-server mypy pylint black ]);
    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # General
      delimitMate
      fzf-vim
      vim-surround
      vim-rooter
      vim-tmux-navigator
      nerdtree
      nerdcommenter
      supertab
      vim-airline

      vim-fugitive
      vim-gitgutter

      # Language
      vim-markdown
      vim-nix
      vim-terraform
      vim-yaml

      # Python
      semshi

      #Theme
      gruvbox

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


      " Git bindings
      nnoremap <leader>ga :Git add %:p<CR><CR>
      nnoremap <leader>gb :Gblame<CR>
      nnoremap <leader>gs :Gstatus<CR>
      nnoremap <leader>ggt :GitGutterToggle<CR>

      " Spell checking
      nnoremap <leader>ce :setlocal spell spelllang=en_ca<CR>
      nnoremap <leader>cf :setlocal spell spelllang=fr<CR>
      nnoremap <leader>cn :setlocal nospell<CR>

      colorscheme gruvbox
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

    plugins = with pkgs.tmuxPlugins; [
      cpu
      vim-tmux-navigator
      gruvbox
      {
        plugin = resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
      {
        plugin = open;
        extraConfig = ''
          set -g @open-S 'https://www.duckduckgo.com/'
        '';
      }
    ];
  };

  programs.zathura = {
    enable = true;
    options = {
      notification-error-bg    =  "#282828"; # bg
      notification-error-fg    =  "#fb4934"; # bright:red
      notification-warning-bg  =  "#282828"; # bg
      notification-warning-fg  =  "#fabd2f"; # bright:yellow
      notification-bg          =  "#282828"; # bg
      notification-fg          =  "#b8bb26"; # bright:green
     
      completion-bg            =  "#504945"; # bg2
      completion-fg            =  "#ebdbb2"; # fg
      completion-group-bg      =  "#3c3836"; # bg1
      completion-group-fg      =  "#928374"; # gray
      completion-highlight-bg  =  "#83a598"; # bright:blue
      completion-highlight-fg  =  "#504945"; # bg2
             
      # Define the color in index mode         
      index-bg                 =  "#504945"; # bg2
      index-fg                 =  "#ebdbb2"; # fg
      index-active-bg          =  "#83a598"; # bright:blue
      index-active-fg          =  "#504945"; # bg2
    
      inputbar-bg              =  "#282828"; # bg
      inputbar-fg              =  "#ebdbb2"; # fg
 
      statusbar-bg             =  "#504945"; # bg2
      statusbar-fg             =  "#ebdbb2"; # fg
   
      highlight-color          =  "#fabd2f"; # bright:yellow
      highlight-active-color   =  "#fe8019"; # bright:orange
  
      default-bg               =  "#282828"; # bg
      default-fg               =  "#ebdbb2"; # fg
      render-loading           =  true     ;
      render-loading-bg        =  "#282828"; # bg
      render-loading-fg        =  "#ebdbb2"; # fg
              
      # Recolor book content's color           
      recolor-lightcolor       =  "#282828"; # bg
      recolor-darkcolor        =  "#ebdbb2"; # fg
      recolor                  =  "true"   ;
    };

  };

  services.dunst = {
    enable = true;
  };

  xdg = {
    enable = true;
  };
}
