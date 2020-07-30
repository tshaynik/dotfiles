{ pkgs, ... }:

let
  vim-lilypond = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-lilypond";
    version = "2019-03-04";
    src = pkgs.fetchFromGitHub {
      owner = "sersorrel";
      repo = "vim-lilypond";
      rev = "5937606dc917a27c65474cdbb3b98cef2c67f078";
      sha256 = "1mrzcb0yzqhmfhg5wga2ggy1nnkw7ljh0vmz6r9bc9hv5c7cpc0s";
    };
    meta.homepage = "https://github.com/sersorrel/vim-lilypond/";
  };

  vim-prolog = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-prolog";
    version = "2013-08-25";
    src = pkgs.fetchFromGitHub {
      owner = "mxw";
      repo = "vim-prolog";
      rev = "093235a78012032b7d53b0e06757bf919380bf3b";
      sha256 = "11mii54r8w83rlv4mic2j3v2jpna4d5gsyjpih0kp9jqkmqcq3zs";
    };
    meta.homepage = "https://github.com/mxw/vim-prolog/";
  };

  vim-rdf = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-rdf";
    version = "2016-06-04";
    src = pkgs.fetchFromGitHub {
      owner = "niklasl";
      repo = "vim-rdf";
      rev = "4b3b4c9f079b65d2b525f567715f0aabe60cfdf3";
      sha256 = "051piyf403k98zxjv0qm68qhgajwv57hblxdab2awsyvk9gzv5lc";
    };
    meta.homepage = "https://github.com/niklasl/vim-rdf/";
  };

in
{
  home.packages = with pkgs; [
    direnv
    htop
    fortune
    nixpkgs-fmt
    rls
    rustfmt

    (
      python37Full.withPackages (
        ps: with ps; [
          jedi
          mypy
          pylint
          black
          ipython
          jupyterlab
        ]
      )
    )
  ];

  programs.home-manager = {
    enable = true;
  };

  programs.kitty = {
    enable = true;

    font.package = pkgs.fira-code;
    font.name = "Fira Code";

    settings = {
      font_size = 13;
      scrollback_lines = 10000;
      enable_audio_bell = false;
      update_check_interval = 0;
      copy_on_select = "yes";
    };
  };

  programs.neovim = {
    enable = true;
    extraPython3Packages = (ps: with ps; [ python-language-server isort mypy pylint black ]);
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

      vim-obsession

      vim-fugitive
      vim-gitgutter

      wmgraphviz-vim

      # Language
      vim-markdown
      vim-nix
      vim-terraform
      vim-yaml
      deoplete-nvim
      neco-syntax
      ale

      neosnippet-vim
      neosnippet-snippets

      # Python
      semshi
      deoplete-jedi

      # Haskell
      ghcid

      # Rust
      deoplete-rust

      #Theme
      gruvbox

      vim-lilypond
      vim-rdf
      vim-prolog
    ];
    extraConfig = builtins.readFile (./extraconfig.vim);
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
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
      selection-clipboard = "clipboard";

      #colors
      notification-error-bg = "#282828"; # bg
      notification-error-fg = "#fb4934"; # bright:red
      notification-warning-bg = "#282828"; # bg
      notification-warning-fg = "#fabd2f"; # bright:yellow
      notification-bg = "#282828"; # bg
      notification-fg = "#b8bb26"; # bright:green

      completion-bg = "#504945"; # bg2
      completion-fg = "#ebdbb2"; # fg
      completion-group-bg = "#3c3836"; # bg1
      completion-group-fg = "#928374"; # gray
      completion-highlight-bg = "#83a598"; # bright:blue
      completion-highlight-fg = "#504945"; # bg2

      # Define the color in index mode         
      index-bg = "#504945"; # bg2
      index-fg = "#ebdbb2"; # fg
      index-active-bg = "#83a598"; # bright:blue
      index-active-fg = "#504945"; # bg2

      inputbar-bg = "#282828"; # bg
      inputbar-fg = "#ebdbb2"; # fg

      statusbar-bg = "#504945"; # bg2
      statusbar-fg = "#ebdbb2"; # fg

      highlight-color = "#fabd2f"; # bright:yellow
      highlight-active-color = "#fe8019"; # bright:orange

      default-bg = "#282828"; # bg
      default-fg = "#ebdbb2"; # fg
      render-loading = true;
      render-loading-bg = "#282828"; # bg
      render-loading-fg = "#ebdbb2"; # fg

      # Recolor book content's color           
      recolor-lightcolor = "#282828"; # bg
      recolor-darkcolor = "#ebdbb2"; # fg
      recolor = "true";
    };
  };

  services.dunst = {
    enable = true;
  };

  services.lorri = {
    enable = true;
  };

  xdg = {
    enable = true;
  };
}
