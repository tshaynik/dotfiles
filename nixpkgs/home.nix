{ pkgs, ... }:

{
  home.packages = with pkgs; [
    htop
    fortune
    nixpkgs-fmt

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

      # Language
      vim-markdown
      vim-nix
      vim-terraform
      vim-yaml
      deoplete-nvim
      ale

      # Python
      semshi
      deoplete-jedi

      #Theme
      gruvbox

    ];
    extraConfig = builtins.readFile (./extraconfig.vim);
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

  xdg = {
    enable = true;
  };
}
