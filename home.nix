{
  config,
  pkgs,
  username,
  ...
}:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = username;
  home.homeDirectory = "/home/${username}";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  programs.carapace.enable = true;
  programs.direnv.enable = true;

  programs.git = {
    enable = true;
    delta.enable = true;

    extraConfig = import git/config.nix;
  };

  programs.helix = {
    enable = true;
    defaultEditor = true;
    ignores = [
      "!.gitignore"
      "!.github"
      "!.bazelrc"
      "!.bazelversion"
    ];

    extraPackages = [
      pkgs.bash-language-server
      pkgs.buildifier
      pkgs.dprint
      pkgs.marksman
      pkgs.nixd
      pkgs.nixfmt-rfc-style
      pkgs.ruff
      pkgs.rust-analyzer
      pkgs.starpls-bin
      pkgs.taplo
    ];
    languages = builtins.fromTOML (builtins.readFile ./helix/languages.toml);
    settings = builtins.fromTOML (builtins.readFile ./helix/config.toml);
  };

  programs.jujutsu.enable = true;
  programs.keychain.enable = true;

  programs.nushell = {
    enable = true;
    # extraConfig = builtins.readFile ./nushell/config.nu;
    configFile.source = ./nushell/config.nu;
    envFile.source = ./nushell/env.nu;
  };

  programs.starship.enable = true;
  programs.yazi.enable = true;
  programs.zellij.enable = true;
  programs.zoxide.enable = true;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    pkgs.google-cloud-sdk-gce
    pkgs.ripgrep
    pkgs.fd
    pkgs.tig
    pkgs.tokei
    pkgs.lazygit
    pkgs.lazyjj
    pkgs.tree
    pkgs.nixfmt-rfc-style
    pkgs.buildifier
    pkgs.starlark
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;
    ".local/bin/hxextra" = {
      source = ./bin/hxextra;
      executable = true;
    };

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/tshaynik/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "hx";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
