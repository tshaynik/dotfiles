Dotfiles
========

My home-manager configuration.

To install on a new computer.

1. Make sure Nix is installed
1. Clone the repo: `git clone git@github.com:tshaynik/dotfiles.git`
1. Add an additional `homeConfiguration` in `flake.nix` for your user
1. Run home manager: `nix run home-manager/master -- switch`
