if [ -e /home/jimothy/.nix-profile/etc/profile.d/nix.sh ]; then . /home/jimothy/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
export PATH="$PATH:$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin:$GEM_HOME/bin"
export EDITOR=nvim
#export TERM=kitty
#export TERMINAL=$TERM
export MOZ_ENABLE_WAYLAND=1
export GRIM_DEFAULT_DIR=~/Screenshots
export PKG_CONFIG_PATH="$HOME/.local/share/pkgconfig"
# musl
# export LD=ld.bfd
export QT_QPA_PLATFORM=wayland
