{ pkgs, ... }:
 
{
  home.stateVersion = "23.11";
 
  home.packages = with pkgs; [
    emacs
    # TODO
  ];
 
#  programs.zsh = {
#    enable = true;
# 
#    shellAliases = {
#      ls = "ls --color";
#    };
  programs.bash = {
    enable = true;
  };
}
 
