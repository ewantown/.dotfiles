{ pkgs, ... }:
 
{
  home.stateVersion = "25.05";

  home.packages = with pkgs; [
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

  programs.direnv = {
    enable = true;
  };
    
  imports = [
    # TODO
  ];
}
 
