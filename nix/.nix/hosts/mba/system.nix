{ pkgs, ...} @ args:

let username = "et";
    mkImports = import ../../lib/mkImports.nix args;
in
{
  system.stateVersion = 0;

  imports = mkImports {
    inherit username;

    imports = [
      ./homebrew.nix
    ];
  };
  
  services.nix-daemon.enable = true;

  environment.shells = with pkgs; [ bash ];
  
  users.users.et = {
    home = "/Users/ET";
    shell = pkgs.zsh;
  };
 
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = aarch64-darwin
  '';
 
}
