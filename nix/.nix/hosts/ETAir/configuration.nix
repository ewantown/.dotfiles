{ pkgs, inputs, ...} @ args:

let username = "ET";
    mkImports = import ../../lib/mkImports.nix args;
in
{

  environment.systemPackages = [
    pkgs.emacs
  ];

  nix.settings.experimental-features = "nix-command flakes";

  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
  
  system.stateVersion = 5;

  nixpkgs.hostPlatform = "aarch64-darwin";

  imports = mkImports {
    inherit username;

    imports = [
      ./homebrew.nix
    ];
  };

  programs.bash.enable = true;
  
  environment.shells = with pkgs; [ bash ];  
  
  users.users.ET = {
    home = "/Users/ET";
    shell = pkgs.bash;
  };
 
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = aarch64-darwin
  '';
 
}
