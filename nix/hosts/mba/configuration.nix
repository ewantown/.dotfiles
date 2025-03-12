_:
 
{
  services.nix-daemon.enable = true;
 
  users.users.et = {
    home = "/Users/ET";
  };
 
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = aarch64-darwin
  '';
 
  homebrew = {
    enable = true;
 
    casks = [
      # TODO
    ];
  };
}
