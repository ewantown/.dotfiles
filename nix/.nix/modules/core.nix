{ pkgs, lib, username, hostname, system, ... } @ args:
{
  # services.nix-daemon-enable = true;
  
  nixpkgs.config = {
    allowUnfree = true;
    hostPlatform = system;
  };

  nix.gc = {
    automatic = lib.mkDefault true;
    options = lib.mkDefault "--delete-older-than 7d";
  };
  nix.package = pkgs.nix;
  
  nix.settings = {
    experimental-features = [ "nix-command" "flakes"];
    auto-optimise-store = false;
    trusted-users = [username];
  };

  users.users."${username}" = {
    home = "/Users/${username}";
    description = username;
  };  
}
