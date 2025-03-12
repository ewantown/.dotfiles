{
  description = "Nix configuration";
 
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
 
    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
 
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };
 
  outputs = inputs @ { self, nixpkgs, ... }:
    let
      nixpkgsConfig = {
        config.allowUnfree = true;
      };
    in {
      darwinConfigurations =
        let
          inherit (inputs.nix-darwin.lib) darwinSystem;
        in {
          "mba" = darwinSystem {
            system = "aarch64-darwin";
            
            specialArgs = { inherit inputs; };
            
            modules = [
              inputs.nix-homebrew.darwinModules.nix-homebrew
              inputs.home-manager.darwinModules.home-manager
              ./.nix/hosts/mba/system.nix
              {
                nixpkgs = nixpkgsConfig;
                
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.et = import ./.nix/home/home.nix;
              }
            ]; 
          };
        };
    };
}
