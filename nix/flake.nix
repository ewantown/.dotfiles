{
  description = "Nix configuration";
  
  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs";
 
    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
 
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };
 
  outputs = inputs@{ self, nixpkgs, nix-darwin, nix-homebrew, flake-utils, ... }:    
    let      
      nixpkgsConfig.config = {
        allowUnfree = true;
      };

      configuration = { pkgs, ... }: {
        environment.systemPackages = [
          pkgs.emacs
        ];

        nix.settings.experimental-features = "nix-command flakes";
        
        system.configurationRevision = self.rev or self.dirtyRev or null;
        
        system.stateVersion = 5;
        
        nixpkgs.hostPlatform = "aarch64-darwin";

        users.users.ET = {
          home = "/Users/ET";
          shell = pkgs.zsh;
        };
 
        nix.extraOptions = ''
            auto-optimise-store = true
            experimental-features = nix-command flakes
            extra-platforms = aarch64-darwin
        '';        
      };
    in {
      darwinConfigurations =
        let
          inherit (inputs.nix-darwin.lib) darwinSystem;
        in {
          ETAir = darwinSystem {
            system = "aarch64-darwin";            
            
            specialArgs = { inherit inputs; };
            
            modules = [
              inputs.nix-homebrew.darwinModules.nix-homebrew
              inputs.home-manager.darwinModules.home-manager
              ./.nix/hosts/ETAir/configuration.nix
              {
                nixpkgs = nixpkgsConfig;
                
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.ET = import ./.nix/home/home.nix;
              }
            ]; 
          };
        };
    } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in {
      packages = pkgs;
      
    }
  );
}
  
