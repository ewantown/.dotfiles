{
  description = "Nix config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
 
    darwin = {
      url = "github:lnl7/nix-darwin/master";      
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    #flake-utils.url = "github:numtide/flake-utils";
    
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";

    nixpkgs-firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
  };

  outputs = inputs @ { self, nixpkgs, darwin, home-manager, nix-homebrew, nixpkgs-firefox-darwin }:    
    let
      system = "aarch64-darwin";
      hostname = "ETAir";
      username = "et";
      specialArgs = inputs // { inherit hostname username; };
      nixpkgs-firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
    in {      
      darwinConfigurations."${hostname}" = darwin.lib.darwinSystem {
        inherit system specialArgs;        
        modules = [
          inputs.nix-homebrew.darwinModules.nix-homebrew

          ./modules/core.nix          
          ./modules/apps.nix
          ./modules/macos.nix
                    
          inputs.home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = specialArgs;
            home-manager.users.${username} = import ./modules/home.nix;
          }                    
        ];
      };
    };
}  
