{ pkgs, username, ... }:
{
  home = {
    username = username;
    homeDirectory = "/Users/${username}";
    stateVersion = "25.05";

    packages = with pkgs; [
      # todo
    ];

    shellAliases = {
      # todo
    };
  };

  programs = {
    home-manager.enable = true;
    
    git = {
      enable = true;
      userName = "ewantown";
      userEmail = "etown@duck.com";
      extraConfig = {
        core = {
          editor = "nano";
        };
        init = {
          defaultBranch = "master";
        };
      };
      ignores = [
        ".DS_Store"
        "*.*~"
      ];
    };

    # librewolf
    firefox = {
      enable = true;
      package = pkgs.librewolf;
      policies = {
        DisplayBookmarksToolbar = "never";
        DisplayMenuBar = "default-off";
        Preferences = {
          "browser.compactmode.show" = true;
          "browser.uidensity" = 1;
          "browser.toolbars.bookmarks.visibility" = false;
          "browser.search.suggest.enabled" = false;          
          "browser.urlbar.suggest.addons" = false;
          "browser.urlbar.suggest.clipboard" = false;
          "browser.urlbar.suggest.engines" = false;
          "browser.urlbar.suggest.history" = false;          
          "browser.urlbar.suggest.fakespot" = false;
          "browser.urlbar.suggest.mdn" = false;
          "browser.urlbar.suggest.pocket" = false;
          "browser.urlbar.suggest.quickactions" = false;
          "browser.urlbar.suggest.recentsearches" = false;
          "browser.urlbar.suggest.topsites" = false;
          "browser.urlbar.suggest.trending" = false;
          "browser.urlbar.suggest.yelp" = false;
        };
        ExtensionSettings = {
          # Block all, force some
          "*".installation_mode = "blocked";
          # uBlock Origin
          "uBlock0@raymondhill.net" = {
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
            installation_mode = "force_installed";            
          };
          # Bitwarden
          "446900e4-71c2-419f-a6a7-df9c091e268b" = {
            install_url = "https://addons.mozilla.org/firefox/downloads/file/4440363/bitwarden_password_manager/latest.xpi";
            installation_mode = "force_installed";
          };
          # Vimium      
          "d7742d87-e61d-4b78-b8a1-b469842139fa" = { 
            install_url = "https://addons.mozilla.org/en-US/firefox/addon/vimium-ff/latest.xpi";
            installation_mode = "force_installed";
          };
          # DarkReader
          "addon@darkreader.org" = { 
            install_url = "https://addons.mozilla.o…irefox/addon/darkreader/";
            installation_mode = "force_installed";
          };
        };
      };
    };    
  };    
}
