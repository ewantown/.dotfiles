{ pkgs, ... }:
{
  system = {
    stateVersion = 5;

    activationScripts.postUserActivation.text = ''
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';

    defaults = {
      # See system.defaults... at https://daiderd.com/nix-darwin/manual/index.html
      
      controlcenter = {
        BatteryShowPercentage = true;        
      };
      
      dock = {
        tilesize = 36;        
        orientation = "left";
        autohide = true;
        show-recents = false;
        launchanim = true;
        mineffect = "suck";
        minimize-to-application = true;        
        persistent-apps = [
          {
            app = "/Applications/Emacs.app";
          }
          {
            app = "/Applications/Nix Apps/Librewolf.app";
          }
          {
            app = "/Applications/Spotify.app";
          }
          {
            app = "/Applications/ElectronMail.app";
          }
        ];
      };
      NSGlobalDomain = {        
        InitialKeyRepeat = 15;
        KeyRepeat = 3;
        
        "com.apple.swipescrolldirection" = false;
        "com.apple.sound.beep.feedback" = 0;
        
        NSDocumentSaveNewDocumentsToCloud = false;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSNavPanelExpandedStateForSaveMode2 = true;
      };

      # Customize settings that not supported by nix-darwin directly
      # See https://macos-defaults.com
      CustomUserPreferences = {
        "com.apple.desktopservices" = {
          # prevent .DS_Store on network and usb
          DSDontWriteNetworkStores = true;
          DSDontWriteUSBStores = true;
        };
        "com.apple.WindowManager" = {
          EnableStandardClickToShowDesktop = 0;
          StandardHideDesktopIcons = 0;
          HideDesktop = 0;
          StageManagerHideWidgets = 0;
          StandardHideWidgets = 0;
        };
        NSGlobalDomain = {
          WebKitDeveloperExtras = true;
        };
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
      swapLeftCommandAndLeftAlt = true;  
    };
  };

  # TouchID for sudo authentication
  security.pam.services.sudo_local.touchIdAuth = true;
}
  
