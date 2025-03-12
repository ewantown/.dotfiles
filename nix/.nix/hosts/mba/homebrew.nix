_:

{
  nix-homebrew = {
    enable = true;

#    enableRosetta = true;

    user = "et";

    autoMigrate = true;
  };

  homebrew = {
    enable = true;


#    brews = [
#      "ollama"
#    ];
#
#    casks = [
#      "firefox"
#      "1password-cli"
#      "discord"
#      "alacritty"
#      "spotify"
#      "ghostty"
#      "rescuetime"
#    ];
  };
}
