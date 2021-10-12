# https://github.com/cole-mickens/nixcfg/blob/main/mixins/_common/termsettings.nix
{ ... }: {

  fonts = rec {
    default = jetbrains;

    iosevka = {
      name = "Iosevka Nerd Font";
      size = 13;
    };

    jetbrains = {
      name = "JetBrainsMonoMedium Nerd Font";
      size = 13;
    };
  };

  colors = rec {
    default = ayuDark;

    ayuDark = {
      foreground = "#E6E1CF";
      background = "#0F1419";

      cursorForeground = "#1F2430";
      cursorBackground = "#F29718";

      selectionForeground = "#191F26";
      selectionBackground = "#253340";

      black = "#1D242C";
      brightBlack = "#686868";

      red = "#FF7733";
      brightRed = "#F07178";

      green = "#B8CC52";
      brightGreen = "#CBE645";

      yellow = "#FFB454";
      brightYellow = "#FFEE99";

      blue = "#36A3D9";
      brightBlue = "#6871FF";

      magenta = "#CA30C7";
      brightMagenta = "#FF77FF";

      cyan = "#95E6CB";
      brightCyan = "#A6FDE1";

      white = "#C7C7C7";
      brightWhite = "#FFFFFF";
    };

    oneDark = {
      foreground = "#ABB2BF";
      background = "#1E2127";

      cursorForeground = "#5C6370";
      cursorBackground = "#2C323C";

      selectionForeground = "#2C323C";
      selectionBackground = "#3E4451";

      black = "#1E2127";
      brightBlack = "#5C6370";

      red = "#E06C75";
      brightRed = "#E06C75";

      green = "#98C379";
      brightGreen = "#98C379";

      yellow = "#D19A66";
      brightYellow = "#D19A66";

      blue = "#61AFEF";
      brightBlue = "#61AFEF";

      magenta = "#C678DD";
      brightMagenta = "#C678DD";

      cyan = "#56B6C2";
      brightCyan = "#56B6C2";

      white = "#ABB2BF";
      brightWhite = "#FFFFFF";
    };
  };
}
