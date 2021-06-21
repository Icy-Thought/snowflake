{ config, pkgs, ... }:

let
  cpupower = config.boot.kernelPackages.cpupower;
  perf = config.boot.kernelPackages.perf;

  kernelPkgs = with pkgs; [
    cpupower                                    # Examine/Tool powersaving features.
    perf                                        # Profile & Performance counter.
  ];

  defaultPkgs = with pkgs; [
    chrome-gnome-shell                          # Gnome Shell integration for Chrome.
  ];

  utilPkgs = with pkgs; [
    binutils                                    # Tools for manipulating binaries.
    moreutils                                   # An extension to GNU utils.
    killall                                     # Completely eradicate processes.
    direnv                                      # Shell extension to manage env.
    pciutils                                    # lspci, setpci & update-pciids support.
    psmisc                                      # Useful utilities which uses proc fs.
    wireguard                                   # Wireguard tools.
    gnupg                                       # Encrypt/Decrypt software.
    firejail                                    # Namespace-based sandboxing tool.
    exiftool                                    # Control file metadata.
    fail2ban					# Scans failed login attempts + bans IP.
    usbguard					# Protect against infected USB devices.
    xclip                                       # Copy/Paste in XOrg terminal.
    wl-clipboard                                # Wayland c-p/c-v.
    gh                                          # Official GitHub client.
    pv                                          # Progress-bar for mv/cp.
    fd                                          # faster find.
    exa                                         # better ls.
    ripgrep                                     # faster grep.
    skim                                        # faster fzf.
    tree 					# Tree view.
    bustle                                      # Sequence diagrams for D-Bus traffic.
    dfeet                                       # D-Bus debugger.
    diffoscope                                  # In-depth comparison tool.
    common-updater-scripts                      # Common nix updating scripts????
    zstd                                        # Undo-fu-session/undo-tree-compression.
    unrar                                       # Extracting files from rar files.
  ];

  envPkgs = with pkgs; [
    wayland                                     # Wayland window system code + protocol.
    mesa                                        # FOSS 3D Graphics Lib.
    mesa-demos                                  # Collection of demos/tests OpenGL & Mesa.
    vulkan-headers                              # Vulkan Header files + API registery.
    fish                                        # Shell with better defaults.
    iwd                                         # WPA_Supplicant alternative.
    pipewire                                    # Multimedia pipeline API.
    git                                         # Tool for git usage.
    podman                                      # Docker alternative.
    appimage-run 				# Enable AppImages support in NixOS.
    xdg-desktop-portal-gtk                      # DIP for sandboxed apps.
  ];

in {
  environment = {
    systemPackages = builtins.concatLists [
      # kernelPkgs
      defaultPkgs
      utilPkgs
      envPkgs
    ];

    variables = {
      VK_ICD_FILENAMES = [ "/run/opengl-driver/share/vulkan/icd.d/amd_icd64.json" ];
    };
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ 
      "Iosevka"
      "JetBrainsMono"
      ]; 
    })

    source-code-pro
    emacs-all-the-icons-fonts
    liberation_ttf
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];


}

