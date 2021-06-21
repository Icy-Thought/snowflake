{ config, pkgs, ... }:

let
  defaultPkgs = with pkgs; [
    chrome-gnome-shell                                              # Gnome Shell integration for Chrome.
  ];

  utilPkgs = with pkgs; [
    moreutils                                                       # An extension to GNU utils.
    direnv                                                          # Shell extension to manage env.
    pciutils                                                        # lspci, setpci & update-pciids support.
    psmisc                                                          # Useful utilities which uses proc fs.
    wireguard                                                       # Wireguard tools.
    gnupg                                                           # Encrypt/Decrypt software.
    firejail                                                        # Namespace-based sandboxing tool.
    fail2ban							    # Scans for failed login attempts + bans IP.
    usbguard							    # Protect against infected USB devices.
    unrar                                                           # Enable extracting files from rar files.
    wl-clipboard                                                    # Wayland clipboard.
    xclip                                                           # Copy/Paste in XOrg terminal.
    pv                                                              # Progress-bar for mv/cp.
    fd                                                              # faster find.
    exa                                                             # better ls.
    ripgrep                                                         # faster grep.
    skim                                                            # faster fzf.
    tree 							    # Tree view.
    bustle                                                          # Draw sequence diagrams of D-Bus traffic.
    dfeet                                                           # D-Bus debugger.
    diffoscope                                                      # In-depth comparison tool.
    common-updater-scripts                                          # Common nix updating scripts????
    zstd                                                            # Undo-fu-session/undo-tree-compression.
    gh                                                              # Official GitHub client.
    upower                                                          # D-Bus service for power management.
    binutils                                                        # Tools for manipulating binaries.
    exiftool                                                        # Control file metadata.
  ];

  envPkgs = with pkgs; [
    wayland                                             	    # Wayland window system code + protocol.
    mesa                                                	    # FOSS 3D Graphics Lib.
    mesa-demos                                          	    # Collection of demos/tests OpenGL & Mesa.
    vulkan-headers                                      	    # Vulkan Header files + API registery.
    fish                                                	    # Shell with better defaults.
    iwd                                                 	    # WPA_Supplicant alternative.
    pipewire                                            	    # Multimedia pipeline API.
    git                                                 	    # Tool for git usage.
    podman                                              	    # Docker alternative.
    appimage-run 						    # Enable AppImages support in NixOS.
  ];

in {
  environment.systemPackages = builtins.concatLists [
    defaultPkgs
    utilPkgs
    envPkgs
  ];

}

