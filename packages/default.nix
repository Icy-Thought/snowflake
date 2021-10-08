{ config, pkgs, ... }:

let
  sysPkgs = with pkgs; [ wireguard killall xclip wl-clipboard ];

  altPkgs = with pkgs; [ exa skim ];

  utilPkgs = with pkgs; [ firejail exiftool ];

in { environment.systemPackages = sysPkgs ++ altPkgs ++ utilPkgs; }
