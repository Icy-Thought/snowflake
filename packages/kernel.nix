{ config, lib, pkgs, ... }:

let
  cpupower = config.boot.kernelPackages.cpupower;
  perf = config.boot.kernelPackages.perf;

  kernelPkgs = with pkgs; [
    cpupower # Examine/Tool powersaving features.
    perf # Profile & Performance counter.
  ];

in { environment.systemPackages = kernelPkgs; }
