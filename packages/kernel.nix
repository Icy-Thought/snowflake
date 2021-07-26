{ config, lib, pkgs, ... }:

let
  cpupower = config.boot.kernelPackages.cpupower;
  perf = config.boot.kernelPackages.perf;

  kernelPkgs = with pkgs; [
    cpupower # Examine/Tool Powersaving Features.
    perf # Profile & Performance Counter.
  ];

in { environment.systemPackages = kernelPkgs; }
