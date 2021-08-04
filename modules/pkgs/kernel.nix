{ config, lib, pkgs, ... }:

let
  cpupower = config.boot.kernelPackages.cpupower;
  perf = config.boot.kernelPackages.perf;

  kernelPkgs = [
    pkgs.cpupower # Examine/Tool Powersaving Features.
    pkgs.perf # Profile & Performance Counter.
  ];

in { environment.systemPackages = kernelPkgs; }
