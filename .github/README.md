<p align="center">
    <img src="./assets/main/png/chess.png" alt="Snowflake" width="400" height="400"/>
</p>

<p align="center">
    <a href="https://github.com/Icy-Thought/snowflake/stargazers">
        <img alt="Stars" src="https://img.shields.io/github/stars/Icy-Thought/snowflake?style=for-the-badge&logo=starship&color=C9CBFF&logoColor=D9E0EE&labelColor=302D41">
    </a>
    <a href="https://github.com/Icy-Thought/snowflake/issues">
        <img alt="Issues" src="https://img.shields.io/github/issues/Icy-Thought/snowflake?style=for-the-badge&logo=gitbook&color=B5E8E0&logoColor=D9E0EE&labelColor=302D41">
    </a>
    <a href="https://github.com/Icy-Thought/snowflake">
        <img alt="Size" src="https://img.shields.io/github/repo-size/Icy-Thought/snowflake?style=for-the-badge&logo=github&color=F2CDCD&logoColor=D9E0EE&labelColor=302D41">
    </a>
</p>

<div align="center">
    <h4>
        <a href="https://github.com/Icy-Thought/nvim.d">Neovim Conf</a>
        <span> | </span>
        <a href="https://github.com/Icy-Thought/emacs.d">Emacs Conf</a>
        <span> | </span>
        <a href="https://github.com/Icy-Thought/Lost-in-Space">Deprecated Conf</a>
    </h4>
</div>

# Table of Contents

- [Desktop Preview](#desktop-preview)
  - [XMonad](#xmonad)
  - [Emacs](#emacs)
  - [Neovim](#neovim)
- [Introduction](#introduction)
- [Getting Started](#getting-started)
  - [Prepare System Environment for Nix-Flake](#prepare-system-environment-for-nix-flake)
- [Nix-Flake: Beginning of a Journey](#nix-flake-beginning-of-a-journey)
  - [Clone `Snowflake` and Link Files To Dir](#clone-snowflake-and-link-files-to-correct-path)
  - [Replacing Necessary Configuration Entries](#replacing-necessary-configuration-entries)
    - [Create Your Hosts Directory](#create-your-hosts-directory)
    - [Hardware-Configuration.nix](#hardware-configurationnix)
    - [Hide Your FileSystem From Nautilus & Dolphin](#hide-your-filesystem-from-nautilus--dolphin)
    - [Installing Nix-Flake System](#installing-nix-flake-system)
- [(Optional) Doom Emacs](#doom-emacs)
- [Congratulations! 🎉](#congratulations-)
- [Useful Links](#useful-links)
- [Special Thanks](#special-thanks)

# Desktop Preview

## XMonad

![XMonad](./assets/themes/catppuccin/xmonad.png)

## Emacs

![Emacs](./assets/themes/catppuccin/emacs.png)

## Neovim

![Neovim](./assets/themes/catppuccin/neovim.png)

# Introduction

A hamerspace containing a declarative NixOS environment consisting of many
hand-crafted configurations, ranging from: Kitty, Alacritty, Doom-Emacs, Zathura
to many other applications which can be found in the `./home` directory.

> [!WARNING]
> This repository, similar to other dotfile/configuration repositories, is subjected to change.
> The reader ought to read through the commit history before blindly cloning, fetching or updating the necessary files required to power up their NixOS environment!

In this README I will attempt to explain how to replicate my NixOS setup or
achieve similar feats. If you find this guide not as useful as it was designed
to be, do submit an issue requesting a change for the specific sections you
found to be confusing. _Or_ submit a push request (PR) to this repository and
hopefully we can produce better results together!

Throughout my Nix journey, I've came across two interesting projects and among
those projects are [Nix-Flakes](https://github.com/NixOS/nix) and
[Home-Manager](https://github.com/nix-community/home-manager). (_Both projects
will be introduced later in this README._) These projects have been setup in
such manner that allows its users to carefully tune their system environments to
their liking!

> [!NOTE]
> This project is still in its early stages! Henceforth one should familiarize themselves with the [Risks](https://github.com/nix-community/home-manager#words-of-warning) that comes with the usage of such experimental features.

# Getting Started

## NixOS Installer

As of `22.05` the NixOS ISO comes equipped with a well-developed installer that
reduces the "_complexity_" of installing NixOS on your device! Therefore manual
intervention should not be required for a minimal installation.

## Prepare System Environment for Nix-Flake

Don't forget to append the following lines of code to your `/etc/nixos/configuration.nix`:

```nix
nix.package = pkgs.nixUnstable;
nix.extraOptions = ''
  experimental-features = nix-command flakes
'';
```

# Nix-Flake: Beginning of a Journey

## 1. Cloning `Snowflake`

```sh
git clone --depth 1 https://github.com/Icy-Thought/Snowflake.git
```

## 2. Creating `hosts/deviceX` Directory

``` sh
cd snowflake && cp -r templates/hosts/desktop hosts/deviceX
```

where `deviceX` is the name you'd like to give to your host device. This name will later be used when installing/updating your new NixOS setup.

## 3. Replace `hardware.nix` with `hardware-configuration.nix`

Suggestion: before replacing `hardware.nix` with your `hardware-configuration.nix`:
1. view the file and see what you'd like to retain
2. rename the file (backup)
3. add your Nix generated hardware configuration `.nix` file
4. paste your desired configurations in your new file

**OTHERWISE**, proceed by executing the command below.

``` sh
cp /etc/nixos/hardware-configuration.nix deviceX/hardware.nix
```

## 4. Replacing Necessary Configuration Entries

### 1. Snowflake Directory

> [!WARNING]
>  As of now, my snowflake directory is expected to be placed inside
> `~/Workspace/public/snowflake/`. And for your configuration to work properly you are
> expected to place the directory in that exact location!

Modify `snowflake.dir` to point to the location where you are keeping the
snowflake repository:
https://github.com/Icy-Thought/snowflake/blob/f576ca018a7dd97e0f9d887835e2559e1e5cc02c/modules/options.nix#L26-L29

### 2. Hide Your File-system From Nautilus & Dolphin

> [!NOTE]
> By default, Nautilus & Dolphin does not hide system partitions from mounted devices category.

I have chosen to hide those partitions from the mounted devices category of the
mentioned file managers. I have also added several kernel parameters and other
device-specific configurations for my setup in this repository. (check
`hosts/deviceX`)

What you are required to edit is:

1. `fileSystems` entries in `default.nix`.
2. The specific parts which you wish to exclude from your setup, such as: kernel
   parameters, modules to disable, packages to be installed among other
   configurations.

_(Example): hiding `/boot` from Nautilus mounted devices._

```nix
fileSystems."/boot" = {
  device = "/dev/disk/by-label/BOOT";
  fsType = "vfat";
  options = [ "x-gvfs-hide" ]; # For hiding boot partition entry in Nautilus.
};
```

> [!WARNING]
> Make sure to replace `/dev/disk/by-uuid/xyz` (or `partuuid`) with `/dev/disk/by-label/X`, where `X` follows the label you have chosen to name your partitions with during your partition setup.

## 5. Installing Nix-Flake System

> [!NOTE]
> Before rebuilding our NixOS system with our new device configurations, we ought to make our `flake.nix` aware of our new device directory (`deviceX`). Otherwise you'll run into #25!

``` sh
git add ./hosts/deviceX/*
```

After completing your setup, there remains one command to be executed (device =
directory name of your device placed inside `hosts`, which in this case is `deviceX`:

```sh
nixos-rebuild switch --use-remote-sudo --flake .#deviceX --impure";
reboot
```

# Congratulations! 🎉

You have successful installed your/my personally hand-crafted/replicated
Nix-Flake environment. I hope it suits your needs, if not then you are always
welcome to propose changes or fork the project and customize the repository to
your heart's content!

# Useful Links

- [NixOS Manual](https://nixos.org/manual/nixos/stable): A manual for the
  newcomer to read and understand different parts of the NixOS distribution.
- [Home-Manager](https://github.com/nix-community/home-manager): Helps you
  manage your `~/home` related configurations.
- [Nix Pills](https://nixos.org/guides/nix-pills): A series written to
  familiarize the user with the Nix programming language.
- [Nixpkgs Unstable](https://nixos.org/manual/nix/unstable): A manual which
  introduces the Nix-language to people unfamiliar with the wonders of this
  language.
- [Nix Flake MVP](https://gist.github.com/edolstra/40da6e3a4d4ee8fd019395365e0772e7):
  A written Nix guide by [edolstra](https://github.com/edolstra).
- [A Tour of Nix](https://nixcloud.io/tour/?id=1): a beautifully crafted
  introduction into the Nix programming language.

# Special Thanks

- [hlissner/dotfiles](https://github.com/hlissner/dotfiles): nix-flake skeleton!
