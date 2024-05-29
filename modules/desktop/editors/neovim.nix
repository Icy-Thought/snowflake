{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.modules) mkIf mkMerge;

  cfg = config.modules.desktop.editors.neovim;
in {
  options.modules.desktop.editors.neovim = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) enum nullOr;
  in {
    enable = mkEnableOption "Spread the joy of neovim in our flake";
    template = mkOption {
      type = nullOr (enum ["agasaya" "ereshkigal"]);
      default = "agasaya";
      description = "Which Neovim configuration to setup.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = attrValues (
        optionalAttrs (config.modules.develop.cc.enable == false) {
          inherit (pkgs) gcc; # Treesitter
        }
      );

      programs.neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;
      };
    }

    (mkIf (cfg.template == "agasaya") {
      modules.develop.lua.enable = true;

      home.configFile = {
        agasaya-config = {
          target = "nvim";
          source = "${inputs.nvim-dir}";
          recursive = true;
        };

        agasaya-init = {
          target = "nvim/init.lua";
          text = ''
            -- THIS (`init.lua`) FILE WAS GENERATED BY HOME-MANAGER.
            -- REFRAIN FROM MODIFYING IT DIRECTLY!

            local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

            if not vim.loop.fs_stat(lazypath) then
                vim.fn.system({
                    "git",
                    "clone",
                    "--filter=blob:none",
                    "--single-branch",
                    "https://github.com/folke/lazy.nvim.git",
                    lazypath,
                })
            end

            vim.opt.runtimepath:prepend(lazypath)

            -- Point Nvim to correct sqlite path
            vim.g.sqlite_clib_path = "${pkgs.sqlite.out}/lib/libsqlite3.so"

            -- Call-forward Agasaya:
            require("config").init()
          '';
        };
      };
    })

    (mkIf (cfg.template == "ereshkigal") {
      modules.develop.lua.fennel.enable = true;

      home.configFile = {
        ereshkigal-config = {
          source = "${inputs.nvim-dir}";
          target = "nvim";
          recursive = true;
        };
        ereshkigal-init = {
          target = "nvim/init.lua";
          text = ''
            -- THIS (`init.lua`) FILE WAS GENERATED BY HOME-MANAGER.
            -- REFRAIN FROM MODIFYING IT DIRECTLY!

            local function fprint(string, ...)
                print(string.format(string, ...))
            end

            local function plugin_status(status)
                if not status then
                    return "start/"
                else
                    return "opt/"
                end
            end

            local function assert_installed(plugin, branch, status)
                local _, _, plugin_name = string.find(plugin, [[%S+/(%S+)]])
                local plugin_path = vim.fn.stdpath("data")
                    .. "/site/pack/packer/"
                    .. plugin_status(status)
                    .. plugin_name
                if vim.fn.empty(vim.fn.glob(plugin_path)) > 0 then
                    fprint(
                        "Couldn't find '%s'. Cloning a new copy to %s",
                        plugin_name,
                        plugin_path
                    )
                    if branch > 0 then
                        vim.fn.system({
                            "git",
                            "clone",
                            "https://github.com/" .. plugin,
                            "--branch",
                            branch,
                            plugin_path,
                        })
                    else
                        vim.fn.system({
                            "git",
                            "clone",
                            "https://github.com/" .. plugin,
                            plugin_path,
                        })
                    end
                end
            end

            assert_installed("wbthomason/packer.nvim", nil, true)
            assert_installed("rktjmp/hotpot.nvim", "nightly")

            -- Point Nvim to correct sqlite path
            vim.g.sqlite_clib_path = "${pkgs.sqlite.out}/lib/libsqlite3.so"

            if pcall(require, "hotpot") then
                require("hotpot").setup({
                    modules = { correlate = true },
                    provide_require_fennel = true,
                })
                require("core.init")
            else
                print("Failed to require Hotpot")
            end
          '';
        };
      };
    })
  ]);
}
