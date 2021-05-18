{ config, lib, pkgs, ... }:
{

  programs.neovim = {
    enable       = true;
    viAlias      = true;
    vimAlias     = true;
    vimdiffAlias = true;
    withPython3  = true; # for plugins
  };

  xdg.configFile."../config/nvim/init.lua".source                   = "init.lua";
  xdg.configFile."../config/nvim/lua/compe-completion.lua".source   = "lua/compe-completion.lua";
  xdg.configFile."../config/nvim/lua/file-icons.lua".source         = "lua/file-icons.lua";
  xdg.configFile."../config/nvim/lua/gitsigns-nvim.lua".source      = "lua/gitsigns-nvim.lua";
  xdg.configFile."../config/nvim/lua/misc-utils.lua".source         = "lua/misc-utils.lua";
  xdg.configFile."../config/nvim/lua/nvim-lspconfig.lua".source     = "lua/nvim-lspconfig.lua";
  xdg.configFile."../config/nvim/lua/nvimTree.lua".source           = "lua/nvimTree.lua";
  xdg.configFile."../config/nvim/lua/pluginList.lua".source         = "lua/pluginList.lua";
  xdg.configFile."../config/nvim/lua/statusline.lua".source         = "lua/statusline.lua";
  xdg.configFile."../config/nvim/lua/telescope-nvim.lua".source     = "lua/telescope-nvim.lua";
  xdg.configFile."../config/nvim/lua/top-bufferline.lua".source     = "lua/top-bufferline.lua";
  xdg.configFile."../config/nvim/lua/treesitter-nvim.lua".source    = "lua/treesitter-nvim.lua";
  xdg.configFile."../config/nvim/lua/zenmode.lua".source            = "lua/zenmode.lua";

}
