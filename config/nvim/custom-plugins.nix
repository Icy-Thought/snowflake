pkgs:
let
  buildPlugin =
    (pkgs.vimUtils.override { vim = pkgs.vim; }).buildVimPluginFrom2Nix;
in with pkgs; {
  nvim-lsp-installer = buildPlugin rec {
    pname = "nvim-lsp-installer";
    version = "2022-03-11";
    src = pkgs.fetchFromGitHub {
      owner = "williamboman";
      repo = pname;
      rev = "a049c201279c7e48e1a3a68acb5a15a306671551";
      sha256 = "ieZ/2n7NXpJuS094gSArX6s5UGTKIlG36ZZNOdMSyIo=";
    };
  };
  fzy-lua-native = buildPlugin rec {
    pname = "fzy-lua-native";
    version = "2021-08-02";
    src = pkgs.fetchFromGitHub {
      owner = "romgrk";
      repo = pname;
      rev = "aa00feb01128c4d279c8471898e15898e75d5df5";
      sha256 = "JTldjJz5XnG9bgPfAYvjUaHYm0PtJ0MiDBQ9DfjSTQ8=";
    };
  };
}
