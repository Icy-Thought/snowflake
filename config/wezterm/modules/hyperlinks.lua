local M = {}

M.hyperlink_rules = {
    { -- default if no hyperlink_rules specified
        regex = "\\b\\w+://(?:[\\w.-]+)\\.[a-z]{2,15}\\S*\\b",
        format = "$0",
    },
    { -- linkify email addresses
        regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
        format = "mailto:$0",
    },
    { -- file:// URI
        regex = "\\bfile://\\S*\\b",
        format = "$0",
    },
    { -- nixpkgs review current program
        regex = "nixpkgs-review pr (\\d+)",
        format = "https://github.com/NixOS/nixpkgs/pull/$1",
    },
    { -- TODO
        regex = "pr-(\\d+)-?\\d?",
        format = "https://github.com/NixOS/nixpkgs/pull/$1",
    },
    { -- nix flake github references
        regex = "github:([\\w\\d_-]+)/([\\w\\d_\\.-]+)",
        format = "https://github.com/$1/$2",
    },
    { -- nix flake github references with commit
        regex = "github:([\\w\\d_-]+)/([\\w\\d_\\.-]+)/([\\d\\w-]+)",
        format = "https://github.com/$1/$2/commit/$3",
    },
    { -- git ssh remote url
        regex = "git@(\\w+\\.\\w+):(\\w+/\\w+)\\.git",
        format = "https://$1/$2",
    },
}

return M
