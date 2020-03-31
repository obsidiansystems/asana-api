let pkgs = (import ./reflex-platform {}).nixpkgs;
in { asana-api = import ./openapi-hs-generator {
    inherit pkgs;
    specFile = (pkgs.fetchFromGitHub {
      owner = "Asana";
      repo = "developer-docs";
      rev = "5c626f93d80cb1d1dc63696557f3bda4abe25a21";
      sha256 = "0y0ki2x13vrhmz1bjag2z378sdi24x5dy67w4i6ssrjpgjb2z0dl";
    }) + "/defs/asana_oas.yaml";
    packageName = "asana-api";
    baseModule = "Asana";
    versionFn = x: x + ".0.0";
    cabalFileReplacements = {
      "Author Name Here" = "Obsidian Systems LLC";
      "author.name@email.com" = "maintainer@obsidian.systems";
      "YEAR - AUTHOR" = "2020 Obsidian Systems LLC";
      "UnspecifiedLicense" = "BSD3";
      "extra-source-files:" = "extra-source-files:\n    ChangeLog.md";
      "cabal-version:  >= 1.10" = ''
        cabal-version:  >= 1.10
        tested-with: GHC ==8.6.5
        license-file: LICENSE
      '';
      # Package version changes:
      "http-media >= 0.4 && < 0.8" = "http-media >= 0.4 && < 0.9";
      "http-client >=0.5 && <0.6" = "http-client >=0.5 && <0.7";
    };
    cabalFileAppendix = ''
      source-repository head
        type: git
        location: https://github.com/obsidiansystems/asana-api
    '';
  };
}
