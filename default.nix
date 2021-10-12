{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
, withHoogle ? false

# to spin up localhost:8080 hoogle use:
# nix-shell --arg withHoogle true -A shells.ghc --command "hoogle server -p 8080 --local"
}:
with obelisk;

let
  withHoogle = true;
  pkgs = import <nixpkgs> {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  polysemySrc = fetchGit {
    "url" = "https://github.com/polysemy-research/polysemy.git";
    "rev" = "8b80661f5df524cf37a159c73cb698af83479b37";
  };

  polysemyPluginSrc = polysemySrc + "/polysemy-plugin";

  # reflexCodeMirrorSrc = fetchGit {
  #   url = https://github.com/Atidot/reflex-codemirror;
  #   rev = "b115596528f26a9574d8bc48eb792b32d64abfb4";
  # };

  # reflexUtilsSrc = fetchGit {
  #   url = https://github.com/atidot/reflex-utils;
  #   rev = "c3c13716ae53f6fefdddc673640d861e91383c56";
  # };

  # reflexFileApiSrc = fetchGit {
  #   url = https://github.com/santiweight/reflex-fileapi;
  #   rev = "f2bfa8e212c9d524633816f3ffe8c786feef93f3";
  # };

  # reflexJExcelSrc = fetchGit {
  #   url = https://github.com/santiweight/reflex-jexcel;
  #   rev = "8eaaa86763b8386d1d2a2856d0fe9d419787b1d4";
  # };

  servantReflexSrc = fetchGit {
    url = "https://github.com/imalsogreg/servant-reflex";
    rev = "37a3e8f2566627d910df140982bd49bf4dba171e";
  };

  servantSnapSrc = pkgs.fetchFromGitHub {
    owner  = "haskell-servant";
    repo   = "servant-snap";
    rev    = "af5172a6de5bb2a07eb1bf4c85952075ec6ecdf3";
    sha256 = "0973iq3gc36qhiqnf5vp5djqsrz70srw1r7g73v8wamw04dx0g3z";
  };
in
project ./. ({ pkgs, ... }: {
  inherit withHoogle;
  overrides = self: super: {
    polysemy-plugin = pkgs.haskell.lib.dontCheck (self.callCabal2nix "polysemy-plugin" polysemyPluginSrc {});
    polysemy = pkgs.haskell.lib.dontCheck (self.callCabal2nix "polysemy" polysemySrc {});
    hspec-snap     = pkgs.haskell.lib.dontCheck (self.callHackage "hspec-snap" "1.0.1.0" { });

  };

  ios = {};

  # shellToolOverrides = {
  #   tasty-discover = justStaticExecutables (dontCheck (callHackageDirect {
  #       pkg = "tasty-discover";
  #       version = "4.2.2";

  #   }));
  # };

  packages = {
    servant-snap = servantSnapSrc;
    servant-reflex = servantReflexSrc;
    # reflex-jexcel = reflexJExcelSrc;
    # reflex-fileapi = reflexFileApiSrc;
    # reflex-codemirror = reflexCodeMirrorSrc;
    poker-base = deps/base;
    poker-game = deps/game;
    xploit-query = deps/xploit-query;
    poker-histories = deps/poker-histories;
    # reflex-utils = reflexUtilsSrc;
  };

  # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  # android.displayName = "Obelisk Minimal Example";
  # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  # ios.bundleName = "Obelisk Minimal Example";

})
