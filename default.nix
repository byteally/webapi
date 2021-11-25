(import ./reflex-platform.nix).project ({ pkgs, ... }: {
  packages = {
    webapi                   = ./webapi;
    webapi-contract          = ./webapi-contract;
    webapi-client-reflex-dom = ./webapi-client-reflex-dom;
    webapi-swagger           = ./webapi-swagger;
    webapi-docs              = ./webapi-docs;
    bytestring-trie   = ((pkgs.fetchFromGitHub {
     owner = "capital-match";
     repo = "bytestring-trie";
     rev = "47526b2ec810239fe824c03c13cf1d81f0741b5c";
     sha256 = "1m4ywdh2wh0l8f7w7q7k4p0icsx5slcpjgnv3biylz1yvzb1y42q";
    }));

  };

  overrides = self : super : {
    bytestring-lexing = pkgs.haskell.lib.dontCheck super.bytestring-lexing;
    http-media        = pkgs.haskell.lib.dontCheck super.http-media;
    Glob              = pkgs.haskell.lib.dontCheck super.Glob;
    multiset          = pkgs.haskell.lib.dontCheck super.multiset;
  };

  shells = {
    ghc   = ["webapi"
	     "webapi-contract"
	     "webapi-client-reflex-dom"
	     "webapi-swagger"
	     "webapi-docs"
            ];
    ghcjs = ["webapi-contract"
             "webapi-client-reflex-dom"
            ];
  };

})
