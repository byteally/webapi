let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
     reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "1e3e666d22035c395e417d302754ba28d3357b20";
      sha256 = "06c6rh9r144i2dyb3ij6vkvpxs9zalf0s2ppgf28x94dwbcrpi35";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
