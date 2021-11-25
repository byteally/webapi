let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
     reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "df0bdcca5eb2a3236ec0496e4430d91876b29cf5";
      sha256 = "1ja3vkq9px8f9iyiazq44mamaahgiphi9l236pvzbl5jvhi5c4qr";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
