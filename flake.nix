{
  description = "Indigo smart contracts";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.plutus.url = "github:input-output-hk/plutus"; # used for libsodium-vrf

  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs = { self, nixpkgs, haskell-nix, plutus, flake-compat }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          fakeSrc = pkgs.runCommand "real-source" { } ''
            cp -rT ${self} $out
            chmod u+w $out/cabal.project
            cat $out/cabal-haskell.nix.project >> $out/cabal.project
          '';
        in (nixpkgsFor system).haskell-nix.cabalProject' {
          src = fakeSrc.outPath;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          modules = [{
            packages = {
              plutus-ledger.doHaddock = false;
              plutus-contract.doHaddock = false;
              plutus-use-cases.doHaddock = false;

              marlowe.flags.defer-plugin-errors = deferPluginErrors;
              plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
              plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
              plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

              cardano-crypto-praos.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
            };
          }];
          shell = {
            withHoogle = false;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.haskellPackages.fourmolu
              pkgs.entr
              pkgs.nixfmt
              pkgs.haskellPackages.cabal-fmt
              pkgs.bashInteractive
            ];

            tools = {
              hlint = "3.3.6";
              haskell-language-server = "1.7.0.0";
            };

            additional = ps: [
              ps.base-deriving-via
              ps.cardano-addresses
              # ps.cardano-addresses-cli
              # ps.cardano-binary
              ps.cardano-crypto
              ps.cardano-crypto-class
              ps.cardano-crypto-praos
              ps.cardano-crypto-wrapper
              ps.cardano-ledger-alonzo
              ps.cardano-ledger-byron
              ps.cardano-ledger-core
              ps.cardano-ledger-pretty
              ps.cardano-ledger-shelley
              ps.cardano-ledger-shelley-ma
              ps.cardano-prelude
              ps.cardano-slotting
              ps.flat
              ps.freer-extras
              ps.goblins
              ps.measures
              ps.orphans-deriving-via
              ps.playground-common
              ps.plutus-contract
              ps.plutus-core
              ps.plutus-ledger
              ps.plutus-ledger-api
              ps.plutus-pab
              ps.plutus-playground-server
              ps.plutus-tx
              ps.plutus-tx-plugin
              ps.plutus-use-cases
              ps.prettyprinter-configurable
              ps.quickcheck-dynamic
              ps.Win32-network
              ps.word-array
              ps.plutus-pab-executables
              ps.plutus-script-utils
              ps.plutus-simple-model
              ps.plutus-tx-spooky
              ps.plutonomy
            ];
          };
          sha256map = {
            "https://github.com/input-output-hk/plutus-apps"."795718b099572c09f4ecee1f1b6f5f6cb66c120c" =
              "1h6rq3zwysl5v698k2fmqqd2q441441gvbcjs8q4m49pf3hl1k28";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."066f7002aac5a0efc20e49643fea45454f226caa" =
              "0s6x4in11k5ba7nl7la896g28sznf9185xlqg9c604jqz58vj9nj";
            "https://github.com/IndigoProtocol/plutus"."f003b096dbc5abca9dbb74fbff847914dd39fa6b" =
              "1aiqnpbfr0ik6q7llhrsl405ib0yphix0j55w8mx354r1h4wmlss";
            "https://github.com/input-output-hk/ekg-forward"."297cd9db5074339a2fb2e5ae7d0780debb670c63" =
              "1zcwry3y5rmd9lgxy89wsb3k4kpffqji35dc7ghzbz603y1gy24g";
            "https://github.com/Quid2/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8" =
              "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
            "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" =
              "10pb0yfp80jhb9ryn65a4rha2lxzsn2vlhcc6xphrrkf4x5lhzqc";
            "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00" =
              "0da1vn2l6iyfxcjk58qal1l4755v92zi6yppmjmqvxf1gacyf9px";
            "https://github.com/input-output-hk/cardano-wallet"."7bac5971aef5e3b62b19b79ec6f22bd7fa2e094d" =
              "16l5384wc1sxzri56wr6r6hpnnf9lmgrikcw9f2pyax4axggvlna";
            "https://github.com/input-output-hk/cardano-node"."1.35.1" =
              "1z0zv1i58ikmbqg878f9z573jkwp4lzhmmswshm6c96rq6lprzh8";
            "https://github.com/input-output-hk/cardano-config"."1646e9167fab36c0bff82317743b96efa2d3adaa" =
              "11kf65x38laqhwspsl28j2x5a4rky8mfr6356w0li5g53sfykmjc";
            "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" =
              "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
            "https://github.com/input-output-hk/hedgehog-extras"."967d79533c21e33387d0227a5f6cc185203fe658" =
              "0rbqb7a64aya1qizlr3im06hdydg9zr6sl3i8bvqqlf7kpa647sd";
            "https://github.com/input-output-hk/cardano-ledger"."3be8a19083fc13d9261b1640e27dd389b51bb08e" =
              "0dvm9l43mp1i34bcywmznd0660hhcfxwgawypk9q1hjkml1i41z3";
            "https://github.com/input-output-hk/ouroboros-network"."a65c29b6a85e90d430c7f58d362b7eb097fd4949" =
              "1fmab5hmi1y8lss97xh6hhikmyhsx9x31yhvg6zpr2kcq7kc6qkf";
            "https://github.com/input-output-hk/io-sim"."57e888b1894829056cb00b7b5785fdf6a74c3271" =
              "1kv8lwmrw1c0g03jy3i3fgk3c8d47ihjcslg295djqj442y95y2f";
            "https://github.com/input-output-hk/typed-protocols"."181601bc3d9e9d21a671ce01e0b481348b3ca104" =
              "1lr97b2z7l0rpsmmz92rsv27qzd5vavz10cf7n25svya4kkiysp5";
            "https://github.com/input-output-hk/cardano-base"."0f3a867493059e650cda69e20a5cbf1ace289a57" =
              "0p0az3sbkhb7njji8xxdrfb0yx2gc8fmrh872ffm8sfip1w29gg1";
            "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" =
              "00h10l5mmiza9819p9v5q5749nb9pzgi20vpzpy1d34zmh6gf1cj";
            "https://github.com/input-output-hk/cardano-crypto"."f73079303f663e028288f9f4a9e08bcca39a923e" =
              "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
            "https://github.com/input-output-hk/cardano-addresses"."b6f2f3cef01a399376064194fd96711a5bdba4a7" =
              "10yj47gay72kx6v564qlfiigggcpqfdzrg61ii8p25m5n8ijz045";
            "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" =
              "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
            "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" =
              "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
            "https://github.com/vshabanov/ekg-json"."00ebe7211c981686e65730b7144fbf5350462608" =
              "1zvjm3pb38w0ijig5wk5mdkzcszpmlp5d4zxvks2jk1rkypi8gsm";
            "https://github.com/haskell-works/hw-aeson"."d99d2f3e39a287607418ae605b132a3deb2b753f" =
              "1vxqcwjg9q37wbwi27y9ba5163lzfz51f1swbi0rp681yg63zvn4";
            "https://github.com/raduom/hysterical-screams"."4c523469e9efd3f0d10d17da3304923b7b0e0674" =
              "0w118v4vffrsjxfmwfv8qcn2qxmxpd1gxwcjnda91qz09jnpg0vp";
            "https://github.com/mlabs-haskell/plutus-simple-model"."31a18e5dc28ae7620c06adfad061f06ec176346b" =
              "1dsdv59dbjyxmdhnl2srdxdpmj61k6haf5idahn4kzhajqxzv583";
            "https://github.com/input-output-hk/quickcheck-dynamic"."c272906361471d684440f76c297e29ab760f6a1e" =
              "1b9ppgavqad78a2z1zxv7v4jasjz6zz0mxkr0zx0bbcd0i00jajf";
          };
        };
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out");
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          devShell = self.flake.${system}.devShell;
        # required to avoid "argument list too long" error
        # goes from ~1900 buildInputs to ~300
        in pkgs.lib.overrideDerivation (devShell) (oldAttrs: {
          buildInputs = pkgs.lib.lists.unique devShell.buildInputs;
        }));
    };
}
