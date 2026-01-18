{
  description = "NixOS configuration for kanta (t14-gen1)";

  nixConfig = {
    extra-substituters = [
      "https://install.determinate.systems"
    ];
    extra-trusted-public-keys = [
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
    ];
  };

  inputs = {
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    most-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    devenv.url = "github:cachix/devenv/main";
    ghostty.url = "github:ghostty-org/ghostty/main";
    opencode.url = "github:anomalyco/opencode/dev";

    # Home manager
    # change back to release-25.05 when this is fixed: https://github.com/nix-community/home-manager/pull/7472
    # or reverted
    home-manager.url = "github:nix-community/home-manager/c26266790678863cce8e7460fdbf0d80991b1906";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    determinate,
    ...
  } @ inputs: let
    inherit (self) outputs;
    # Supported systems for your flake packages, shell, etc.
    systems = [
      # "aarch64-linux"
      # "i686-linux"
      "x86_64-linux"
      # "aarch64-darwin"
      # "x86_64-darwin"
    ];
    # This is a function that generates an attribute by calling a function you
    # pass to it, with each system as an argument
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in {
    # Your custom packages
    # Accessible through 'nix build', 'nix shell', etc
    packages = forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});
    # Your custom packages and modifications, exported as overlays
    overlays = import ./overlays {inherit inputs;};
    # Reusable nixos modules you might want to export
    # These are usually stuff you would upstream into nixpkgs
    nixosModules = import ./modules/nixos;
    # Reusable home-manager modules you might want to export
    # These are usually stuff you would upstream into home-manager
    homeManagerModules = import ./modules/home-manager;

    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations.kanta = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs outputs;};
      modules = [
        determinate.nixosModules.default
        ./nixos/kanta/configuration.nix
      ];
    };

    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager --flake .#your-username@your-hostname'
    homeConfigurations = {
      # FIXME replace with your username@hostname
      "deni@kanta" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
        extraSpecialArgs = {inherit inputs outputs;};
        # > Our main home-manager configuration file <
        modules = [./home/home.nix];
      };
    };
  };
}
