# This file defines overlays
{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs final.pkgs;

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });

    conky = prev.conky.overrideAttrs (oldAttrs: {
      # https://github.com/brndnmtthws/conky/issues/2131
      # version = "1.22.0";
      version = "1.21.9";
      src = prev.fetchFromGitHub {
        owner = "brndnmtthws";
        repo = "conky";
        rev = "v1.21.9";
        sha256 = "sha256-iGUWeEKNDsTrEenQF5IuzVQhkQcKDBYCvdBgM0BnHPI=";
      };
      buildInputs = oldAttrs.buildInputs ++ [prev.pkgs.gperf];
    });

    devenv = inputs.devenv.packages.${final.system}.default;

    # NOTE: workaround for HP returning a 403 when downloading
    # the hplip plugin via curl (headless). Downloaded manually
    # for now. KEEPING THIS FOR IF IT HAPPENS AGAIN.
    # hplipWithPlugin = prev.hplip.overrideAttrs (oldAttrs: {
    #   withPlugin = true;
    #   plugin = ../hotfix/hp/hplip-3.24.4-plugin.run;
    # });
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };

  nur = inputs.nur.overlays.default;
}
