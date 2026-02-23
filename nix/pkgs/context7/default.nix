{
  pkgs,
  lib,
  ...
}:
pkgs.buildNpmPackage rec {
  pname = "context7-mcp";
  version = "2.1.0";

  src = pkgs.fetchurl {
    url = "https://registry.npmjs.org/@upstash/context7-mcp/-/context7-mcp-${version}.tgz";
    hash = "sha256-U4WZfB1hOlXMVbvqvaccNKKQzJX8SOEfK4ZZEvAOSOU=";
  };

  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-1AByUEGFZLpdQ3tHhSOeB27SytHfWf1G8QmS31p3kuE=";

  dontNpmBuild = true;

  meta = with lib; {
    description = "MCP server for Context7 - up-to-date documentation for AI coding";
    homepage = "https://github.com/upstash/context7";
    license = licenses.mit;
    mainProgram = "context7-mcp";
  };
}
