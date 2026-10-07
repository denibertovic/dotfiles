{
  lib,
  python3Packages,
  fetchFromGitHub,
}:
python3Packages.buildPythonApplication rec {
  pname = "google-workspace-mcp";
  version = "1.29.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "taylorwilsdon";
    repo = "google_workspace_mcp";
    tag = "v${version}";
    hash = "sha256-A0U60bkmbhn/UjQ18p3ClDsoA+ckK+9eqktglMa5dGU=";
  };

  build-system = with python3Packages; [
    setuptools
    wheel
  ];

  dependencies = with python3Packages; [
    cryptography
    defusedxml
    email-validator
    fastapi
    fastmcp
    google-api-python-client
    google-auth-httplib2
    google-auth-oauthlib
    httpx
    markdown-it-py
    py-key-value-aio
    pyjwt
    pypdf
    python-dotenv
    pytz
    pyyaml
    tzdata
    urllib3
  ];

  # The test suite needs network access and Google credentials.
  doCheck = false;

  pythonImportsCheck = [
    "core.server"
    "auth.oauth_config"
    "gmail"
    "gcalendar"
    "gdrive"
  ];

  meta = {
    description = "Google Workspace MCP server for Calendar, Gmail, Docs, Sheets, Slides and Drive";
    homepage = "https://github.com/taylorwilsdon/google_workspace_mcp";
    changelog = "https://github.com/taylorwilsdon/google_workspace_mcp/releases/tag/v${version}";
    license = lib.licenses.mit;
    mainProgram = "workspace-mcp";
  };
}
