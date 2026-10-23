{
  writeShellApplication,
  fetchurl,
  whisper-cpp,
  pipewire,
  xclip,
  libnotify,
}: let
  model = fetchurl {
    url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-large-v3-turbo-q5_0.bin";
    hash = "sha256-OUIhcJzVrR9AxG5gMcphvOiJMebgiMGIKUxtWlX/p+I=";
  };
in
  writeShellApplication {
    name = "dictate";
    runtimeInputs = [(whisper-cpp.override {vulkanSupport = true;}) pipewire xclip libnotify];
    runtimeEnv.DICTATE_MODEL = model;
    text = builtins.readFile ./dictate.sh;
  }
