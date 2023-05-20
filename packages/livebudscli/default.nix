{
  lib,
  rustPlatform,
  fetchFromGitHub,
  installShellFiles,
  pkg-config,
  bluez,
  dbus,
  libpulseaudio,
}:
rustPlatform.buildRustPackage rec {
  pname = "livebudscli";
  version = "0.1.9";

  src = fetchFromGitHub {
    owner = "JojiiOfficial";
    repo = "LiveBudsCli";
    rev = "v${version}";
    hash = "sha256-A4XQiJrk4ehb6+935L2JFOeAhUJ7bdukV5mL0Jxn0sQ=";
  };

  cargoHash = "sha256-j9rVqGTUoqI7c+gypPuSiM/JqkQ7GMGPLVENi2ahk8g=";

  nativeBuildInputs = [installShellFiles pkg-config];

  buildInputs = [bluez dbus libpulseaudio];

  postInstall = ''
    installShellCompletion --cmd earbuds \
      --bash <($out/bin/earbuds --generate bash) \
      --fish <($out/bin/earbuds --generate fish) \
      --zsh <($out/bin/earbuds --generate zsh)
  '';

  meta = with lib; {
    description = "A tool to control your Galaxy buds+, live and pro (beta) from linux";
    homepage = "https://github.com/JojiiOfficial/LiveBudsCli";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [Icy-Thought];
  };
}
