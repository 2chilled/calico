let pkgs = import <nixpkgs>{};
    myNeovim =
      let pythonDeps = x: with x; [websocket_client sexpdata greenlet ];
      in pkgs.neovim.override {
        extraPythonPackages = pythonDeps;
        extraPython3Packages = pythonDeps;
      };
    mySbt = pkgs.sbt.override {jre = pkgs.jdk11;};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    mySbt
    jdk11
    myNeovim
    coursier
    #ammonite
    scalafmt
    bloop
  ];
}
