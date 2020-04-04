{ dockerTools, smoke }:

dockerTools.buildLayeredImage {
  name = "smoke";
  config = {
    Env = [ "ENV=en_US.utf8" ];
    Entrypoint = [ "${smoke}/bin/smoke" "$*" ];
  };
}
