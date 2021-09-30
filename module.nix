{ nixosPkgs, ... }: {...}: {
  imports = [ (nixosPkgs.path + /nixos/modules/virtualisation/virtualbox-image.nix) ];
}
