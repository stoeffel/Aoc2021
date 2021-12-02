let
  # This comes from https://nixos.org/channels/
  #
  # Pick a release (e.g. nixpkgs-unstable) and open the `git-revision`
  # file. It will contain a revision hash. Copy and paste it below.
  rev = "94c989365d563238068841763ecc5f7f4b25b22c";
  # Generate the SHA256 hash for this revision's tarball.
  #
  #   $ nix-prefetch-url --unpack --type sha256 \
  #   >   https://github.com/NixOS/nixpkgs/archive/${rev-defined-above}.tar.gz
  sha256 = "11knmg207z2bw3yqvv6ixh1s1fzxl20wjcy75s95msv0jdjmvrsy";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };
in import nixpkgs { }
