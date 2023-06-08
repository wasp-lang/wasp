To include node packages with an installation of Wasp, create a directory
in this folder and add a `build` script to the `package.json`.

On CI runs, `tools/install_packages_to_data_dir.sh` is used to compile these
projects and copy them into `data/packages`. You can also use that script
locally.
