## Compilation

```sh
ghc -o Buildfile Buildfile.hs
```

## Usage

```sh
# Create symlink in directory with Dockerfile
# name of symlink will be used as a name for container
# and part of tag
ln -s /path/to/Buildfile ./my-container

# Chain commands
./my-container build push start shell
```

## Additional configuration

You can use .docker-{ports, volumes, env, links, hosts} files to store docker options.

Example .docker-ports file:

```text
-p 80:3000
-p 22:2222
```

Key -p is necessary.
