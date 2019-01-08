# EjStand

[![github release](https://img.shields.io/github/release/apathism/ejstand.svg)](https://github.com/apathism/ejstand/releases)
[![license](https://img.shields.io/github/license/apathism/ejstand.svg)](https://www.gnu.org/licenses/agpl-3.0.en.html)

EjStand is a simple and configurable web standings daemon for ejudge contest
management system. It's intended to be used in situations when standing contains
results from different contests or when standing needs some additional display
options not supported by ejudge.

## Getting started

### Installing

EjStand is written in Haskell and can be easily built with
[Stack tool](https://www.haskellstack.org/).

```bash
cd ejstand
stack build
```

If you wish to install EjStand locally (to `~/.local/bin`), you can use 
```bash
stack install
```

As for recommended option, you should make a package for your Linux
distribution on your own. As an example there is ArchLinux sample package
source in `dist` directory.

### Configuration

EjStand has two different types of configuration files: _global_ and _local_.

Global configuration file is unique and contains options which are applied for
all standings served by EjStand. For example, two of the most important options
in that file are hostname/IP and port to bind an application web server.

Local configuration files define settings for different standing tables and
therefore aren't unique.

Examples with detailed descriptions for each option are presented in the
`conf` directory. You can use these examples as templates for your own EjStand
instance.

Personally i'd start my own EjStand configuration with
```bash
sudo install -Dm644 conf/global.cfg.example /etc/ejstand.cfg
"${EDITOR}" /etc/ejstand.cfg
```

### Setting Up WebServer

Most of the time it makes sense to set up EjStand next to ejudge, and that means
that you're probably want some kind of directory (for example, /ejstand) on
your Apache/nginx/whatever-you-use-webserver to be proxied to EjStand.

This [article](https://www.nginx.com/resources/wiki/start/topics/examples/likeapache/)
can help you to setup your global server in a proper way.

## Bugs & Issues

You must be kiddin'. There are no bugs in this software. But if you think there
are, you can write your issue [here](https://github.com/apathism/ejstand/issues).
Please describe all steps to reproduce your bug, your EjStand version and ejudge
files which are parsed (if possible).

## Contributing

There are no strict policies about contributing and all your patches are
welcomed. Your pull requests must meet only three requirements:

1. the patch is sane and doesn't break any existing features;
2. you agree with project license, and allow to distribute your code under its
   conditions;
3. the code is well-formatted with `utils/restyle_code.sh` script or
   [Brittany](https://github.com/lspitzner/brittany) with the same options.

## License

This project is licensed under the
[GNU AGPL license](https://www.gnu.org/licenses/agpl-3.0.en.html) version 3.
See LICENSE file for more details.

EjStand uses a lot of third-party libraries licensed under permissive open
source licenses. You can review third-party licenses in `third-party/licenses`
directory.