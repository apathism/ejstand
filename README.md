# ejstand

ejstand is a simple and configurable web standings daemon for ejudge contest
management system. Its intended to use in situations when standing contains
results from different contests or when standing needs some additional display
options not supported by ejudge.

## Getting started

### Installing

ejstand is written in Haskell programming language and can be easily
built with [Stack tool](https://www.haskellstack.org/).

```bash
cd ejstand
stack build
```

If you wish to install ejstand locally (to `~/.local/bin`), you can use 
```bash
stack install
```

As for recommended option, you should make a package for your Linux
distribution on your own. As an example there is ArchLinux sample package
source in `dist` directory.

### Configuration

ejstand has two different types of configuration files: _global_ and _local_.

Global configuration file is unique and contains options which are applied for
all standings served by ejstand. For example, two of the most important options
in that file is hostname/IP and port to bind an application webserver.

Local configuration files define settings for different standing tables and
therefore aren't unique.

Examples with detailed descriptions for each option are presented in the
`conf` directory. You can use this examples as templates for your own ejstand
instance.

Personally i'd start my own ejstand configuration with
```bash
sudo install -Dm644 conf/global.cfg.example /etc/ejstand.cfg
"${EDITOR}" /etc/ejstand.cfg
```

### Setting Up WebServer

Most of the time it makes sense to set up ejstand next to ejudge, and that means
that you're probably want some kind of directory (for example, /ejstand) on
your Apache/nginx/whatever-you-use to be proxied to ejstand.

This [article](https://www.nginx.com/resources/wiki/start/topics/examples/likeapache/)
can help you to setup your global server in a proper way.

## Bugs & Issues

You must be kiddin'. There are no bugs in this software. But if you think there
are, you can write your issue [here](https://apathism.net/git/apathism/ejstand/issues),
and maybe I'll review it someday. Or you can just fix it yourself and send me a pull
request. Speaking of which...

## Contributing

There is no strict policies about contributing. The only two conditions of merging
code upstream are:
1. I like your patch (or at least it's not awful, so I can tweak some parts of
   it);
2. you agree with project license, and allow to distribute your code under its
   conditions.

## License

This project is licensed under the
[GNU AGPL license](https://www.gnu.org/licenses/agpl-3.0.en.html) version 3.
See LICENSE file for more details.

If you want to use this program under other license, you can contact
[me](https://apathism.net/contacts) personally, and we'll discuss conditions
of licensing.

## Authors

Currently, only one person :)

* Ivan Koryabkin