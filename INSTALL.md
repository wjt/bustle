Building from source
====================

First, make sure the Haskell Platform is installed, preferably along with the
Gtk+ bindings for Haskell, and some other dependencies. On Debian-flavoured
systems, well, actually just `apt-get build-dep bustle`, but:

    sudo apt-get install \
        pkg-config \
        libdbus-1-dev \
        libglib2.0-dev \
        libpcap0.8-dev \
        haskell-platform \
        libghc-mtl-dev \
        libghc-cairo-dev \
        libghc-gtk-dev \
        libghc-parsec3-dev \
        libghc-glade-dev \
        libghc-dbus-dev \
        libghc-pcap-dev \
        help2man

(If you can't get the Haskell Platform via your package manager, see
<http://hackage.haskell.org/platform/>. If you can't get the Gtk+ binding for
Haskell via your package manager, you'll need to run:

    cabal install gtk2hs-buildtools

and ensure that ~/.cabal/bin is in your PATH before continuing.)

Got that? Great!

    export PREFIX=/opt/bustle

    # Build and install Bustle itself.
    cabal install --prefix=$PREFIX

    # Build and install the stand-alone logger binary, plus the icons, desktop
    # file, etc. etc.
    make install PREFIX=$PREFIX

If the Haskell Platform is not available on the platform you want to do
some D-Bus profiling on, that's fine: the logger is written in C, and
you can view logs generated on your fancy embedded hardware on your more
pedestrian Linux laptop. The logger only depends on a few widely-available
libraries:

    sudo apt-get install libglib2.0-dev libpcap-dev
