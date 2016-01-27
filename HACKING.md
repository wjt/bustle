Want to get involved? Great!
============================

Grab the latest code from git:

    git clone git://anongit.freedesktop.org/bustle

and get stuck in! Please submit patches, or links to git branches, as
bugs on <https://bugs.freedesktop.org/enter_bug.cgi?product=Bustle>.

In new code, try to follow
<https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>.
The author did not follow it in the past but it seems like a good kind of
thing to aim for.

Releasing Bustle
================

* Ideally, automate the steps below
* Update version number in `bustle.cabal`
* Write news in `NEWS.md`

```sh
# Tag release, build and sign the tarballs
make maintainer-make-release

# Stick source and binaries on freedesktop.org
mkdir x.y.z
cp dist/bustle-x.y.z* x.y.z/
scp -r x.y.z annarchy.freedesktop.org:/srv/www.freedesktop.org/www/software/bustle/

# Upload source to Hackage
cabal upload

git push origin --tags master
```
