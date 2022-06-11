Installation for Debian based Linux released
============================================

The 'seer_deb.sh' script in this directory will grab the latest stable Seer release
and will compile and install it. This script works for Debian based Linux releases
(Debian, Ubuntu, etc...).

Use this script if you don't want to run the build commands manually yourself.

Grab this script from the Seer release on github. There's no need to grab the
entire Seer release as the script will do that.

```
    % cd /tmp
    % wget https://github.com/epasveer/seer/blob/main/debian/seer_deb.sh
```

Then run the script, giving it the name and path of a build directory. This build
directory is where the Seer source is installed to and compiled.

```
    % cd /tmp
    % mkdir seer_build
    % seer_deb.sh seer_build
```

Seer will be installed into your local system in the standard Debian location. As
well, other standard Debian package details are setup.

Credits
=======

Thanks to H. Turgut Uyar (https://github.com/uyar) for setting this up for me!

