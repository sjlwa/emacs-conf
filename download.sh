#!/usr/bin/sh

. ./version.sh

function emacs_get_file() {
    local version="${1:Unset}"
    local mirror="${2:Unset}"
    if [[ $mirror == "ftp" ]]; then
        echo "emacs-${version}.tar.xz"
    elif [[ $mirror == "github" ]]; then
        echo "emacs-${version}.tar.gz"
    else
        echo "Invalid mirror ${mirror}"
        return 1
    fi
}

function emacs_get_mirror_url() {
    local version="${1:Unset}"
    local mirror="${2:Unset}"
    local file=$(emacs_get_file $version $mirror)
    if [[ $mirror == "ftp" ]]; then
        echo "https://ftpmirror.gnu.org/emacs/${file}"
    elif [[ $mirror == "github" ]]; then
        echo "https://github.com/emacs-mirror/emacs/archive/refs/tags/${file}"
    else
        echo "Invalid mirror ${mirror}"
        return 1
    fi
}

DOWNLOAD_PATH="/tmp/$(emacs_get_file $VERSION $MIRROR)"
DOWNLOAD_URL="$(emacs_get_mirror_url $VERSION $MIRROR)"
EXTRACT_DIR="/tmp/emacs-$VERSION"

function emacs_download() {
	if [ -d $EXTRACT_DIR ]; then
		echo "$EXTRACT_DIR was found."
		return 0
	fi

	if [ -f $DOWNLOAD_PATH ]; then
		echo "$DOWNLOAD_PATH already exists. skipping downloading."
		return 0
	else
		echo "downloading emacs ..."
		wget -v -O $DOWNLOAD_PATH $DOWNLOAD_URL
	fi
}

function emacs_extract() {
	if [ -d $EXTRACT_DIR ]; then
		echo "$EXTRACT_DIR directory already exists. skipping extraction."
		return 0
	fi

	mkdir $EXTRACT_DIR
	tar xvf $DOWNLOAD_PATH --strip-components=1 -C "$EXTRACT_DIR"
}

function emacs_configure () {
    ./configure \
        --with-x \
        --with-x-toolkit=gtk3 \
        --without-toolkit-scroll-bars \
        --with-cairo \
        --without-xft \
        --with-harfbuzz \
        --without-libotf \
        --with-gnutls \
        --without-xdbe \
        --without-xim \
        --without-gpm \
        --disable-gc-mark-trace \
        --with-gsettings \
        --with-modules \
        --with-threads \
        --with-libgmp \
        --with-xml2 \
        --with-tree-sitter \
        --with-zlib \
        --without-included-regex \
        --with-native-compilation \
        --with-file-notification=inotify \
        --without-compress-install
}

function emacs_install() {
  echo $EXTRACT_DIR
	cd $EXTRACT_DIR
	sudo pacman -S --needed libgccjit
	./autogen.sh

  export CFLAGS="-O2 -pipe -march=native -mtune=native -fno-omit-frame-pointer -fno-plt -flto=auto"
  export LDFLAGS="-Wl,-O2 -Wl,-z,now -Wl,-z,relro -Wl,--sort-common -Wl,--as-needed -Wl,-z,pack-relative-relocs -flto=auto -O2"

  emacs_configure
  make -j "$(nproc)" -l $(nproc --ignore=1)
  sudo make install-strip && \
    echo "#include \"$HOME/dev/emacs-conf/.Xresources\"" >> ~/.Xresources && xrdb ~/.Xresources
}

function emacs_clean() {
	emacs --version && rm $DOWNLOAD_PATH && rm -rf $EXTRACT_DIR
}

"${@}"
