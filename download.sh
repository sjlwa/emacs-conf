#!/usr/bin/sh

. ./info.sh
FILE=emacs-$VERSION.tar.xz
DOWNLOAD_PATH=/tmp/$FILE
EXTRACT_DIR=/tmp/emacs-$VERSION

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
		wget -v -O $DOWNLOAD_PATH https://ftpmirror.gnu.org/emacs/$FILE
	fi
}

function emacs_extract() {
	if [ -d $EXTRACT_DIR ]; then
		echo "$EXTRACT_DIR directory already exists. skipping extraction."
		return 0
	fi

	mkdir $EXTRACT_DIR
	tar xvf $DOWNLOAD_PATH -C /tmp
}

function emacs_install() {
	cd $EXTRACT_DIR
	sudo pacman -S --needed libgccjit && \
	./configure && make -j16 && sudo make install && \
    echo "#include \"$HOME/dev/emacs-conf/.Xresources\"" >> ~/.Xresources && xrdb ~/.Xresources
}

function emacs_clean() {
	emacs --version && rm $DOWNLOAD_PATH && rm -rf $EXTRACT_DIR
}
