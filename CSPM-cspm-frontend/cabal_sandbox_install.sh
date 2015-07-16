#!/bin/bash

# a hack for (re-)installing hashable on MacOS X 10.6
ForceInstallHashable() {
	OS=$(uname)
	echo "operation system: $OS"
	if [ "$OS" = "Darwin" ]
	then
		#statements
		version=$(sw_vers | grep '10.6')
		if [ -n "$version" ]
		then
			#statements
			echo "force reinstalling hashable"
			cabal install --reinstall --force-reinstall hashable profunctors
		fi
	fi
}

# cabal sandbox delete                       # Built-in command
rm -rf .cabal-sandbox cabal.sandbox.config # uninstalling sandbox

# initialise sandbox
cabal sandbox init

#add sources to local repository
cabal sandbox add-source ../CSPM-Frontend
cabal sandbox add-source ../CSPM-ToProlog

# install only dependencies
cabal install --only-dependencies

# re-intsall hashable and profunctors on MacOS X 10.6
ForceInstallHashable

#build cspmf-tool
cabal build