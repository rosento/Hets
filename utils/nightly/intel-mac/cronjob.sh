#!/bin/bash -x

export PATH=/home/pub-bkb/bin:/bin:/usr/bin:/opt/local/bin
export UDG_HOME=/home/mac-bkb/uDrawGraph-3.1
export HETS_LIB=/Users/Shared/maeder/haskell/Hets-lib

cd /Users/Shared/maeder/haskell

. ../cronjob.sh

makeHets
makeLibCheck

strip hets
installHetsBinary intel-mac
chgrp macbkb hets
\cp -fp hets /home/mac-bkb/intel/hets/hets-latest/bin/

createLogFiles
runIsaBasic
runSPASSBasic
runDarwinBasic

checkEnvs
checkPrfs
