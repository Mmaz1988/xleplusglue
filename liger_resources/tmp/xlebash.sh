#!/bin/bash
export XLEPATH=/xle
export PATH=${XLEPATH}/bin:$PATH
export DYLD_LIBRARY_PATH=$XLEPATH/lib:$XLEPATH/bin/sp-3.12.7
export LD_LIBRARY_PATH=${XLEPATH}/lib
export LD_LIBRARY_PATH=${XLEPATH}/lib:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=${XLEPATH}/lib:$DYLD_LIBRARY_PATH

export TCL_LIBRARY=${XLEPATH}/tcl/scripts/tcl
export TCLLIBPATH=${XLEPATH}/tcl/scripts/tcl
export TKLIBPATH=${XLEPATH}/tcl/scripts/tk
export TK_LIBRARY=${XLEPATH}/tcl/scripts/tk

xle -noTk -e "set timeout 5; create-parser ./grammars/dev/glue-basic-drt.lfg; parse-testfile /Users/princess_zelda/IdeaProjects/xleplusglue/liger_resources/tmp/testfile.lfg -outputPrefix /Users/princess_zelda/IdeaProjects/xleplusglue/liger_resources/tmp/parser_output/sentence; exit"
