#    Copyright (C) 2019–2020 Mark-Matthias Zymla
#
#    This file is part of XLE+Glue (https://github.com/Mmaz1988/xle-glueworkbench-interface).
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

####################################################################
# Set up Glue menu
####################################################################

proc init-glue {{compiled 0}} {
    global defaultGlueParser semParser transferDebug
    #global defaultPrologFiles

    if {[string match "*.glue" $defaultGlueParser]} {

	puts "Detected glue grammar $defaultGlueParser"
	
	#Set path of gswb
	set ligerpath "jars/liger.jar"
	set command "-glue2lfg $defaultGlueParser"

	#Set up the command to call the GSWB in accordance with the parameters set in xlerc
	set glue2grammar exec
	lappend glue2grammar java
	lappend glue2grammar -jar
	lappend glue2grammar $ligerpath
	set glue2grammar [concat $glue2grammar $command]

	puts "Executing command $glue2grammar"

	#Evaluates the command defined above
	eval $glue2grammar

	set defaultGlueParser [string range $defaultGlueParser 0 [expr [string length $defaultGlueParser] - 6]]
	
    } else {
	puts "The string does not end with .glue"
    }
 
    #Create parser
    create-parser $defaultGlueParser
    #Add Glue menus to XLE GUI. Specified below. 
    create-glue-menus

    #COmment in when working with Sicstus-Prolog; not recommended    
    #foreach prologFile $defaultPrologFiles {
    #prolog "load_files('$prologFile')."}

    if {$semParser == 1} {
	puts "Semantic parser is active."
    }
    if {$transferDebug == 1} {
	puts "Debug mode is active." 
    }
}

####################################################################
# Set up menu items on f-structure windows 
####################################################################

#This simply adds a button to the XLE command menu in the GUI
#Calls the tcl function window-to-sem specified below
proc create-glue-menus {} {
    global fsCommands fsViews fsChartCommands fsChartViews 

    add-item-to-xle-menu \
	{command -label "Semantics" \
	     -command "window-to-sem 0 $self" \
	     -doc "Derives Glue semantics premises."} \
	fsCommands
}

    # Convert contents of window to semantics; display
    proc window-to-sem {packed window} {
	
	global semwindowtop
	global semDisplay

    set fsData [get-window-prop $window data]
   fswindow-to-premises ".semantics" $window  \
	          $semDisplay 522+$semwindowtop
}

#------------------------------------
#This function runs the pipeline from XLE to Glue representation
#It operates on an f-structure window $window
#displaywindow is simply the name of the window
#displaymode and position is for arranging the new window relative to the XLE GUI.

proc fswindow-to-premises {displaywindow window displaymode position} {
   
    global defaultTmpDir prover semParser \
    transferDebug processDRT solutionOnly \
    mcEncoding outputfont fontsize
	
    
    #For Sicstus Prolog
    #global defaultPrologFiles


    file delete -force tmp
   
    file mkdir tmp

    set gswbfile [relpath tmp/gswb_[pid].pl]	
    
    if {$displaymode == "window"} {
	set displayfile tmp/display_[pid].pl
    } else {
	set displayfile stdout
    }

    #Currently processed f-structure
    set prologfile tmp/default_[pid].pl
    #Output of the transfer component
    set outputfile tmp/transferOutput_[pid].pl

    print-fs-as-prolog $prologfile $window

    set sentence [get-sentence $prologfile]

        if {$transferDebug == 1} {
	    puts "#### Prolog F-structure: $prologfile ####"
	set fp [open $prologfile]
	set data [read $fp]

	puts -nonewline $data
	puts "+------------------------------------------+\n"
        close $fp
        } else {
	   puts "Generating $prologfile"
	}
    
    #Set path of liger
    set ligerpath "jars/liger.jar"
    set prologPath [relpath $prologfile]
    set outputPath [relpath $outputfile]
    set command "-fs2mcs $mcEncoding $prologPath $outputPath"

    
    #Set up the command to call LIGER in accordance with the parameters set in xlerc
    set fs2glue exec
    lappend fs2glue java
    lappend fs2glue -jar
    lappend fs2glue $ligerpath
    if {$prover == 3} {
    lappend fs2glue "-multi"
    }
    set fs2glue [concat $fs2glue $command]

    puts "Executing command $fs2glue"

    #Evaluates the command defined above
    eval $fs2glue
    
    if {$transferDebug == 1} {
	puts "#### Output of the Prolog Transfer System: $outputfile ####"
	set fp [open $outputfile]
	set data [read $fp]

	puts -nonewline $data
	puts "+------------------------------------------+\n"
	close $fp
    } else {
	puts "Generating $outputfile"
    }


# Set path of gswb
set gswbpath "jars/gswb.jar"

# Constructing the base command
set gswb [list java -jar $gswbpath -i $outputPath -o $gswbfile]

if {$prover == 0} {
    lappend gswb "-pr" "0"
} elseif {$prover == 1} {
    lappend gswb "-pr" "1"
} elseif {$prover == 2} {
    lappend gswb "-pr" "2"
} elseif {$prover == 3} {
    lappend gswb "-pr" "3"
}

# Add conditional arguments
if {$semParser == 1} {
    lappend gswb "-parseSem" "-inputStyle" "0"
} elseif {$semParser == 2} {
    lappend gswb "-outputStyle" "1"
}
if {$processDRT == 1} {
    lappend gswb "-drt"
}
if {$solutionOnly == 1} {
    lappend gswb "-s"
}

puts "Executing command: [join $gswb { }]"

# Running the Java application using exec not working on tcl 8.4 and lower
# XLE for Linux is compiled with tcl 8.4
#if {[catch {exec {*}$gswb} result]} {
    # This block will be executed if there was an error.
#    puts "Error while executing command: $result"
#} else {
    # This block will be executed if the command succeeded.
#    puts $result
#}

#alternative version with eval
if {[catch {eval exec $gswb} result]} {   
    puts "Error while executing command: $result"
} 


    #Run Java Glue prover; jar file relative to execution as above
   # if {$semParser == 0} {
#	eval exec java [list -jar glueSemWorkbench2.jar \
#			    -i $outputfile -o $gswbfile -s]
#    } elseif {$semParser == 1} {
#	eval exec java [list -jar glueSemWorkbench2.jar \
#			    -i $outputfile -o $gswbfile -parseSem -s]
#    } elseif {$semParser == 2} {
#	eval exec java [list -jar glueSemWorkbench2.jar \
#			    -i $outputfile -o $gswbfile -prolog -s]
    #   }

    if {$transferDebug == 1} {
	puts "#### Output of the Glue Semantics Workbench: $gswbfile ($sentence) ####"
	set fp [open $gswbfile]
	set data [read $fp]
	puts -nonewline $data
	puts "+------------------------------------------+"
	close $fp
    } else {
	puts "Generating $gswbfile"
    }

    set displayfile $gswbfile
    
    if {$displaymode == "window"} {
	display-file $displayfile $displaywindow $position "$outputfont $fontsize"
    }

    	#Delete temporary files

	if {$transferDebug == 0} {
	    file delete $prologfile
	    file delete $outputfile
	    file delete $displayfile
#	    file delete $drtOutputFile
	    file delete $prettydrtfile
	    file delete $gswbfile
	    file delete -force tmp
	    puts "Temporary files are deleted after procedure is completed."
	} else {
	    puts "Temporary files are stored in the /tmp folder."
	}

    puts "+------------------------------------------+"
    puts "Done"
}


#parse-testfile testfile.lfg -outputPrefix testdir/testdir
proc testfile-to-sem {sentenceFile outputDir} {
    global defaultparser break_test max_count
    set outputPrefix "-outputPrefix"
    if {$outputDir != ""} {
	set outputDir "testdir/testdir"
	puts "No output directory has been specified."
        puts "Output directory automatically set to 'testdir/testdir' ..." 
    }
	set dirlist [split $outputDir "/"]
	set dir [lindex $dirlist 0]
    
    
    file delete -force $dir
    file mkdir $dir

    puts "#### Output for testfile: $sentenceFile ####"

    puts "Created output directory: $dir"
    
    puts "#### Parsing testfile: $sentenceFile ####"
    
    parse-testfile $sentenceFile $outputPrefix $outputDir

    puts "+------------------------------------------+\n"
    
   puts "#### Generating semantic analysis: $sentenceFile ####"
    set debugFile $dir/sem_result.txt
    set debug [open $debugFile "w"]
    
    set parses  [lsort -dictionary [glob -directory $dir -- "*.pl"]]
    foreach parse $parses {

	set sentence [get-sentence $parse]
	
	fsdata-to-premises $parse "$parse-sem" "$parse-premises"
	set sem [open "$parse-sem" "r"]
	set semData [read $sem]
	puts $debug "GSWB result for $parse ($sentence):"
	puts $debug $semData
	close $sem
    }
    close $debug
    puts "Done"
}

proc get-sentence {fstr} {
set sentence ""    
set f [open $fstr]
while {[gets $f line] != -1} {
    if {[regexp {'markup_free_sentence'\('(.*)'\)} $line all value]} {
	set sentence $value
	break
    }
}
close $f
return $sentence
}




