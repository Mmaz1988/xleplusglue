# XLE+Glue toolset

## Intro

This repository serves to provide a toolset for working with XLE+Glue. Furthermore, it provides the means to create a Docker container for running the components in a virtual environment.
This repository contains:
- XLE+Glue: A simple user interface for using the Glue Semantics Workbench with XLE
- The latest version of the Glue Semantics Workbench
- The latest version of LiGER (Linguistic Graph Expansion and Rewriting) 
- XLE+Glue_web: A browser-based user interface for the GSWB and LiGER
- Sample Grammars for XLE+Glue
- A docker compose file for running the system on a local docker container

## Requirements

- Access to the XLE binaries (distributed via the **[University of Konstanz](https://ling.sprachwiss.uni-konstanz.de/pages/xle/index.html)**, requires signing a license with PARC)
- **[Docker](https://www.docker.com/)**

### For running local version:

- Java (jars are compiled at version 11; available at **[OpenJDK](https://jdk.java.net/13/)**
- **[SWI-Prolog](https://www.swi-prolog.org/) (version > v6.xx)**

Due to XLE's architecture, the Docker-based system cannot be run successfully on Apple machines with ARM-based chips like the M1 and M2. It should run on machines that support x86 on any operating system.

## Installation -- XLE+Glue_web

In the folder ./liger_resources create a file named xle_paths.txt with the following contents:

```
XLE="/bin/xle"
grammar="/grammars/glue-basic-drt-tense.lfg.glue"
OS="LINUX"
```
Here, you also need to specify the grammar you intend to use before building the system. The grammar should be stored within the grammar folder in the repository. A system for uploading grammars and changing the grammar during use is in preparation.

For the system to work properly, you need access to the XLE binaries for Linux. These need to be stored in a folder called _xle_ in the root directory of this repository. The Dockerfile-liger specifies where to copy the XLE binaries from if you want to store them elsewhere.

## Running the system 

The web interface is hosted on _http://localhost:80_ 
The system has been tested with Firefox and Chrome. 

For further details, a documentation document will be made available soon. 







