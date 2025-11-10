# XLE+Glue toolset

This is the main branch of XLE+Glue. It serves as a landing page for people interested in the tool set. However, specific uses of the tool set are developed in different branches of this repository. Thus, choosing the right branch can lead to a leaner experience in working with XLE+Glue. 

# Branches

The three following branches provide access to three main variants of XLE+Glue. 

 - [XLE+Glue with Inference](https://github.com/Mmaz1988/xleplusglue/tree/2024_inference)
 - [UD+Glue](https://github.com/Mmaz1988/xleplusglue/tree/2025_udplusglue)
 - [Local XLE+Glue](https://github.com/Mmaz1988/xleplusglue/tree/2025_xlepluglue_local)

The _XLE+Glue with Inference_ branch corresponds to the main branch of this repository providing the most comprehensive demonstration of the XLE+Glue toolset including inference. The _UD+Glue_ branch substitutes the XLE for Stanza's Universal Dependency parser which is couched in a Docker container. Thus, no extra installation is needed. The _Local XLE+Glue_ is a lean version of XLE+Glue that ommits the reasoning component (adding support for this is a future goal), and the Docker architecture including the Web interface. It is designed to allow users to use XLE+Glue within the original user interface of the XLE. 

The corresponding branches contain sub-branches with demos for specific events/papers. From these, substantial updates are incorporated into the main braches regularly.


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

- Java (jars are compiled at version 17; available at **[OpenJDK](https://jdk.java.net/13/)**
- **[SWI-Prolog](https://www.swi-prolog.org/) (version > v6.xx)**

Due to XLE's and Vampire's architecture, the Docker-based XLE interface is a bit slow on ARM machines.

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

The system can be started from the command line by navigating to the _./Docker_ folder within the repository. There you need to execute the following command:

```
docker compose up --build
```
This will start the system and provide access to the browser-based user interface. It is hosted on _http://localhost:80_ 
The system has been tested with Firefox and Chrome. 

The system can be shut down by using the keyboard command _ctrl+c_ in the command line where the docker container is running.

For further details, a documentation document will be made available soon. 







