# XLE+Glue

This is the main branch of XLE+Glue. It serves as a landing page for people interested in the tool set. However, specific uses of the tool set are developed in different branches of this repository. Thus, choosing the right branch can lead to a more focused experience in working with XLE+Glue. 

# Branches

The three following branches provide access to three main variants of XLE+Glue. 

 - [XLE+Glue with Inference](https://github.com/Mmaz1988/xleplusglue/tree/lfg2026_pragmatic_parsing)
 - [Local XLE+Glue](https://github.com/Mmaz1988/xleplusglue/tree/2025_xlepluglue_local)
 - [UD+Glue](https://github.com/Mmaz1988/xleplusglue/tree/2025_udplusglue)

The _XLE+Glue with Inference_ branch corresponds to the main branch of this repository, providing the most comprehensive demonstration of the XLE+Glue toolset, including the web interface and inference.  The _Local XLE+Glue_ is a more compact version of XLE+Glue that omits the reasoning component (adding support for this is a future goal), and the Docker architecture, including the Web interface. It is designed to allow users to use XLE+Glue within the original user interface of the XLE. 

The _UD+Glue_ branch is special in that it does not use XLE. It replaces the XLE with Stanza's Universal Dependency parser, which is packaged in a Docker container. Thus, no extra installation is needed. However, as the system still shares many properties with XLE+Glue, we also mention it here. For more information, see the corresponding branch. 

The main branches mentioned here may contain sub-branches with demos for specific events/papers. From these, substantial updates are incorporated into the main branches regularly.


## Intro

This repository provides a toolset for working with XLE+Glue. Furthermore, it provides the means to create a Docker container for running the components in a virtual environment.
This repository contains:
- XLE+Glue: A simple user interface for using the Glue Semantics Workbench with XLE
- The latest version of the [Glue Semantics Workbench](https://github.com/Mmaz1988/GlueSemWorkbench_v2) (a set of theorem provers)
- The latest version of [LiGER](https://github.com/Mmaz1988/liger) (Linguistic Graph Expansion and Rewriting) 
- [XLE+Glue_web](https://github.com/Mmaz1988/xleplusglue-client): A browser-based user interface for the GSWB and LiGER
- Sample Grammars for XLE+Glue (There are two encoding styles for grammars, an avm-based encoding and a "literal" encoding)
- An interface to the [Vampire](https://vprover.github.io/) theorem prover that is integrated in the web interface
- A docker compose file for running the system on a local Docker container

## Requirements

- Access to the XLE binaries (distributed via the **[University of Konstanz](https://ling.sprachwiss.uni-konstanz.de/pages/xle/index.html)**, requires signing a license with PARC)
- **[Docker](https://www.docker.com/)**

### For running local version

- Java (jars are compiled at version 17; available at **[OpenJDK](https://jdk.java.net/13/)**
- **[SWI-Prolog](https://www.swi-prolog.org/) (version > v6.xx)**

## Installation -- XLE+Glue_web

In the folder ./liger_resources create a file named xle_paths.txt with the following contents:

```
XLE="/bin/xle"
grammar="/grammars/glue-basic-drt-tense.lfg.glue"
OS="LINUX"
```
Here, you also need to specify the grammar you intend to use before building the system. The grammar should be stored within the grammars folder in the repository. All grammars in the grammars folder are available once the application has been started. However, to upload new grammars, the system must be restarted. 

For the system to work properly, you need access to the XLE binaries for Linux. These need to be stored in a folder called _xle_ in the root directory of this repository. The Dockerfile-liger specifies where to copy the XLE binaries from if you want to store them elsewhere.

## Running the system 

The system can be started from the command line by navigating to the _./Docker_ folder within the repository. There, you need to execute the following command:

```
docker compose up --build
```
This will start the system and provide access to the browser-based user interface. It is hosted on _http://localhost:80_ 
The system has been tested with Firefox and Chrome. 

The system can be shut down by using the keyboard command _ctrl+c_ in the command line where the docker container is running.

For further details, a documentation document will be made available soon. 

## Using the local version

The local version requires users to have XLE installed on their system. It can then be used by running xle using the xlerc file provided in this repository. In the xlerc file, users can specify which grammar to use and which LiGER rule files to use, in case they want to produce a grammar that uses both co-description and description-by-analysis.  

## Recommended settings for testing grammars (to be set in the xlerc file)

We demonstrate two groups of grammars: Those using the avm-based encoding and those using the "literal" encoding. The avm-based grammars can be found in grammars-fstr-notation and the grammars using the literal notation are found in grammars-literal-notation. These grammars are stored in separate folders as they produce auxiliary files which would clutter the folder otherwise. We recommend using this approach for all literal-notation grammars.
Some branches might contain an additional demo folder containing a grammar demonstrating the capabilities of the respective branch. Here, we demonstrate the inference grammar with additional TAM rules (i.e., a hybrid grammar).

F-structure encoding

| Grammar                  | Prover | semParser | processDRT | mcEncoding | transfer |
|--------------------------|--------|-----------|------------|------------|----------|
| glue-basic               | 0      | 0         | 0          | 0          | 0        |
| glue-basic-semparser     | 0      | 1         | 0          | 0          | 0        |
| glue-basic-semstr        | 0      | 0         | 0          | 0          | 0        |
| glue-basic-flat-encoding | 0      | 0         | 0          | 0          | 0        |

Literal encoding


| Grammar                  | Prover | semParser | processDRT | mcEncoding | transfer |
|--------------------------|--------|-----------|------------|------------|----------|
| glue-basic               | 0      | 0         | 0          | 1          | 0        |
| glue-basic-drt           | 0      | 2         | 1          | 1          | 0        |


Demo


| Grammar             | Prover | semParser | processDRT | mcEncoding | transfer |
|---------------------|--------|-----------|------------|------------|----------|
| main_fracas_grammar | 1      | 2         | 1          | 1          | 1        |



These are the settings intended for the different grammars. Using different settings might cause problems.
Generally, the HEPPLE prover (0) allows for linear quantification, while the LEV prover allows for more efficient solving, including the noscope flag.

## Known issues

- Due to XLE's and Vampire's architecture, the Docker-based XLE interface and the inference interface are a bit slow on ARM machines.





