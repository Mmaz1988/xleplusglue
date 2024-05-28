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

## Installation

In the folder ./liger_resources create a file named xle_paths.txt with the following contents:

```
XLE="/bin/xle"
grammar="/grammars/glue-basic-drt-tense.lfg.glue"
OS="LINUX"
```






