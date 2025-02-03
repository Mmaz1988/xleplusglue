# XLE+Glue toolset

INFO: This branch contains the local version of XLE+Glue without the web interface. 

## Intro

This repository serves to provide a toolset for working with XLE+Glue. 
This repository contains:

- XLE+Glue: A simple user interface for using the Glue Semantics Workbench with XLE
- The latest version of the Glue Semantics Workbench
- The latest version of LiGER (Linguistic Graph Expansion and Rewriting)
- Sample Grammars for XLE+Glue

## Requirements

- XLE installed on your machine (distributed via the **[University of Konstanz](https://ling.sprachwiss.uni-konstanz.de/pages/xle/index.html)**, requires signing a license with PARC)
- **[Java LTS (compiled for version 11)](https://www.docker.com/)**
- **[SWI-Prolog LTS or version > v6.xx (development with version 9.04)](https://www.docker.com/)**



## Running the system 

The system is fully integrated in XLE and is accessible when starting XLE from the main directory (/xleplusglue).
It is recommended that grammars are stored in separate folders in the /grammars directory.


## Recommended settings (to be set in the xlerc file)

| Grammar          | Prover | semParser | processDRT | mcEncoding | transfer |
|------------------|--------|-----------|------------|------------|----------|
| glue-basic       | any    | 1         | 0          | 1          | 0        |
| hybrid-basic     | any    | 2         | 1          | 1          | 1        |
| multistage-basic | 3      | 1         | 0          | 1          | 0        |

These are the settings intended for the different grammars. Using different settings might cause problems. 

## Known bugs

- Output only prints: "Proof:"


