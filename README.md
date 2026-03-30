# UD+Glue


# Development

XLE+Glue is developed actively on two branches:

 - [XLE+Glue with Inference](https://github.com/Mmaz1988/xleplusglue/tree/lfg2026_pragmatic_parsing)
 - [UD+Glue](https://github.com/Mmaz1988/xleplusglue/tree/2025_xleplusud) (CURRENT)

As the name suggests, the different versions are interfaced with different syntactic parsers. Please use a version accordingly. 
In the future, the main branch will coincide with the XLE+Glue with Inference branch, as it will be the main version of XLE+Glue.
The corresponding branches contain sub-branches with demos for specific events/papers. From these, substantial updates are incorporated into the main braches regularly.


## Intro

This repository serves to provide a toolset for working with XLE+Glue. Furthermore, it provides the means to create a Docker container for running the components in a virtual environment.
This repository contains:
- UD+Glue: A simple user interface for using the Glue Semantics Workbench with UD
- The latest version of the Glue Semantics Workbench
- The latest version of LiGER (Linguistic Graph Expansion and Rewriting) 
- UD+Glue_web: A browser-based user interface for the GSWB and LiGER
- A docker compose file for running the system on a local docker container

## Requirements

- **[Docker](https://www.docker.com/)**

## Running the system 

The system can be started from the command line by navigating to the _./Docker_ folder within the repository. There you need to execute the following command:

```
docker compose up --build
```
This will start the system and provide access to the browser-based user interface. It is hosted on _http://localhost:80_ 
The system has been tested with Firefox and Chrome. 

The system can be shut down by using the keyboard command _ctrl+c_ in the command line where the docker container is running.

For further details, a documentation document will be made available soon. 







