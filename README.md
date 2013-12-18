CycDAG
======

WHAT:
A lightweight reimplementation of the OpenCyc ontology system. This project aims to replicate most of OpenCyc's functionality and data while remaining reasonably simple to use, efficient, and with low-memory usage. This need came about to address issues with OpenCyc's stability, accessibility, memory usage, and perhaps speed (though it is not yet known if CycDAG is faster).

WHY:
As stated above, the CycDAG came about as a prototype to test if OpenCyc could be reimplemented into a faster, smaller, and more open form, while retaining most of the same operations. The reasoning is this: OpenCyc is built to do A LOT of heavy reasoning, but this is often not needed for simple projects. For this reason, CycDAG only aims to reimplement minor-mid level reasoning; complex backward chaining is not intended to be supported (but it can be with appropriate modules).

The open nature of CycDAG also gives a much clearer view of what the ontology is doing and how it is doing it. The modular add-ins allow users to add in their own chunks of code to the core system, expanding the number of uses for the system.

HOW:
The first thing that needs to happen is the OpenCycAssertions.zip file needs to be unzipped. The file allAssertions.txt contains every direct assertion from OpenCyc (perhaps not the Horn clauses that form the rules OpenCyc uses). CycDAG will read this file on startup and create the ontology (this can take some time. Best to start it up overnight).

To run as a command-line interface, compile the java files and launch CycDAGCLI. Optional arguments include -p <portNumber> -r <rootDirectory> -n <numCachedNodes> -e <numCachedEdges>. The various config files define additional aspects such as which commands and modules are in use (many are defined within the java files).

Once started, CycDAGCLI either creates the ontology from file (allAssertions.txt), or loads up a pre-serialised ontology. The former process takes quite some time (20 minutes on some machines, half a day on others), whereas the latter should not take too long. During the former process, one can begin using the ontology as normal, though assertions may be missing throughout.

To connect to the ontology, run 'telnet localhost 2425' (or whatever port number is used). Type 'list' to see all available commands and type 'help <command>' to see more information on each command.

WHO & WHERE:
The CycDAG was initially implemented by Dr Sam Sarjant at The University of Waikato, New Zealand. It attempts to reproduce many of the inference processes the OpenCyc ontology performs. It is built upon the DagYo framework (https://github.com/Effervex/DagYo).

All source code is copyright The University of Waikato, New Zealand (2013).

Contact: Sam Sarjant sarjant@waikato.ac.nz
