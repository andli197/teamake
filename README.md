# tea-make
Transient Emacs cmake (tools) integration.

I started this project to learn transient and to replace my old cmake-build project with something better, instead of trying to integrate presets in it. My goal is that it should be easy and seemless to configure, build, test, and pack using this system.

(!!!)
This is still heavily under construction but may still be used for executing configurations, builds and presets.
(!!!)

magit is the inspiration and many patterns and clever constructions are plainly stolen from that project.

# Usage
Add the path to teamake and have (require 'teamake) somewhere in the current configuration and you are good to go.
Invoke the project management using the command 'teamake (alias for teamake-project) where projects can be handled. From that menu the selected project can go to the different cmake invocation menus (configuration, build, etc.)
If having configured a project once and saved it, each cmake menu can be invoked from within that projects source dir and the correct project is used with the current values for each project and command.

Example:
Project source dir is "~/projects/my_proj/code"
1. Invoke the teamake-project menu (alias teamake works as well). 
2. Select option for Creating a project.
3. Now select the source dir
   The project menu will re-appear with the source dir being the selected one and project name being parsed from the CMakeLists.txt file.
4. Now select Configure from the CMake part of the project management menu and the project can be configured using the options available there.

After a project is created it is automatically saved in a global project list that is persisted to a file during emacs shutdown. Next time teamake is loaded that file is parsed and all projects and configurations are readily available again for quick usage.

# Architectural idea
The main idea was to encapsulate different CMake projects with its configurations and settings individually. So one may work on different projects and seamlessly switch between them with output from each project placed in different buffers.

Hence the main structure for such a project is encapsulated in a property list with three main properties.
1. :source-dir
   This is where the source code is located and is a dead given for creating a project.
2. :name
   The name is automatically parsed from the CMakeLists.txt (project property) but can be customized.
3. :current
   This property holds the currently configured transient values for each type of transient (here, each transient represents a different cmake concept, i.e. configure, build, test, pack, etc.)
   
