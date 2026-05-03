# tea-make
Transient Emacs cmake (tools) integration.

I started this project to learn transient and to replace my old cmake-build project with something better, instead of trying to integrate presets in it. My goal is that it should be easy and seemless to configure, build, test, and pack using this system.

> [!WARNING]
> This is still heavily under construction but may still be used.
> The effectiveness is the most under development but functionality should be there, for the most parts.

This project is heavily inspired by Magit (duh!) and many constructions and ideas are stolen from that project.

# Usage
The main idea for tea-make is to hold a cmake project together. Invoking cmake seamlessly from the menu system and also storing all previously used projects for easy access later on.

## Setup
Download and add teamake to the path ```(add-to-list 'load-path <path>)``` and just add ```(require 'teamake)``` somewhere in the initialization of emacs and you are good to go.

## Basic
The main invokation point is ```teamake``` which is an alias for ```teamake-project```. From here you can create new projects, load previously used projects, save current project or delete projects.

### Configuring a teamake project
When creating a new project, one must select a correct source-dir containing a CMakeLists.txt file. The project-name in that file is used as suggested project name for the teamake project but may be changed. Changing this is a good idea if developing using worktrees in git since otherwise the project name is the same for all worktrees (since spawning from same repository).
