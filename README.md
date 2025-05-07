# tea-make
Transient Emacs cmake (tools) integration.

I started this project to learn transient and to replace my old cmake-build project with something better, instead of trying to integrate presets in it. My goal is that it should be easy and seemless to configure, build, test, and pack using this system.

(!!!)
This is still heavily under construction but may still be used for executing configurations, builds and presets but not as seemlessly as I would like. It will come.
For now each prefix must be invoked from the path it should be executed in. That is, configuration must be invoked from the code-tree and building must be invoked from the build-tree.
(!!!)

magit is the inspiration and many patterns and clever constructions are plainly stolen from that project.
