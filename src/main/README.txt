
The following directories are part of the Kiss core and should not normally be changed.

core
frontend/kiss

By not changing these files you can easily upgrade the Kiss framework.

Your application code would normally reside under:

precompiled
backend
frontend (except the kiss subdirectory)

All front-end code goes under the frontend directory.
If files are changed while the system is running, you will have to re-load them in your browser
(and disable caching in your browser).

All back-end web services go under the backend directory.
Code added under the backend directories gets compiled while the system is running.
Whenever they change, the system automatically recompiles them.
Nearly all of your back-end application will go under here.

The precompiled directory contains all code that is precompiled at build time.
This is common code that all of your web services can access.
If changes are made under here, the system will need to be shut down and restarted.
An important note - except for the Tasks.java file, all other files must be put in some sort of package
layout under the precompiled directory.

