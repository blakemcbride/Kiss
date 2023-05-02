# KISS Web Application Full Stack Framework


The KISS Framework is an application development framework for
developing web-based business applications. KISS can also be used to
build command-line utilities, and, in conjunction with
[Electron](https://electronjs.org), desktop applications that are
portable to Windows, Mac, and Linux.


Please see the main web site for a more detailed description at
[https://kissweb.org](https://kissweb.org)

This project is in full release status and not a beta.  It is being used in production.  

Home for the code is at:  [https://github.com/blakemcbride/Kiss](https://github.com/blakemcbride/Kiss)

For build and usage instructions see the manual at `manual/man/index.html`
Or, the user manual can be read online at [Kiss Online User Manual](http://htmlpreview.github.io/?https://github.com/blakemcbride/Kiss/blob/master/manual/man/index.html)

Public discussion and support is available at [Kiss Support](https://groups.google.com/forum/#!forum/kissweb)

## Training Videos

A 6-part training series on KISS is at:

* [Part 1 - Introduction](https://youtu.be/FAnL7dpMld4)
* [Part 2 - Setup & Configuration](https://youtu.be/xT-C-yQo0Ec)
* [Part 3 - Web Services](https://youtu.be/9zRZcxMjoW0)
* [Part 4 - Front-end](https://youtu.be/zMjrp-ft_Tc)
* [Part 5 - Data Persistence](https://youtu.be/pS7DezhYpGo)
* [Part 6 - Deployment](https://youtu.be/fGEzv7uuJCk)

An article about KISS is available at [The KISS Web Development Framework](https://www.linuxjournal.com/content/kiss-web-development-framework)

Another article about KISS is available at [Developing REST Microservices Simply](https://medium.com/codex/developing-rest-microservices-simply-ed934f846ff3)

## Quick Start

Presuming you have the Java JDK (tested with Java 8, 11, and 17), GIT, and an
Internet connection, you can run the following commands to download, install,
configure, and run Kiss, tomcat, and the required JAR files:

### Linux, Mac, BSD, etc.

    git clone https://github.com/blakemcbride/Kiss.git
    cd Kiss
    ./bld develop

### Windows

    git clone https://github.com/blakemcbride/Kiss.git
    cd Kiss
    build-builder
    bld develop

In either environment, you can then go to `http://localhost:8000`
within your browser to use the system.  After that, both the front-end
and the back-end code can be changed while the system is running. No need for
additional compiles or deploys.

The system comes with (essentially) three manuals as follows:


User Manual:        [manual/man/index.html](https://htmlpreview.github.io/?https://github.com/blakemcbride/Kiss/blob/master/manual/man/index.html)

Front-end API:      [manual/jsdoc/index.html](https://htmlpreview.github.io/?https://github.com/blakemcbride/Kiss/blob/master/manual/jsdoc/index.html)

The back-end JavaDoc must be built by you.  This can be done by typing:

    ./bld javadoc               [Linux, Mac, BSD, etc.]
        -or-
    bld javadoc                 [Windows]

You then get:  `build.work/javadoc/index.html`
