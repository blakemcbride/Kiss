# KISS Web Application Framework


The KISS Framework is an application development framework for
developing web-based business applications.  Please see the main web
site for a more detailed description at
[https://kissweb.org](https://kissweb.org)


Home for the code is at:  [https://github.com/blakemcbride/Kiss](https://github.com/blakemcbride/Kiss)

For build and usage instructions see the manual at `manual/man/index.html`
Or, the user manual can be read online at [Kiss Online User Manual](http://htmlpreview.github.io/?https://github.com/blakemcbride/Kiss/blob/master/manual/man/index.html)

Public discussion and support is available at [Kiss Support](https://groups.google.com/forum/#!forum/kissweb)

## Quick Start

Presuming you have the JDK (tested with JDK 8 & 11), GIT, and an
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
within your browser to use the system.

The system comes with (essentially) three manuals as follows:


User Manual:        manual/man/index.html

Front-end API:      manual/jsdoc/index.html

The back-end JavaDoc must be built by you.  This can be done by typing:

    ./bld javadoc               [Linux, Mac, BSD, etc.]
        -or-
    bld javadoc                 [Windows]

You then get:  `build.work/javadoc/index.html`
