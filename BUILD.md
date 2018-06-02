
__See Kiss.pdf instead of this file.__

# Build instructions under Gradle

The build system is [Gradle](https://gradle.org)

Most IDE's (eclipse, intelliJ, NetBeans) can import Gradle projects.
This will give you a great development and debugging environment.

If you'd prefer to use raw gradle then install gradle and:

To build do:  `gradle war`
The WAR file can be just placed in the webapps directory of tomcat to deploy.

The WAR file ends up in build/libs

To build javadocs:  `gradle javadoc`

The docs end up in build/docs/javadoc

To clean the system:  `gradle clean`

To run the app with a local server do:  `gradle appRun`
It will display the URL to use
^C to exit

