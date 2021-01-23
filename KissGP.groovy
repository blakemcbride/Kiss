/*
     This is a demonstration program.  It demonstrates the ability to run
     a Groovy program from the command-line in the context of Kiss and 
     PostgreSQL.
     
     First, the single file needed is KissGP.jar
     It can be created with:  ./bld KissGP
     It will end up in the build.work directory.
     
     Second, to run this file, use:  
        java -jar build.work/KissGP.jar KissGP.groovy
*/

import org.kissweb.DateTime


public static void main(String [] args) {
	println "Welcome to KissGP.groovy"
	int n = args.size()
	for (int i=0 ; i < n ; i++)
		println "arg " + (i+1) + " = " + args[i]
	println DateTime.currentDateTimeFormatted()
}
