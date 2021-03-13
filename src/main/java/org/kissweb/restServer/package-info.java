/**
 * This package provides the REST server used by KISS.  It supports:
 *
 * 1. Java pre-compiled services
 * 2. Java runtime-compiled services
 * 3. Groovy runtime-compiled services
 * 4. Common Lisp services
 *
 * Groovy is the preferred method because they load and run fast as microservices.
 *
 * All of the above act as microservices (i.e. can be changed on a running system) except the Java pre-compiled services.
 */
package org.kissweb.database;

