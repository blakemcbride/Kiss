
This package is a Java library for dealing with JSON objects.

This is a fork of https://github.com/stleary/JSON-java
by Blake McBride

It fixes certain problems with the library.  Pull requests were
not accepted because it would break old code.  I've just decided
to support my own version from this point forward.

At first, I maintained this as an independent fork of the original.
However, I encountered a problem when I added a different jar, and it required and included
the original version of this package. I therefore had the same package twice which doesn't work.
I ended up deciding to merge my fork into Kiss and renaming the package so there wouldn't be a hit.

Corrections to the original library:

1.  

JSONObject.put("key", 'x');
and
JSONObject.put("key", (Character) 'x');

used to give different results. The first one would treat 'x' as in
int. My change makes them work the same (treat as a character not an
int).


2.  

JSONObject.put("key", null) used to remove the key!!  I changed it to 
create a json key with a null value.

3.

Created a build.sh build script
(what you need to build json.jar)

4.  Enhanced .gitignore

5.  Changed getString to return null when the JSON has a null.

6.  toString used to convert "</" into "<\/".  Corrected.

7.  Add ability for strings to have embedded \n and \r

8.  Restructure to be more IDE friendly

9.

Changed all of the get methods to return null when not found rather than throwing an exception.

Add get methods with default return values

Remove opt methods

10.  Added:

JSONObject.getCharacter
JSONArray.getCharacter

11.  Improve handling of NULL on numeric fields and correct JSONArray numeric return types
     Improve Java 17 support

12.  Enhanced getJSONObject and getJASONArray to handle cases where those elements are 
     being represented as Strings

13.  Library was too agressive at trying to find object values.  Was causing stack overflows.
     Corrected.

14.  Removed the difference between JSON null and Java null

15.  Handle Unicode Strings with BOM codes

