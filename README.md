Scalastyle - Scala style checker
================================

Scalastyle examines your Scala code and indicates potential problems with it.

Rules
-----

For a list of (planned) rules, see the [Scalastyle WIKI](https://github.com/scalastyle/scalastyle/wiki).
For a list of actual rules, see the Implemented Rules section below

Usage
-----

    $ mvn exec:java


Running scalastyle from the command line
----------------------------------------

This process will be improved in time, but for now, you need to build scalastyle and then build scalastyle-batch. First install scalastyle in your local maven repo

    $ git clone git://github.com/scalastyle/scalastyle.git
    $ cd scalastyle
    $ mvn install

Then get and build scalastyle-batch

    $ git clone git://github.com/scalastyle/scalastyle-batch.git
    $ cd scalastyle-batch
    $ mvn package # or mvn install

This will create in scalastyle-batch/target a zip file which contains an executable jar along with the dependencies in lib/. Unzip this file somewhere, and run the jar as such:

    $ java -jar scalastyle-batch_2.9.1.jar --config lib/scalastyle_config.xml src/main/scala

or similar. You can copy your initial configuration from lib/scalastyle_config.xml in the scalastyle project, and change it from there.

Maven repo
----------

Snapshots are available from the Sonatype nexus repository: https://oss.sonatype.org/content/repositories/snapshots/.
To use, add something like the following to your settings.xml:

    <repository>
      <id>sonatype-nexus-snapshots</id>
      <name>Sonatype OSS</name>
      <url>https://oss.sonatype.org/content/repositories/snapshots/</url>
      <layout>default</layout>
      <releases>
        <enabled>false</enabled>
        <updatePolicy>never</updatePolicy>
      </releases>
      <snapshots>
        <enabled>true</enabled>
        <updatePolicy>never</updatePolicy>
      </snapshots>
    </repository>

There are only snapshots available at the minute.

Testing
-------

To run the tests:

    $ mvn test

Contributing
------------

Want to contribute? Great! Look at the wiki for potential rules to implement, or do one of your own, and implement it.

1. Read the [developer guide](https://github.com/scalastyle/scalastyle/wiki/developer-guide)
2. Fork the repo.
3. Create a branch (`git checkout -b my_new_rule`)
4. Commit your changes (`git commit -am "Added NoFooAllowed"`)
5. Push to the branch (`git push origin my_new_rule`)
6. Create an [Issue](https://github.com/scalastyle/scalastyle/issues) with a link to your branch
7. Enjoy a coffee and wait

Comment filters
---------------

If you wish to ignore a particular scalastyle rule, you can put a comment before and after the line, with the following syntax:

    // scalastyle:off
    // scalastyle:on

You can also switch off checking for a particular rule by specifying the id of the rule to ignore:

    // scalastyle:off magic.number
    var foobar = 134
    // scalastyle:on magic.number

Custom messages
---------------

Messages are defined within the scalastyle_messages.properties. If however, you wish to have a custom error message for a particular rule, then
you can do so by defining a customMessage element in the configuration, such as:

    <check level="warning" class="org.scalastyle.scalariform.MagicNumberChecker" enabled="true">
     <customMessage>Please don't use magic numbers</customMessage>
    </check>

Implemented Rules
-----------------

These are the rules which are currently implemented:

## Class org.scalastyle.file.FileLengthChecker - Check the number of lines in a file

 * id - file.size.limit
 * default level - WarningLevel

### Parameters

 * Maximum file Length

## Class org.scalastyle.file.FileLineLengthChecker - Check the number of characters in a line

 * id - line.size.limit
 * default level - WarningLevel

### Parameters

 * Maximum line length
 * Tab size

## Class org.scalastyle.file.FileTabChecker - Check that there are no tabs in a file

 * id - line.contains.tab
 * default level - WarningLevel

### Parameters

No Parameters

## Class org.scalastyle.file.HeaderMatchesChecker - Check the first lines of each file matches the text

 * id - header.matches
 * default level - WarningLevel

### Parameters

 * Header

## Class org.scalastyle.file.WhitespaceEndOfLineChecker - Check that there is no trailing whitespace at the end of lines

 * id - whitespace.end.of.line
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.ClassNamesChecker - Check that class names match a regular expression

 * id - class.name
 * default level - WarningLevel

### Parameters

 * Regular expression

## Class org.scalastyle.scalariform.CovariantEqualsChecker - Check that classes and objects do not define equals without overriding equals(java.lang.Object).

 * id - covariant.equals
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.EqualsHashCodeChecker - Check that if a class implements either equals or hashCode, it should implement the other

 * id - equals.hash.code
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.IllegalImportsChecker - Check that a class does not import certain classes

 * id - illegal.imports
 * default level - WarningLevel

### Parameters

 * Illegal Imports

## Class org.scalastyle.scalariform.MagicNumberChecker - Checks for use of magic numbers

 * id - magic.number
 * default level - WarningLevel

### Parameters

 * Ignore (list of numbers to ignore)

## Class org.scalastyle.scalariform.NoCloneChecker - Check that classes and objects do not define the clone() method

 * id - no.clone
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.NoFinalizeChecker - Check that classes and objects do not define the finalize() method

 * id - no.finalize
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.NoWhitespaceAfterLeftBracketChecker - No whitespace after left bracket ''(''

 * id - no.whitespace.after.left.bracket
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.NoWhitespaceBeforeLeftBracketChecker - No whitespace before left bracket ''(''

 * id - no.whitespace.before.left.bracket
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.NullChecker - Check that null is not used

 * id - null
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.ObjectNamesChecker - Check that object names match a regular expression

 * id - object.name
 * default level - WarningLevel

### Parameters

 * Regular expression

## Class org.scalastyle.scalariform.ParameterNumberChecker - Maximum number of Parameters for a method

 * id - parameter.number
 * default level - WarningLevel

### Parameters

 * Maximum Number

## Class org.scalastyle.scalariform.ReturnChecker - Check that return is not used

 * id - return
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.SpacesAfterPlusChecker - Check that the plus sign is followed by a space

 * id - spaces.after.plus
 * default level - WarningLevel

### Parameters

No Parameters
## Class org.scalastyle.scalariform.SpacesBeforePlusChecker - Check that the plus sign is preceded by a space

 * id - spaces.before.plus
 * default level - WarningLevel

### Parameters

No Parameters

## Class org.scalastyle.scalariform.StructuralTypeChecker - Check that structural types are not used

 * id - structural.type
 * default level - WarningLevel

### Parameters

No Parameters

## Class org.scalastyle.file.RegexChecker - Check that a regular expression cannot be matched

 * id - regex
 * default level - WarningLevel

### Parameters

 * Regular expression the regular expression to use

### Example

    <check level="warning" class="org.scalastyle.file.RegexChecker" enabled="true">
      <parameters>
        <parameter name="regex"><![CDATA[(?m)^\s\*$(\r|)\n^\s*$(\r|)\n]]></parameter>
      </parameters>
      <customMessage>No double blank lines</customMessage>
    </check>

  The above example would check for double blank lines in a file and report with a custom error.

## Class org.scalastyle.scalariform.NumberOfTypesChecker - Check that a file doesn't contain too many types

 * id - number.of.types
 * default level - WarningLevel

### Parameters

 * maxTypes: The maximum number of types (outer & inner classes) to allow per file

### Example

    <check level="warning" class="org.scalastyle.scalariform.NumberOfTypesChecker" enabled="true">
      <parameters>
        <parameter name="maxTypes">20</parameter>
      </parameters>
    </check>

## Class org.scalastyle.scalariform.CyclomaticComplexityChecker - Check the cyclomatic complexity of methods

 * id - cyclomatic.complexity
 * default level - WarningLevel

### Parameters

 * maximum: The maximum allowed cyclomatic complexity of a method

### Example

    <check level="warning" class="org.scalastyle.scalariform.CyclomaticComplexityChecker" enabled="true">
      <parameters>
        <parameter name="maximum">10</parameter>
      </parameters>
    </check>

## Class org.scalastyle.scalariform.UppercaseLChecker - Check that long literals use an uppercase L

 * id - uppercase.l
 * default level - WarningLevel

### Parameters

No Parameters

### Example

    <check level="warning" class="org.scalastyle.scalariform.UppercaseLChecker" enabled="true"/>

## Class org.scalastyle.scalariform.IfBraceChecker - Check that `if` blocks have braces

 * id - if.brace
 * default level - WarningLevel

### Parameters

 * singleLineAllowed: The lack of braces is allowed if everything is on one line
 * doubleLineAllowed: The lack of braces is allowed if the `if` and the `else` are on different lines.
   The expressions associated with the if and the else still need to be on the same lines though.

### Example

    <check level="warning" class="org.scalastyle.scalariform.IfBraceChecker" enabled="true">
      <parameters>
        <parameter name="singleLineAllowed">true</parameter>
        <parameter name="doubleLineAllowed">false</parameter>
      </parameters>
    </check>
