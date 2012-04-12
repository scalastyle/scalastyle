Scalastyle - Scala style checker
================================

Scalastyle examines your Scala code and indicates potential problems with it.

Rules
-----

For a list of (planned) rules, see the [Scalastyle WIKI](https://github.com/scalastyle/scalastyle/wiki).
For a list of actual rules, see the Implemented Rules section below

Usage
-----

    mvn exec:java

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


