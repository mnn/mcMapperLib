mcMapperLib
===========

It's a simple library for accessing mapping information in files used by [MCP](http://mcp.ocean-labs.de).

[![Build Status](https://travis-ci.org/mnn/mcMapperLib.png?branch=master)](https://travis-ci.org/mnn/mcMapperLib)

Used libraries
--------------

* [opencsv](http://opencsv.sourceforge.net/) - CSV parser
* [ScalaTest](http://scalatest.org/) - test framework

Download
--------

Donate me a few cents via [AdFly](http://adf.ly/2536344/mcmapperlib--1-0) or use [direct](http://www.mediafire.com/download/vq6ch49ciadq3kp/mcmapperlib_2.10-1.0.jar) link.

How to use
----------
First, don't forget to add opencsv library to your project's dependencies.
```scala
libraryDependencies += "net.sf.opencsv" % "opencsv" % "2.3"
```
Then you can command mcMapper to load all those data files.
```scala
val data = McMapper.load(mcp811path)
```
If you'd like to fast search in your data, you can use `MappingDatabaseSearchable`.
```scala
val db = MappingDatabaseSearchable(data)
```
Then you can simply call `db.searchClass("cl")` to get classes named `cl` (so far it's only supported a full-match).

For more samples you can take a look at `CompleteParsingTest` test class.
