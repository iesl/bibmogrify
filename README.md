BibMogrify
==========

_High-volume format translation and processing of scholarly citations and patents._

---

BibMogrify is a general framework for translating and processing bibliographic data, including scholarly citations and patents. 

BibMogrify reads various input formats (such as BibTeX, various XML formats, etc,) into a common internal representation.  It can then write these records in a variety of formats, including BibTeX, Mallet, and a host of tab-delimited formats.  Outputs may include computed fields such as word counts and citation counts, as well as text that has been processed in some way (e.g. lowercasing, punctuation removal, etc.).  BibMogrify is easily extensible with additional format readers, writers, and data-massaging functions.

BibMogrify decompresses archive files on the fly in a streaming manner, avoiding performance bottlenecks due to i/o and memory limitations.  It can also take full advantage of multicore processors.  We commonly use this tool to rapidly process tens of millions of records.


Principle of Operation
----------------------

BibMogrify applies a series of Transformers to citation records, where the chain of transformations is specified on the command line.  Records may have different data types at different points in the pipeline.  Each transformation accepts one record of a given input type, and emits zero, one, or many records of a given output type.  The chain is thus a series of "flatmap" operations.  Care must be taken to match the output type of each transformation with the input type of the next; otherwise an error will result.

The pipeline is fed with Strings either provided as command-line arguments or, lacking these, read from STDIN (one string per line).  Thus, the first Transformer in the chain must accept Strings as input.

A Sink is an ultimate destination of records of a given type.  The default case is the ConsoleSink, which accepts Strings and prints them to STDOUT.  Thus, in the common case, the transformation pipeline must end with a transformer that produces Strings.  It is also possible to write a Sink which writes structured data into a database or takes some other action on each record.

Choosing the proper order of Transformers may require examining the code to understand what they do (lacking sufficient documentation for now, sorry).

Usage
-----

```
    Usage:  bibmogrify [options] <input1 input2 ...>
    
  Options:

  -s, --sink  <arg>    sinks (mongo, console, jsonconsole)
  -x, --xform  <arg>   transforms, comma separated (see below)
      --help           Show help message
      --version        Show version of this program
```

Running `bibmogrify --help` will show a list of available transformers.


Example
-------

Imagine that we have a large number of compressed files containing citation data in Medline format, and we'd like to convert these all to BibTeX.  Here's how:

```sh
bibmogrify --xform toUrl,byLinePar,toUrl,extract,medline,bibtex /home/soergel/bibmogrify/example/allfiles | gzip > allfiles.bibtex.gz
```

That works as follows:

* `/home/soergel/bibmogrify/example/allfiles` is a list of all the files to be imported (i.e., probably generated with "find").
* The first `toUrl` interprets that command-line argument as a filename.
* `byLinePar` says to process the lines of that file in parallel, to take advantage of a multicore machine.  (`byLine` would read the lines of the file without providing concurrency)
* the second `toUrl` says that the lines from the first file are also filenames.
* `extract` deals with the fact that those filenames may be tar, gz, zip, etc. in whatever combination.
* `medline` is the parser (it implements `Transformer[NamedInputStream, StructuredCitation]`)
* `bibtex` is the output writer (it implements `Transformer[StructuredCitation, String]`)



Common data types used by transformers
--------------------------------------

* `String`

* `URL`

* `TaggedCitation`.  A string that has been tagged with labeled spans, where the labels are typically "authors", "title", and so forth.  See the `standardLabels` and `extendedLabels` XML parsers for details.

* `StructuredCitation`.  A common representation accommodating all the fields we expect ever to find in citation data.  This is the central class for the whole system; reading the code is the best way to see what all it can accomodate.  Fields may or may not be populated.  In particular, there is no guarantee that all the fields available in some input file are parsed, even if there is an appropriate slot in the StructuredCitation type.  This is just because, when writing the parsers, we naturally concentrated on the fields that we immediately needed.  Please feel free to make improvements in this regard!

* `StructuredPatent`.  Extends StructuredCitation with patent-specific fields (e.g., Claims and Patent Families).


Person Name Normalization
-------------------------

Some of the provided plugins normalize author names using [NameJuggler](https://github.com/iesl/namejuggler), so don't be surprised if "Bach, JS" in the input turns into "J. S. Bach" in the output (etc.).


Extending
---------

You can write your own Transformers.  Just create objects implementing `Transformer[From, To]` and `NamedPlugin`, and place the resulting classes on the classpath.  You can confirm that Bibmogrify picks up your plugins by checking the help message.

For example, here's a simple filter plugin:

```scala
object RequireTitle extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin with Logging
    {
    val name = "requireTitle"
    val fromType = "StructuredCitation"
    val toType = "StructuredCitation"
    
    def apply(cm: StructuredCitation) = cm.title.map(q => cm)
    }

```

Building
--------

You can build an executable jar like this:
```sh
./sbt assembly
```


Questions and feedback
----------------------

Please contact David Soergel <soergel@cs.umass.edu>.


