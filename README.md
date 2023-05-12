[![Build Status](https://github.com/smootoo/TableDiff/actions/workflows/ci.yml/badge.svg)](https://github.com/smootoo/TableDiff/actions/workflows/ci.yml)

TableDiff
=========

A Scala based (but usable from Java and other JVM languages) utility for finding a diff of 2 tables, taking account of their structure. 

Available on [maven central](http://search.maven.org/#artifactdetails|org.suecarter|tablediff_3|1.1|jar)

There are lots of options in the library functions, so for simple usage, you probably want to create little util functions.
e.g. to compare 2 lists of cases classes.
```
import org.suecarter.tablediff._
def listsDiffString(leftList: Iterable[Product], rightList: Iterable[Product]): String = {
  def listReport(list: Iterable[Product]) =
    ReportContent(
      columnHeaders = Seq(list.headOption.getOrElse(Nil).productElementNames.toSeq), // Scala 2.13+ for this
      mainData = list.map(x => x.productIterator.toSeq).toSeq,
    )
  StringTableDiff.diffReportToString(
    TableDiff.produceReportDiff(listReport(leftList), listReport(rightList)),
  )
}

case class A(b: Int, c: String)
listsDiffString(Seq(A(1,"x"), A(2,"y"), A(4, "c")), Seq(A(1,"x"), A(2,"z"), A(3, "c"))) 

+---------------------+
|b         |c         |
+---------------------+
|1         |x         |
|2         |[-y-]{+z+}|
|[-4-]{+3+}|c         |
+---------------------+
```
 
For usage example in java. (See this in the class [org.suecarter.javatablediffexample.JavaTableDiffTest.java](./SampleApp/src/test/java/org/suecarter/javatablediffexample/JavaTableDiffTest.java), the testDemo test)

```
Compare left table
+----+--------------+
|    |col1|col2|col3|
+----+--------------+
|row1|m1,1|m2,1|m3,1|
|row2|m1,2|m2,2|m3,2|
+----+--------------+

to right table
+----+---------------------+
|    |col1|col2|col2.5|col3|
+----+---------------------+
|row1|m1,x|m2,1|m2.5,1|m3,1|
|row2|m1,2|m2,2|m2.5,2|m3,2|
+----+---------------------+

Produces these diffs, showing us that the cell in row1,col1 has
changed from m1,1 to m1,x and a whole new column (col2.5) has 
been added.
+----+----------------------------------+
|    |col1         |col2|{+col2.5+}|col3|
+----+----------------------------------+
|row1|m1,[-1-]{+x+}|m2,1|{+m2.5,1+}|m3,1|
|row2|m1,2         |m2,2|{+m2.5,2+}|m3,2|
+----+----------------------------------+
```


There was HTML rendering code that was removed in V1.1. It was old and not used by me any more. Let me know if anyone still needs it.

