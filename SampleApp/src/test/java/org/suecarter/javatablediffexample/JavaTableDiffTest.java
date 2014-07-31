package org.suecarter.javatablediffexample;

import org.junit.Test;
import org.suecarter.tablediff.*;

import java.io.File;

import static org.junit.Assert.assertEquals;

public class JavaTableDiffTest {
    @Test
    public void testReportDiff() {
        String[][] cols = {
                {"a","b"},
                {"c","d"}};
        String[][] rows = {
                {"r1","r2"},
                {"r3","r4"}};
        Integer[][] mainData = {
                {3,4},
                {5,6}};
        Integer[][] diffData = {
                {3,4},
                {5,7}};
        ReportContent diffReport = JTableDiff.produceReportDiff(
                new ReportContent<String, String, Integer>(rows, cols, mainData, cols),
                new ReportContent<String, String, Integer>(rows, cols, diffData, cols));
        String onlyDiffs = StringTableDiff.diffReportToString(TableDiff.onlyTheDiffs(diffReport));
        assertEquals(onlyDiffs,
                        "+-----+----------+\n" +
                        "|a |b |b         |\n" +
                        "|c |d |d         |\n" +
                        "+-----+----------+\n" +
                        "|r3|r4|[-6-]{+7+}|\n" +
                        "+-----+----------+\n");
    }

    @Test
    public void testArraySlicing() {
        String[][] arrayTable = {
                {" ", "c11","c21","c31"},
                {" ", "c12","c22","c32"},
                {"r1", "m11","m21","m31"},
                {"r2", "m12","m22","m32"}
        };
        ReportContent table1 = JTableDiff.reportContentFromArray(arrayTable, 1, 2);
        arrayTable[3][3] = "mD32iff";
        arrayTable[0][2] = "c21Diff";
        ReportContent table2 = JTableDiff.reportContentFromArray(arrayTable, 1, 2);
        ReportContent diffReport2 = JTableDiff.produceReportDiff(table1, table2);
        String diffs2 = StringTableDiff.diffReportToString(TableDiff.onlyTheDiffs(diffReport2));
        assertEquals(diffs2,
                "+--+---------------------------+\n" +
                "|  |c21{+Diff+}|c31            |\n" +
                "|  |c22        |c32            |\n" +
                "+--+---------------------------+\n" +
                "|r2|m22        |m{+D+}32{+iff+}|\n" +
                "+--+---------------------------+\n");
    }
    @Test
    public void testDemo() {
        String[][] arrayTableLeft = {
                {" ", "col1", "col2", "col3"},
                {"row1", "m1,1", "m2,1", "m3,1"},
                {"row2", "m1,2", "m2,2", "m3,2"}
        };
        String[][] arrayTableRight = {
                {" ", "col1", "col2", "col2.5", "col3"},
                {"row1", "m1,x", "m2,1", "m2.5,1", "m3,1"},
                {"row2", "m1,2", "m2,2", "m2.5,2", "m3,2"}
        };
        ReportContent tableLeft = JTableDiff.reportContentFromArray(arrayTableLeft, 1, 1);
        System.out.println("Compare left table\n" + StringTableDiff.diffReportToString(tableLeft));
        ReportContent tableRight = JTableDiff.reportContentFromArray(arrayTableRight, 1, 1);
        System.out.println("to right table\n" + StringTableDiff.diffReportToString(tableRight));
        ReportContent diffReport = JTableDiff.produceReportDiff(tableLeft, tableRight);
        String diffs = StringTableDiff.diffReportToString(diffReport);
        System.out.println("Produces these diffs\n" + diffs);
        assertEquals(diffs,
                       "+----+----------------------------------+\n" +
                        "|    |col1         |col2|{+col2.5+}|col3|\n" +
                        "+----+----------------------------------+\n" +
                        "|row1|m1,[-1-]{+x+}|m2,1|{+m2.5,1+}|m3,1|\n" +
                        "|row2|m1,2         |m2,2|{+m2.5,2+}|m3,2|\n" +
                        "+----+----------------------------------+\n");
        File dir = new File(System.getProperty("java.io.tmpdir") + File.separatorChar + "TableDiffTestsHTMLFiles");
        dir.mkdirs();
        HTMLTableDiff.writeHTMLDiffAndContext("TableDiffDemo", dir, diffReport);
    }
}
