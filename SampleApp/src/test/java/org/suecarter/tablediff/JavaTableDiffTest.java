package org.suecarter.tablediff;

import org.junit.Test;

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
        ReportContent diffReport = TableDiff.produceReportDiff(
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
                {"a", "b","c","x"},
                {"d", "e","f","g"},
                {"h", "i","j","k"},
                {"x", "y","z","z"}
        };
        ReportContent table1 = JTableDiff.reportContentFromArray(arrayTable, 1, 2);
        arrayTable[3][3] = "l";
        arrayTable[0][2] = "j";
        ReportContent table2 = JTableDiff.reportContentFromArray(arrayTable, 1, 2);
        ReportContent diffReport2 = TableDiff.produceReportDiff(table1, table2);
        String diffs2 = StringTableDiff.diffReportToString(TableDiff.onlyTheDiffs(diffReport2));
        assertEquals(diffs2,
                "+-+---------------------+\n" +
                "|a|[-c-]{+j+}|x         |\n" +
                "|d|f         |g         |\n" +
                "+-+---------------------+\n" +
                "|x|z         |[-z-]{+l+}|\n" +
                "+-+---------------------+\n");
    }
}
