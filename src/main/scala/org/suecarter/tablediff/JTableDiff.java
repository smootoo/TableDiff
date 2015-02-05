package org.suecarter.tablediff;

/**
 * Java wrapper for TableDiff, to make syntax cleaner
 */
public class JTableDiff {
    /**
     * Given an array of array of type T, produce ReportContent structure
     * @param content source data array
     * @param rowDepth how many columns from the left constitute the row header section
     *                 i.e. how wide are top left and bottom left sections
     * @param colDepth how many rows down constitute the column header section
     *                 i.e. how deep are top left and top right sections
     * @param <T>
     * @return ReportContent
     */
    static public <T> ReportContent reportContentFromArray(T[][] content, int rowDepth, int colDepth) {
        return ReportContent$.MODULE$.apply(content, rowDepth, colDepth);
    }

    /**
     * Produce a diff of 2 tables. Just a java wrapper of the scala function, to make the syntax cleaner
     * @param table1 Left table
     * @param table2 Right table
     * @return ReportContent containing diffs
     */
    @SuppressWarnings("unchecked")
    static public ReportContent produceReportDiff(ReportContent table1, ReportContent table2) {
        return TableDiff$.MODULE$.produceReportDiff(table1, table2, TableDiff$.MODULE$.defaultMainValueComparison());
    }
}
