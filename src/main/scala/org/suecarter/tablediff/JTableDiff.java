package org.suecarter.tablediff;

public class JTableDiff {
    static public ReportContent reportContentFromArray(Object[][] content, int rowDepth, int colDepth) {
        return ReportContent$.MODULE$.apply(content, rowDepth, colDepth);
    }
    static public ReportContent produceReportDiff(ReportContent table1, ReportContent table2) {
        return TableDiff$.MODULE$.produceReportDiff(table1, table2, TableDiff$.MODULE$.defaultMainValueComparison());
    }
}
