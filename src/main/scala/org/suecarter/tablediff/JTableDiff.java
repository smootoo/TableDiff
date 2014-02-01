package org.suecarter.tablediff;

public class JTableDiff {
    static public ReportContent reportContentFromArray(Object[][] content, int rowDepth, int colDepth) {
        return ReportContent$.MODULE$.apply(content, rowDepth, colDepth);
    }
}
