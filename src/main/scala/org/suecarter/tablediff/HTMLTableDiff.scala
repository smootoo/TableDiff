package org.suecarter.tablediff

import ReportContent._
import java.io._

/**
 * Functions to produce html representation on table and tables containing diffs
 */
object HTMLTableDiff {
  import TableDiff._
  private def readStringFromFile(filename: String) = {
    val buffer = new StringBuffer
    val inputStream = new InputStreamReader(new FileInputStream(filename))
    val tmp = new Array[Char](4096)
    while (inputStream.ready) {
      val read: Int = inputStream.read(tmp)
      buffer.append(tmp, 0, read)
    }
    inputStream.close
    buffer.toString
  }
  private def writeStringToFile(filename: String, text: String) {
    val writer = new OutputStreamWriter(new FileOutputStream(filename))
    writer.write(text)
    writer.close
  }

  private def extractJSON(htmlString: String) =
    """(?s)var gridData = .*};""".r.findFirstIn(htmlString).map(_.replace("var gridData = ", "").replace("};", "}"))

  /**
   * Write a file containing an html representaion of the report
   * @param extraHeader Extra text to add into the html page
   */
  def writeHTMLFile[R, C, M](reportName: String, directory: File, report: ReportContent[R, C, M], extraHeader: Option[String] = None) {
    val tableString = toHTMLString(report, reportName, extraHeader)
    writeStringToFile(directory.getCanonicalPath + "/" + reportNameToLink(reportName), tableString)
    val jsFile = new File(directory, "/Grid.js")
    if (!jsFile.exists)
      writeStringToFile(jsFile.getCanonicalPath, gridJS)
    val cssFile = new File(directory, "/Grid.css")
    if (!cssFile.exists)
      writeStringToFile(cssFile.getCanonicalPath, cssText)
  }

  private def reportNameToLink(name: String) = "./" + name + ".html"

  /**
   * Write a file containing an html representation of the report and a linked page just containing
   * any diffs. The report has to be one containing Diffs.
   */
  def writeHTMLDiffAndContext[R, C, M](reportName: String,
                                       directory: File,
                                       report: ReportContent[ValueDiff[R], ValueDiff[C], ValueDiff[M]]) {
    val fullName = "FullContext_" + reportName
    val onlyReportDiffs = onlyTheDiffs(report)
    writeHTMLFile(reportName, directory, onlyReportDiffs,
      Some("""<br>""" +
        (if (onlyReportDiffs.nonEmpty && onlyReportDiffs == report)
          "The report is full of diffs. The full report is the same as the diff report"
        else {
          writeHTMLFile(fullName, directory, report,
            Some("""<br><a href=""""+ reportNameToLink(reportName) + """">Go back to just the diffs</a>"""))
          "Viewing just the diffs." +
          """ <a href=""""+ reportNameToLink(fullName) + """">See the full report</a>"""
        })))
  }

  private def footerFixedCols(report: ReportContent[_,_,_]) =
    if (report.columnCount == 0 && report.mainDataColumnCount == 0) 0 else report.rowWidth
  protected[tablediff] def toHTMLString[R, C, M](report: ReportContent[R, C, M],
                            name: String,
                            extraHeader: Option[String] = None) = {
    htmlHeader(name, extraHeader.getOrElse("") + (if(report.isEmpty) " This report is empty" else "")) +
      toJsonTable(report) +
      htmlFooter(footerFixedCols(report))
  }

  // scala.util.parsing.json.JSON not thread safe
  import scala.util.parsing.json.JSON
  private object jsonParseLock

  // Helpers for extracting the json
  private class ClassExtract[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

  private object MapStrAny extends ClassExtract[Map[String, Any]]
  private object ReportSectionString extends ClassExtract[ReportSection[String]]
  /**
   * Extract a report from a json representation held in the passed in String
   * @return A ReportContent instance with all the elements of type String
   */
  def fromJsonTable(tableString: String) = {
    val jsonString = extractJSON(tableString).getOrElse("")
    val jsonMap = jsonParseLock.synchronized {
      JSON.parseFull(jsonString)
    }
    jsonMap match {
      case Some(MapStrAny(j)) => {
        import org.apache.commons.lang3.StringEscapeUtils.{unescapeHtml4 => unescape}
        def unescapeFromJson(s: String) = {
          unescape(s.replaceAll("""\n""","\n"))
        }
        val rowWidth = j.getOrElse("FixedCols", 2.0) match {
          case x: Double => x.toInt
        }
        val headerSection = j.get("Head").map {
          case ReportSectionString(x) => x
        }.getOrElse(Seq())
        val (rowColumnHeaders, columnHeaders) = headerSection.foldLeft((Seq[Seq[String]](), Seq[Seq[String]]()))((headers, row) => {
          val (rowHead, mainBit) = row.splitAt(rowWidth)
          (headers._1 ++ Seq(rowHead), headers._2 ++ Seq(mainBit))
        })
        val main = j.get("Body").map {
          case ReportSectionString(x) => x
        }.getOrElse(Seq())
        val (rows, mainData) = main.foldLeft((Seq[Seq[String]](), Seq[Seq[String]]()))((headers, row) => {
          val (rowHead, mainBit) = row.splitAt(rowWidth)
          (headers._1 ++ Seq(rowHead), headers._2 ++ Seq(mainBit))
        })
        val escapeMe = (cell: Any) => unescapeFromJson(cell.toString)
        ReportContent(rows, columnHeaders, mainData, rowColumnHeaders).mapAllCells(escapeMe)
      }
      case None => emptyReport
    }
  }

  import org.apache.commons.lang3.StringEscapeUtils.{escapeHtml4 => escape}
  private def escapeForJson(s: String) = {
    escape(s.replaceAll("\n","""\\\n"""))
  }
  // If I knew what I was doing with html, this would probably be css
  private def htmlColour(colour: String) = "<b style=\\\"color:" + colour + ";\\\">"

  /**
   * take a diff value and render it in html
   */
  protected[tablediff] def valueDiffRenderer[T](value: ValueDiff[T]) =
    StringTableDiff.valueDiffRenderer(value,
                                      (x: T) => escapeForJson(x.toString),
                                      htmlColour("red") + "[-</b>" + htmlColour("black"),
                                      htmlColour("green") + "{+</b>" + htmlColour("black"),
                                      htmlColour("black"),
                                      "</b>" + htmlColour("red") + "-]</b>",
                                      "</b>" + htmlColour("green") + "+}</b>",
                                      "</b>")

  /**
   * take a report and produce a json representation
   * @return A string representation of the json for the report
   */
  def toJsonTable[R, C, M](report: ReportContent[R, C, M], gridName: String = "gridData") = {
    def jsonRowMap[T](row: Seq[T]) = "[" + row.map(x => "\"" + (x match {
      case d: ValueDiff[T @unchecked] => valueDiffRenderer(d) //unchecked as we don't care about the type in ValueDiff
      case s => escapeForJson(x.toString)
    }) + "\"").mkString(",") + "]"
    def bodyMap[T](name: String, body: Seq[T]) = name + " : [\n" + body.mkString(",\n") + "]\n"

    def emptyCells(i: Int) = (0 until i).map(x => Right(None))
    val headerString = bodyMap("\"Head\"",
      for ((rowHeader, columnHeader) <- report.rowColumnHeaders zipAll(report.columnHeaders, Seq(), Seq()))
      yield jsonRowMap(rowHeader ++ emptyCells(report.rowWidth - rowHeader.size) ++ columnHeader ++ emptyCells(report.columnCount - columnHeader.size)))
    val bodyString = bodyMap("\"Body\"",
      for ((row, data) <- report.rowHeaders zipAll(report.mainData, Seq(), Seq()))
      yield jsonRowMap(row ++ emptyCells(report.rowWidth - row.size) ++ data))
    "var " + gridName + " = {" + List(headerString, bodyString, "\"FixedCols\" : " + report.rowWidth).mkString(",\n") + "};"
  }

  private def htmlHeader(name: String, extraHeader: String = "") =
    """
      <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<title>""" + name + """</title>
      <meta name="description" content="HTML render of a report structure.">
      <meta http-equiv="X-UA-Compatible" content="IE=Edge">
      <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
      <link href="http://fonts.googleapis.com/css?family=Droid+Sans:400,700"
rel="stylesheet" type="text/css">
      <link href="./Grid.css" rel="stylesheet"
type="text/css">

<script type="text/javascript"
          src="./Grid.js"></script>


<style type="text/css" media="screen">
/* Generic pane rules */
body { margin: 0 }
.row, .col { overflow: hidden; position: absolute; }
.row { left: 0; right: 0; }
.col { top: 0; bottom: 0; }
.scroll-x { overflow-x: auto; }
.scroll-y { overflow-y: auto; }

.header.row { height: 80px; top: 0; }
.body.row { top: 80px; bottom: 00px; }
.footerText.row { height: 75px; bottom: 400; }
.footer.row { height: 0px; bottom: 0; }
</style>

    </head>
    <body>
<div class="header row">
<h2>""" + name + """ Main report """ + extraHeader + """</h2>
</div>
<div id="testGrid" class="body row ">
  <div id="diffDiv">
    <div id="diffGrid" >
      <table id="diffTable">
        <colgroup><col id="diffTableCol1"></colgroup>
      </table>
    </div>
  </div>
</div>


      <script type="text/javascript">

        (function(window, document, undefined) {
          "use strict";

          var gridColSortTypes = ["string", "number", "number", "number",
"number", "number", "number", "number", "number", "number",
"number", "number", "number"
, "string", "string", "string"],
              gridColAlign = [];

          var onColumnSort = function(newIndexOrder, columnIndex,
lastColumnIndex)
   {
            var doc = document;
            if (columnIndex !== lastColumnIndex) {
              if (lastColumnIndex > -1) {
                doc.getElementById("diffHdr" + (lastColumnIndex -
1)).parentNode.style.backgroundColor = "";
              }
              doc.getElementById("diffHdr" + (columnIndex -
1)).parentNode.style.backgroundColor = "#f7f7f7";
            }
          };

          var onResizeGrid = function(newWidth, newHeight) {
            var diffDivStyle = document.getElementById("diffDiv").style;
            diffDivStyle.width = newWidth + "px";
            diffDivStyle.height = newHeight + "px";
          };

          for (var i=0, col; col=gridColSortTypes[i]; i++) {
            gridColAlign[i] = (col === "number") ? "right" : "left";
          }
                    """.replace("\r", "").stripMargin

  private def htmlFooter(fixedColumns: Int = 1) =
    """
    var testGrid = new Grid("testGrid", {
    srcType : "json",
    srcData : gridData,
    allowGridResize : false,
    allowColumnResize : true,
    allowClientSideSorting : false,
    allowSelections : true,
    allowMultipleSelections : true,
    showSelectionColumn : false,
    onColumnSort : onColumnSort,
    onResizeGrid : onResizeGrid,
    colAlign : gridColAlign,
    colBGColors : ["#fafafa"],
    colSortTypes : gridColSortTypes,
    fixedCols : """ + fixedColumns + """
});
})(this, this.document);
</script>
</body>
</html>

    """.stripMargin

  private val cssText = """
                  |/*
                  | Grid
                  | MIT-style license. Copyright 2012 Matt V. Murphy
                  | Hacked around by Sue: original here https://github.com/mmurph211/Grid
                  |*/
                  |.g_Base {
                  |    /* Base grid container */
                  |    position : relative;
                  |    padding : 0px;
                  |    width : 100%;
                  |    height : 100%;
                  |    line-height : 100%;
                  |    font-size : 12px;
                  |    background-color : #fff;
                  |    white-space : nowrap;
                  |    overflow : hidden;
                  |    cursor : default;
                  |    direction : ltr;
                  |    -webkit-tap-highlight-color : transparent;
                  |}
                  |.g_BaseResize {
                  |    /* Used as control for resizing the grid */
                  |    position : absolute;
                  |    z-index : 5;
                  |    bottom : 0px;
                  |    right : 0px;
                  |    background-color : #eee;
                  |    cursor : nw-resize;
                  |    -webkit-user-select : none;
                  |}
                  |.g_BaseResize:hover {
                  |    background-color : #ccc;
                  |}
                  |.g_RS {
                  |    /* Used as control for resizing a grid column */
                  |    display : none;
                  |    width : 4px;
                  |    cursor : e-resize;
                  |    -webkit-user-select : none;
                  |}
                  |.g_ResizeDragger {
                  |    /* Displays as vertical ruler when resizing a column */
                  |    position : absolute;
                  |    z-index : 5;
                  |    top : 0px;
                  |    width : 3px;
                  |    background-color : #ccc;
                  |}
                  |.g_EmptySetMsg {
                  |    /* Displayed when no rows are rendered in the grid body */
                  |    padding : 10px;
                  |    font-style : italic;
                  |}
                  |.g_Head {
                  |    /* Base header container */
                  |    position : absolute;
                  |    z-index : 2;
                  |    top : 0px;
                  |    left : 0px;
                  |    overflow : hidden;
                  |}
                  |.g_Body {
                  |    /* Base body container */
                  |    width : 100%;
                  |    height : 100%;
                  |    overflow : scroll;
                  |}
                  |.g_Foot {
                  |    /* Base footer container */
                  |    position : absolute;
                  |    z-index : 2;
                  |    left : 0px;
                  |    overflow : hidden;
                  |}
                  |.g_HeadFixed {
                  |    /* Base header fixed container if fixedCols > 0 */
                  |    position : absolute;
                  |    z-index : 3;
                  |    top : 0px;
                  |    left : 0px;
                  |    overflow : hidden;
                  |}
                  |.g_BodyFixed {
                  |    /* Base body fixed container if fixedCols > 0 */
                  |    position : absolute;
                  |    *position : static;
                  |    z-index : 1;
                  |    top : 0px;
                  |    left : 0px;
                  |    overflow : hidden;
                  |}
                  |.g_BodyFixed2 {
                  |    /* Inner base body fixed container if fixedCols > 0. Used for IE7 support */
                  |    *position : absolute;
                  |    *z-index : 1;
                  |    *top : 0px;
                  |    *left : 0px;
                  |    *overflow : hidden;
                  |}
                  |.g_FootFixed {
                  |    /* Base footer fixed container if fixedCols > 0 */
                  |    position : absolute;
                  |    z-index : 3;
                  |    bottom : 0px;
                  |    left : 0px;
                  |    overflow : hidden;
                  |}
                  |.g_Cl {
                  |    /* Grid column container */
                  |    display : inline-block;
                  |    *display : inline;
                  |    zoom : 1;
                  |    vertical-align : top;
                  |    overflow : hidden;
                  |}
                  |.g_HR {
                  |    /* Grid header cell */
                  |    padding : 4px 12px 4px 6px !important;
                  |    border-width : 0px 1px 1px 0px;
                  |    border-color : #ccc !important;
                  |    zoom : 1;
                  |    background-color : #eee;
                  |    background-position : 0px 0px;
                  |    background-repeat: repeat-x;
                  |    font-weight : bold;
                  |    color : #333 !important;
                  |    -webkit-user-select : none;
                  |    -moz-user-select : none;
                  |    -ms-user-select : none;
                  |    -o-user-select : none;
                  |    user-select : none;
                  |}
                  |.g_BR {
                  |    /* Grid body cell */
                  |    border-width : 0px 0px 1px 0px;
                  |}
                  |.g_FR {
                  |    /* Grid footer cell */
                  |    border-width : 1px 1px 0px 0px;
                  |    border-color : #ccc !important;
                  |    background-color : #eee;
                  |    background-position : 0px -19px;
                  |    background-repeat: repeat-x;
                  |}
                  |.g_C {
                  |    /* Grid cell (all) */
                  |    padding : 6px 12px 6px 6px;
                  |    border-color : #eee;
                  |    border-style : solid;
                  |    color : #333;
                  |    height : 1.5em;
                  |    line-height : 1.5em;
                  |    vertical-align : top;
                  |    white-space : nowrap;
                  |    visibility : hidden;
                  |    cursor : default;
                  |    overflow : hidden;
                  |}
                  |.g_SH {
                  |    /* Label container for checkbox / radio selection element */
                  |    display : inline-block;
                  |    *display : inline;
                  |    zoom : 1;
                  |    width : 15px;
                  |}
                  |.g_Cb, .g_Rd {
                  |    /* Checkbox and radio selection elements */
                  |    margin : -1px 0px 0px 0px;
                  |    padding : 0px;
                  |    width : 15px;
                  |    height : 15px;
                  |    max-height : 1.0em;
                  |    vertical-align : middle;
                  |    overflow : hidden;
                  |}
                  |.g_Rd {
                  |    margin-top : -2px;
                  |}
                  |@media print {
                  |    /* Print overrides */
                  |    .g_Base, .g_Head, .g_Body, .g_Foot { overflow : visible; }
                  |    .g_HeadStatic, .g_FootStatic { margin-left : 0px !important; }
                  |    .g_BodyFixed2 { margin-top : 0px !important; }
                  |}
                """.stripMargin
  private val gridJS =
    """
      |////////////////////////////////////
      |//
      |// Grid
      |// MIT-style license. Copyright 2012 Matt V. Murphy
      |// Hacked around by Sue: original here https://github.com/mmurph211/Grid
      |//
      |////////////////////////////////////
      |(function(window, document, undefined) {
      |	"use strict";
      |
      |	var GridProto;
      |	var Grid = function(element, options) {
      |		if ((this.element = (typeof(element) === "string") ? $(element) : element)) {
      |			this.css = { idRulePrefix : "#" + this.element.id + " ", sheet : null, rules : {} };
      |			this.columns = 0;
      |			this.columnWidths = [];
      |			this.cellData = { head : [], body : [], foot : [] };
      |			this.alignTimer = null;
      |			this.rawData = [];
      |			this.sortCache = {};
      |			this.lastSortedColumn = [-1, null];
      |			this.selectedIndexes = [];
      |			this.usesTouch = (window.ontouchstart !== undefined);
      |			this.startEvt = (this.usesTouch) ? "touchstart" : "mousedown";
      |			this.moveEvt = (this.usesTouch) ? "touchmove" : "mousemove";
      |			this.endEvt = (this.usesTouch) ? "touchend" : "mouseup";
      |			this.setOptions(options);
      |			this.init();
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	(GridProto = Grid.prototype).nothing = function(){};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.setOptions = function(options) {
      |		var hasOwnProp = Object.prototype.hasOwnProperty,
      |		    option;
      |
      |		this.options = {
      |			srcType : "", // "dom", "json", "xml"
      |			srcData : "",
      |			allowGridResize : false,
      |			allowColumnResize : false,
      |			allowClientSideSorting : false,
      |			allowSelections : false,
      |			allowMultipleSelections : false,
      |			showSelectionColumn : false,
      |			onColumnSort : this.nothing,
      |			onResizeGrid : this.nothing,
      |			onResizeGridEnd : this.nothing,
      |			onResizeColumn : this.nothing,
      |			onResizeColumnEnd : this.nothing,
      |			onRowSelect : this.nothing,
      |			onLoad : this.nothing,
      |			supportMultipleGridsInView : false,
      |			fixedCols : 0,
      |			selectedBgColor : "#eaf1f7",
      |			fixedSelectedBgColor : "#dce7f0",
      |			colAlign : [], // "left", "center", "right"
      |			colBGColors : [],
      |			colSortTypes : [], // "string", "number", "date", "custom", "none"
      |			customSortCleaner : null
      |		};
      |
      |		if (options) {
      |			for (option in this.options) {
      |				if (hasOwnProp.call(this.options, option) && options[option] !== undefined) {
      |					this.options[option] = options[option];
      |				}
      |			}
      |		}
      |
      |		this.options.allowColumnResize = this.options.allowColumnResize && !this.usesTouch;
      |		this.options.allowMultipleSelections = this.options.allowMultipleSelections && this.options.allowSelections;
      |		this.options.showSelectionColumn = this.options.showSelectionColumn && this.options.allowSelections;
      |		this.options.fixedCols = (!this.usesTouch) ? this.options.fixedCols : 0;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.init = function() {
      |		var srcType = this.options.srcType,
      |		    srcData = this.options.srcData,
      |		    data;
      |
      |		this.generateSkeleton();
      |		this.addEvents();
      |
      |		// DOM:
      |		if (srcType === "dom" && (srcData = (typeof(srcData) === "string") ? $(srcData) : srcData)) {
      |			this.convertData(this.convertDomDataToJsonData(srcData));
      |
      |		// JSON:
      |		} else if (srcType === "json" && (data = parseJSON(srcData))) {
      |			this.convertData(data);
      |
      |		// XML:
      |		} else if (srcType === "xml" && (data = parseXML(srcData))) {
      |			this.convertData(this.convertXmlDataToJsonData(data));
      |		}
      |
      |		this.generateGrid();
      |		this.displayGrid();
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.generateSkeleton = function() {
      |		var doc = document,
      |		    elems = [["base", "g_Base", "docFrag"],
      |		             ["head", "g_Head", "base"],
      |		             ["headFixed", "g_HeadFixed", "head"],
      |		             ["headStatic", "g_HeadStatic", "head"],
      |		             ["foot", "g_Foot", "base"],
      |		             ["footFixed", "g_FootFixed", "foot"],
      |		             ["footStatic", "g_FootStatic", "foot"],
      |		             ["body", "g_Body", "base"],
      |		             ["bodyFixed", "g_BodyFixed", "body"],
      |		             ["bodyFixed2", "g_BodyFixed2", "bodyFixed"],
      |		             ["bodyStatic", "g_BodyStatic", "body"]];
      |
      |		this.parentDimensions = { x : this.element.offsetWidth, y : this.element.offsetHeight };
      |		this.docFrag = doc.createDocumentFragment();
      |		for (var i=0, elem; elem=elems[i]; i++) {
      |			(this[elem[0]] = doc.createElement("DIV")).className = elem[1];
      |			this[elem[2]].appendChild(this[elem[0]]);
      |		}
      |
      |		if (this.options.allowGridResize) {
      |			(this.baseResize = doc.createElement("DIV")).className = "g_BaseResize";
      |			this.base.appendChild(this.baseResize);
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.addEvents = function() {
      |		var wheelEvent;
      |
      |		// Simulate mouse scrolling over non-scrollable content:
      |		if (this.options.fixedCols > 0 && !this.usesTouch && !msie) {
      |			try {
      |				wheelEvent = (WheelEvent("wheel")) ? "wheel" : undefined;
      |			} catch (e) {
      |				wheelEvent = (document.onmousewheel !== undefined) ? "mousewheel" : "DOMMouseScroll";
      |			}
      |			if (wheelEvent) {
      |				addEvent(this.bodyFixed, wheelEvent, bind(this.simulateMouseScroll, this));
      |			}
      |		}
      |
      |		// Grid resizing:
      |		if (this.options.allowGridResize) {
      |			addEvent(this.baseResize, this.startEvt, bind(this.initResizeGrid, this));
      |		}
      |
      |		// Column resizing and client side sorting:
      |		if (this.options.allowColumnResize || this.options.allowClientSideSorting) {
      |			addEvent(this.head, this.startEvt, bind(this.delegateHeaderEvent, this));
      |		}
      |
      |		// Row selection:
      |		if (this.options.allowSelections) {
      |			addEvent(this.body, this.startEvt, bind(this.selectRange, this));
      |			if (this.options.showSelectionColumn) {
      |				addEvent(this.body, "click", bind(this.preventSelectionInputStateChange, this));
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.convertDomDataToJsonData = function(data) {
      |		var sections = { "thead" : "Head", "tbody" : "Body", "tfoot" : "Foot" },
      |		    section, rows, row, cells, arr, arr2, i, j, k,
      |		    json = {};
      |
      |		// Cycle through all table rows, change sections when needed:
      |		if (((data || {}).tagName || "").toLowerCase() === "table") {
      |			for (i=0, j=0, rows=data.rows; row=rows[i]; i++) {
      |				if (row.sectionRowIndex === 0 && (section = sections[row.parentNode.tagName.toLowerCase()])) {
      |					json[section] = arr = (json[section] || []);
      |					j = arr.length;
      |				}
      |				arr[j++] = arr2 = [];
      |				k = (cells = row.cells).length;
      |				while (k) { arr2[--k] = cells[k].innerHTML; }
      |			}
      |		}
      |
      |		return json;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.convertXmlDataToJsonData = function(data) {
      |		var sections = { "thead" : "Head", "tbody" : "Body", "tfoot" : "Foot" },
      |		    cellText = (msie < 9) ? "text" : "textContent",
      |		    nodes, node, section, rows, row, cells, cell, tag, n, i, j,
      |		    arr, arr2, a, a2,
      |		    json = {};
      |
      |		// By section:
      |		if ((nodes = (data.getElementsByTagName("table")[0] || {}).childNodes)) {
      |			for (n=0; node=nodes[n]; n++) {
      |				if ((section = sections[node.nodeName]) && (rows = node.childNodes)) {
      |					json[section] = arr = (json[section] || []);
      |					a = arr.length;
      |
      |					// By row:
      |					for (i=0; row=rows[i]; i++) {
      |						if (row.nodeName === "tr" && (cells = row.childNodes)) {
      |							arr[a++] = arr2 = [];
      |							a2 = 0;
      |
      |							// By cell:
      |							for (j=0; cell=cells[j]; j++) {
      |								if ((tag = cell.nodeName) === "td" || tag === "th") {
      |									arr2[a2++] = cell[cellText] || "";
      |								}
      |							}
      |						}
      |					}
      |				}
      |			}
      |		}
      |
      |		return json;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.convertData = function(data) {
      |		var base, cols, h, b, f;
      |
      |		this.addSelectionColumn(data);
      |		this.rawData = data.Body || [];
      |		if ((base = data.Head || data.Body || data.Foot || null)) {
      |			cols = this.columns = base[0].length;
      |			h = this.cellData.head;
      |			b = this.cellData.body;
      |			f = this.cellData.foot;
      |			while (cols) { h[--cols] = []; b[cols] = []; f[cols] = []; }
      |
      |			cols = this.columns;
      |			if (data.Head) {
      |				this.convertDataItem(h, data.Head, "<DIV class='g_C g_HR g_R", cols, this.options.allowColumnResize);
      |			} else {
      |				this.css.rules[".g_Head"] = { display : "none" };
      |			}
      |			if (data.Body) {
      |				this.convertDataItem(b, data.Body, "<DIV class='g_C g_BR g_R", cols, false);
      |			} else {
      |				this.css.rules[".g_BodyFixed"] = { display : "none" };
      |			}
      |			if (data.Foot) {
      |				this.convertDataItem(f, data.Foot, "<DIV class='g_C g_FR g_R", cols, false);
      |			} else {
      |				this.css.rules[".g_Foot"] = { display : "none" };
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.convertDataItem = function(arr, rows, rowClass, cols, allowColResize) {
      |		var rowIdx = rows.length,
      |		    rowDiv, row, colIdx;
      |
      |		while (rowIdx) {
      |			rowDiv = rowClass + (--rowIdx) + "'>";
      |			row = rows[rowIdx];
      |			colIdx = cols;
      |			while (colIdx) {
      |				arr[--colIdx][rowIdx] = rowDiv + (row[colIdx] || "&nbsp;");
      |			}
      |		}
      |		if (allowColResize && (rowIdx = rows.length)) {
      |			colIdx = cols;
      |			while (colIdx) {
      |				arr[--colIdx][0] = ("<SPAN class='g_RS g_RS" + colIdx + "'>&nbsp;</SPAN>") + arr[colIdx][0];
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.addSelectionColumn = function(data) {
      |		var html, rows, i;
      |
      |		if (this.options.showSelectionColumn) {
      |			this.options.colBGColors.unshift(this.options.colBGColors[0] || "");
      |			this.options.colSortTypes.unshift("none");
      |			this.options.colAlign.unshift("left");
      |			if (!this.usesTouch) {
      |				this.options.fixedCols++;
      |			}
      |
      |			if ((rows = data.Head) && (i = rows.length)) {
      |				while (i) { rows[--i].unshift(""); }
      |			}
      |			if ((rows = data.Body) && (i = rows.length)) {
      |				html = "<LABEL class=g_SH><INPUT tabIndex='-1' type=";
      |				html += ((this.options.allowMultipleSelections) ? "checkbox class=g_Cb" : "radio  class=g_Rd");
      |				html += ">&nbsp;</LABEL>";
      |				while (i) { rows[--i].unshift(html); }
      |			}
      |			if ((rows = data.Foot) && (i = rows.length)) {
      |				while (i) { rows[--i].unshift(""); }
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.generateGrid = function() {
      |		this.hasHead = ((this.cellData.head[0] || []).length > 0);
      |		this.hasBody = ((this.cellData.body[0] || []).length > 0);
      |		this.hasFoot = ((this.cellData.foot[0] || []).length > 0);
      |		this.hasHeadOrFoot = (this.hasHead || this.hasFoot);
      |		this.hasFixedCols = (this.options.fixedCols > 0);
      |
      |		this.generateGridHead();
      |		this.generateGridBody();
      |		this.generateGridFoot();
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.generateGridHead = function() {
      |		var hHTML;
      |
      |		if (this.hasHead) {
      |			hHTML = this.generateGridSection(this.cellData.head);
      |			this.headStatic.innerHTML = hHTML.fullHTML;
      |			if (this.hasFixedCols) {
      |				this.headFixed.innerHTML = hHTML.fixedHTML;
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.generateGridBody = function() {
      |		var bHTML;
      |
      |		if (this.hasBody) {
      |			bHTML = this.generateGridSection(this.cellData.body);
      |			this.bodyStatic.innerHTML = bHTML.fullHTML;
      |			if (this.hasFixedCols) {
      |				this.bodyFixed2.innerHTML = bHTML.fixedHTML;
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.generateGridFoot = function() {
      |		var fHTML;
      |
      |		if (this.hasFoot) {
      |			fHTML = this.generateGridSection(this.cellData.foot);
      |			this.footStatic.innerHTML = fHTML.fullHTML;
      |			if (this.hasFixedCols) {
      |				this.footFixed.innerHTML = fHTML.fixedHTML;
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.generateGridSection = function(cols) {
      |		var replaceFunc = function($1, $2) { return cols[parseInt($2, 10)].join("</DIV>"); },
      |		    replaceRgx = /@(\d+)@/g,
      |		    fixedCols = this.options.fixedCols,
      |		    fHtml = [], sHtml = [],
      |		    colIdx = cols.length;
      |
      |		while (colIdx) {
      |			if ((--colIdx) < fixedCols) {
      |				fHtml[colIdx] = "<DIV class='g_Cl g_Cl" + colIdx + " g_FCl'>@" + colIdx + "@</DIV></DIV>";
      |				sHtml[colIdx] = "<DIV class='g_Cl g_Cl" + colIdx + " g_FCl'></DIV>";
      |			} else {
      |				sHtml[colIdx] = "<DIV class='g_Cl g_Cl" + colIdx + "'>@" + colIdx + "@</DIV></DIV>";
      |			}
      |		}
      |
      |		return { fixedHTML : (fixedCols) ? fHtml.join("").replace(replaceRgx, replaceFunc) : "",
      |		         fullHTML : sHtml.join("").replace(replaceRgx, replaceFunc) };
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.displayGrid = function() {
      |		var srcType = this.options.srcType,
      |		    srcData = this.options.srcData,
      |		    replace = false;
      |
      |		// Setup scrolling:
      |		this.lastScrollLeft = 0;
      |		this.lastScrollTop = 0;
      |		this.body.onscroll = bind(this.syncScrolls, this);
      |
      |		// Prep style element:
      |		try {
      |			this.css.sheet.parentNode.removeChild(this.css.sheet);
      |		} catch (e) {
      |			(this.css.sheet = document.createElement("STYLE")).id = this.element.id + "SS";
      |			this.css.sheet.type = "text/css";
      |		}
      |
      |		// Insert grid into DOM:
      |		if (srcType === "dom" && (srcData = (typeof(srcData) === "string") ? $(srcData) : srcData)) {
      |			if ((replace = (this.element === srcData.parentNode))) {
      |				this.element.replaceChild(this.docFrag, srcData);
      |			}
      |		}
      |		if (!replace) {
      |			this.element.appendChild(this.docFrag);
      |		}
      |
      |		// Align columns:
      |		this.alignTimer = window.setTimeout(bind(this.alignColumns, this, false, true), 16);
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.alignColumns = function(reAlign, fromInit) {
      |		var sNodes = [this.headStatic.children || [], this.bodyStatic.children || [], this.footStatic.children || []],
      |		    fNodes = [this.headFixed.children || [], this.bodyFixed2.children || [], this.footFixed.children || []],
      |		    allowColumnResize = this.options.allowColumnResize,
      |		    colBGColors = this.options.colBGColors,
      |		    colAlign = this.options.colAlign,
      |		    fixedCols = this.options.fixedCols,
      |		    rules = this.css.rules,
      |		    colWidth, nodes;
      |
      |		// Compute base styles first, or remove old column width styling if realigning the columns:
      |		if (reAlign !== true) {
      |			this.computeBaseStyles();
      |		} else {
      |			for (var i=0, len=this.columns; i<len; i++) {
      |				rules[".g_Cl" + i].width = "auto";
      |			}
      |			this.setRules();
      |		}
      |
      |		// Compute column width, alignment and background styles:
      |		this.columnWidths = [];
      |		for (var i=0, len=this.columns; i<len; i++) {
      |			nodes = (i < fixedCols) ? fNodes : sNodes;
      |			colWidth = Math.max((nodes[0][i] || {}).offsetWidth || 0,
      |			                    (nodes[1][i] || {}).offsetWidth || 0,
      |			                    (nodes[2][i] || {}).offsetWidth || 0);
      |
      |			this.columnWidths[i] = colWidth;
      |			rules[".g_Cl" + i] = { "width" : colWidth + "px", "text-align" : (colAlign[i] || "left") };
      |			if ((colBGColors[i] || "#ffffff") !== "#ffffff") {
      |				rules[".g_Cl" + i]["background-color"] = colBGColors[i];
      |			}
      |			if (allowColumnResize) {
      |				rules[".g_RS" + i] = { "margin-left" : (colWidth - 2) + "px" };
      |			}
      |		}
      |		this.setRules();
      |		if (fromInit === true) {
      |			this.options.onLoad.call(this);
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.computeBaseStyles = function() {
      |		var rules = this.css.rules,
      |		    headHeight = (this.hasHead) ? this.head.offsetHeight : 0,
      |		    footHeight = (this.hasFoot) ? this.foot.offsetHeight : 0,
      |		    sBarSize = { "x" : this.body.offsetWidth - this.body.clientWidth,
      |		                 "y" : this.body.offsetHeight - this.body.clientHeight };
      |
      |		rules[".g_C"] = { "visibility" : "visible" };
      |		rules[".g_Cl"] = { "background-color" : "#fff" };
      |		rules[".g_BodyStatic"] = { "padding" : headHeight + "px 0px " + footHeight + "px 0px" };
      |		if (this.hasHead) {
      |			rules[".g_Head"] = { "right" : sBarSize.x + "px" };
      |		}
      |		if (this.hasFoot) {
      |			rules[".g_Foot"] = { "bottom" : sBarSize.y + "px", "right" : sBarSize.x + "px" };
      |		}
      |		if (this.hasFixedCols) {
      |			rules[".g_BodyFixed" + ((msie < 8) ? "2" : "")] = { "top" : headHeight + "px", "bottom" : sBarSize.y + "px" };
      |		}
      |		if (this.options.allowGridResize) {
      |			rules[".g_BaseResize"] = { "width" : sBarSize.x + "px", "height" : sBarSize.y + "px" };
      |		}
      |		if (this.options.allowColumnResize) {
      |			rules[".g_ResizeDragger"] = { "bottom" : sBarSize.y + "px" };
      |			rules[".g_RS"] = { "display" : "block",
      |			                   "position" : "relative",
      |			                   "margin-bottom" : (headHeight * -1) + "px",
      |			                   "height" : headHeight + "px" };
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.syncScrolls = function(event) {
      |		var sL = (this.hasHeadOrFoot) ? this.body.scrollLeft : 0,
      |		    sT = (this.hasFixedCols) ? this.body.scrollTop : 0;
      |
      |		if (sL !== this.lastScrollLeft) {
      |			this.lastScrollLeft = sL;
      |			if (this.hasHead) {
      |				this.headStatic.style.marginLeft = (-1 * sL) + "px";
      |			}
      |			if (this.hasFoot) {
      |				this.footStatic.style.marginLeft = (-1 * sL) + "px";
      |			}
      |		}
      |		if (sT !== this.lastScrollTop) {
      |			this.lastScrollTop = sT;
      |			this.bodyFixed2.style.marginTop = (-1 * sT) + "px";
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.simulateMouseScroll = function(event) {
      |		var event = event || window.event,
      |		    deltaY = 0;
      |
      |		if (event.deltaY !== undefined) {
      |			deltaY = event.deltaY;
      |		} else if (event.wheelDelta !== undefined) {
      |			deltaY = event.wheelDelta * (-1/40);
      |		} else if (event.detail !== undefined) {
      |			deltaY = event.detail;
      |		}
      |
      |		this.body.scrollTop += (deltaY * 33);
      |		this.syncScrolls();
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.setRules = function() {
      |		var idRulePrefix = (this.options.supportMultipleGridsInView) ? this.css.idRulePrefix : "",
      |		    hasOwnProp = Object.prototype.hasOwnProperty,
      |		    rules = this.css.rules,
      |		    sheet = this.css.sheet,
      |		    cssText = [], c = 0,
      |		    rule, props, prop,
      |		    doc = document;
      |
      |		for (rule in rules) {
      |			if (hasOwnProp.call(rules, rule) && (props = rules[rule])) {
      |				cssText[c++] = idRulePrefix + rule + "{";
      |				for (prop in props) {
      |					if (hasOwnProp.call(props, prop)) {
      |						cssText[c++] = prop + ":" + props[prop] + ";";
      |					}
      |				}
      |				cssText[c++] = "} ";
      |			}
      |		}
      |
      |		if (!sheet.styleSheet) {
      |			sheet.appendChild(doc.createTextNode(cssText.join("")));
      |		}
      |		if (!$(sheet.id)) {
      |			(doc.head || doc.getElementsByTagName("head")[0]).appendChild(sheet);
      |		}
      |		if (sheet.styleSheet) {
      |			sheet.styleSheet.cssText = cssText.join("");
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.initResizeGrid = function(event) {
      |		var event = event || window.event,
      |		    pagePos;
      |
      |		if (event.button !== 2 && this.options.allowGridResize) {
      |			pagePos = getEventPositions(event, "page");
      |
      |			this.tmp = {
      |				throttle : -1,
      |				origX : pagePos.x,
      |				origY : pagePos.y,
      |				origWidth : this.parentDimensions.x,
      |				origHeight : this.parentDimensions.y,
      |				boundMoveEvt : bind(this.resizeGrid, this),
      |				boundEndEvt : bind(this.endResizeGrid, this)
      |			};
      |
      |			addEvent(document, this.moveEvt, this.tmp.boundMoveEvt);
      |			addEvent(document, this.endEvt, this.tmp.boundEndEvt);
      |			return stopEvent(event);
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.resizeGrid = function(event) {
      |		var pagePos, xDif, yDif, newWidth, newHeight, elemStyle;
      |
      |		if ((this.tmp.throttle++) & 1) {
      |			pagePos = getEventPositions(event || window.event, "page");
      |			xDif = pagePos.x - this.tmp.origX;
      |			yDif = pagePos.y - this.tmp.origY;
      |			newWidth = Math.max(60, (xDif > 0) ? this.tmp.origWidth + xDif : this.tmp.origWidth - Math.abs(xDif));
      |			newHeight = Math.max(30, (yDif > 0) ? this.tmp.origHeight + yDif : this.tmp.origHeight - Math.abs(yDif));
      |
      |			elemStyle = this.element.style;
      |			elemStyle.width = newWidth + "px";
      |			elemStyle.height = newHeight + "px";
      |
      |			this.parentDimensions = { x : newWidth, y : newHeight };
      |			this.syncScrolls();
      |			clearTextSelections();
      |			this.options.onResizeGrid.apply(this, [newWidth, newHeight]);
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.endResizeGrid = function(event) {
      |		removeEvent(document, this.moveEvt, this.tmp.boundMoveEvt);
      |		removeEvent(document, this.endEvt, this.tmp.boundEndEvt);
      |		this.options.onResizeGridEnd.apply(this, [this.parentDimensions.x, this.parentDimensions.y]);
      |		this.tmp = undefined;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.delegateHeaderEvent = function(event) {
      |		var event = event || window.event,
      |		    target = event.target || event.srcElement,
      |		    targetClass = target.className || "";
      |
      |		if (event.button !== 2) {
      |			if (this.options.allowColumnResize && targetClass.indexOf("g_RS") > -1) {
      |				return this.initResizeColumn(event, target, targetClass);
      |			} else if (this.hasBody && this.options.allowClientSideSorting) {
      |				while (targetClass.indexOf("g_Cl") === -1 && targetClass !== "g_Head") {
      |					targetClass = (target = target.parentNode).className || "";
      |				}
      |				if (targetClass.indexOf("g_Cl") > -1) {
      |					this.sortColumn(parseInt(/g_Cl(\d+)/.exec(targetClass)[1], 10));
      |				}
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.initResizeColumn = function(event, target, targetClass) {
      |		var colIdx = parseInt(targetClass.replace(/g_RS/g, ""), 10),
      |		    doc = document;
      |
      |		this.tmp = {
      |			lastLeft : -1,
      |			colIdx : colIdx,
      |			origX : getEventPositions(event, "client").x,
      |			origWidth : this.columnWidths[colIdx],
      |			origLeft : target.offsetLeft,
      |			boundMoveEvt : bind(this.resizeColumn, this),
      |			boundEndEvt : bind(this.endResizeColumn, this),
      |			dragger : doc.createElement("DIV")
      |		};
      |
      |		this.tmp.dragger.className = "g_ResizeDragger";
      |		this.tmp.dragger.style.left = this.tmp.origLeft + "px";
      |		this.base.insertBefore(this.tmp.dragger, this.base.firstChild);
      |
      |		addEvent(doc, this.moveEvt, this.tmp.boundMoveEvt);
      |		addEvent(doc, this.endEvt, this.tmp.boundEndEvt);
      |		return stopEvent(event);
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.resizeColumn = function(event) {
      |		var clientX = getEventPositions(event || window.event, "client").x,
      |		    xDif = clientX - this.tmp.origX,
      |		    newWidth = Math.max(15, (xDif > 0) ? this.tmp.origWidth + xDif : this.tmp.origWidth - Math.abs(xDif)),
      |		    newLeft = (xDif > 0) ? this.tmp.origLeft + xDif : this.tmp.origLeft - Math.abs(xDif);
      |
      |		this.tmp.newWidth = newWidth;
      |		if (this.tmp.lastLeft !== newLeft && newWidth > 15) {
      |			this.tmp.dragger.style.left = newLeft + "px";
      |			this.tmp.lastLeft = newLeft;
      |		}
      |
      |		clearTextSelections();
      |		this.options.onResizeColumn.apply(this, [this.tmp.colIdx, newWidth]);
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.endResizeColumn = function(event) {
      |		var newWidth = this.tmp.newWidth || this.tmp.origWidth,
      |		    colIdx = this.tmp.colIdx;
      |
      |		removeEvent(document, this.moveEvt, this.tmp.boundMoveEvt);
      |		removeEvent(document, this.endEvt, this.tmp.boundEndEvt);
      |
      |		this.tmp.dragger.parentNode.removeChild(this.tmp.dragger);
      |		this.css.rules[".g_Cl" + colIdx]["width"] = newWidth + "px";
      |		this.css.rules[".g_RS" + colIdx]["margin-left"] = (newWidth - 2) + "px";
      |		this.columnWidths[colIdx] = newWidth;
      |		this.setRules();
      |		this.syncScrolls();
      |		this.options.onResizeColumnEnd.apply(this, [colIdx, newWidth]);
      |		this.tmp = undefined;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.sortColumn = function(colIdx, sortAsc) {
      |		var colIdx = parseInt(colIdx, 10) || ((colIdx === 0) ? 0 : -1),
      |		    colSortAs = (colIdx > -1) ? this.options.colSortTypes[colIdx] || "string" : "none",
      |		    lastCol = this.lastSortedColumn;
      |
      |		if (colSortAs !== "none") {
      |			sortAsc = (sortAsc === undefined) ? ((colIdx === lastCol[0]) ? !lastCol[1] : true) : !!sortAsc;
      |			this.sortRawData(colIdx, colSortAs, sortAsc);
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.sortRawData = function(colIdx, colSortAs, sortAsc) {
      |		var selIndexes, ltVal, gtVal, i,
      |		    rawData = this.rawData,
      |		    newSelIndexes = [],
      |		    newIdxOrder = [],
      |		    that = this;
      |
      |		// Store prior index order:
      |		i = rawData.length;
      |		while (i) { rawData[--i].pIdx = i; }
      |
      |		// Sort the body data by type:
      |		ltVal = (sortAsc) ? -1 : 1;
      |		gtVal = (sortAsc) ? 1 : -1;
      |		rawData.sort(function(a, b) {
      |			return that.getSortResult(colSortAs, colIdx, ltVal, gtVal, a[colIdx], b[colIdx]);
      |		});
      |
      |		// Update the grid body HTML:
      |		this.convertDataItem(this.cellData.body, rawData, "<DIV class='g_C g_BR g_R", this.columns, false);
      |		this.generateGridBody();
      |
      |		// Generate new sort order array:
      |		i = rawData.length;
      |		while (i) { newIdxOrder[--i] = rawData[i].pIdx; }
      |
      |		// Update selected row indexes if applicable:
      |		if (this.options.allowSelections && (selIndexes = this.selectedIndexes.concat()).length) {
      |			i = selIndexes.length;
      |			while (i) { newSelIndexes[--i] = indexOf(newIdxOrder, selIndexes[i]); }
      |			this.highlightRows((this.selectedIndexes = newSelIndexes), []);
      |		}
      |
      |		// Fire sort event:
      |		this.options.onColumnSort.apply(this, [newIdxOrder.concat(), colIdx, this.lastSortedColumn[0]]);
      |		this.lastSortedColumn = [colIdx, sortAsc];
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.getSortResult = function(type, colIdx, ltVal, gtVal, a, b, keyA, keyB) {
      |		if (a === b) {
      |			return 0;
      |		}
      |
      |		if (this.sortCache[(keyA = type + "_" + a)] === undefined) {
      |			this.sortCache[keyA] = (type === "string") ? a :
      |			                       (type === "number") ? parseFloat(a) || -Infinity :
      |			                       (type === "date") ? new Date(a).getTime() || -Infinity :
      |			                       (type === "custom") ? this.options.customSortCleaner(a, colIdx) : a;
      |		}
      |		if (this.sortCache[(keyB = type + "_" + b)] === undefined) {
      |			this.sortCache[keyB] = (type === "string") ? b :
      |			                       (type === "number") ? parseFloat(b) || -Infinity :
      |			                       (type === "date") ? new Date(b).getTime() || -Infinity :
      |			                       (type === "custom") ? this.options.customSortCleaner(b, colIdx) : b;
      |		}
      |
      |		return (this.sortCache[keyA] < this.sortCache[keyB]) ? ltVal : gtVal;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.toggleSelectAll = function(toggle) {
      |		var selIndexes = this.selectedIndexes,
      |		    toSelect = [], toRemove = [],
      |		    i;
      |
      |		if (this.hasBody && this.options.allowSelections) {
      |			if (toggle) {
      |				toSelect = [0];
      |				if (this.options.allowMultipleSelections) {
      |					i = this.rawData.length;
      |					while (i) { toSelect[--i] = i; }
      |				}
      |				this.selectIndexes(toSelect);
      |			} else if (selIndexes.length) {
      |				toRemove = selIndexes.concat();
      |				this.selectedIndexes = [];
      |				this.highlightRows(toSelect, toRemove);
      |				this.options.onRowSelect.apply(this, [toSelect, toRemove, -1]);
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.selectIndexes = function(rowIndexes) {
      |		var selIndexes = this.selectedIndexes,
      |		    toSelect = [], toRemove = [],
      |		    i = rowIndexes.length,
      |		    j = 0;
      |
      |		if (i && this.hasBody && this.options.allowSelections) {
      |			if (this.options.allowMultipleSelections) {
      |				while (i) {
      |					if (indexOf(selIndexes, rowIndexes[--i]) === -1) {
      |						toSelect[j++] = rowIndexes[i];
      |					}
      |				}
      |			} else {
      |				toRemove = selIndexes.concat();
      |				toSelect[0] = rowIndexes[0];
      |				selIndexes = [];
      |			}
      |
      |			this.selectedIndexes = selIndexes.concat(toSelect);
      |			this.highlightRows(toSelect, toRemove);
      |			this.options.onRowSelect.apply(this, [toSelect, toRemove, -1]);
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.selectRange = function(event) {
      |		var event = event || window.event,
      |		    target = event.target || event.srcElement,
      |		    targetClass, isSelCol, isCtrlKeyLike, update, rowIdx;
      |
      |		if (event.button !== 2 && this.options.allowSelections) {
      |			targetClass = target.className || "";
      |			while (targetClass.indexOf("g_BR") === -1 && targetClass !== "g_Body") {
      |				targetClass = (target = target.parentNode).className || "";
      |			}
      |			if (targetClass.indexOf("g_BR") > -1) {
      |				update = true;
      |				rowIdx = parseInt(/g_R(\d+)/.exec(targetClass)[1], 10);
      |				targetClass = (target = target.parentNode).className || "";
      |				isSelCol = (this.options.showSelectionColumn && (targetClass.indexOf("g_Cl0") > -1));
      |				isCtrlKeyLike = this.usesTouch || isSelCol;
      |
      |				if (this.usesTouch && this.options.showSelectionColumn && (update = isSelCol)) {
      |					stopEvent(event);
      |				}
      |				if (update) {
      |					this.updateSelectedIndexes(rowIdx, event.ctrlKey || isCtrlKeyLike, event.shiftKey);
      |				}
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.updateSelectedIndexes = function(rowIdx, ctrlPressed, shiftPressed) {
      |		var selIndexes = this.selectedIndexes.concat(),
      |		    rowIdxSelected = (indexOf(selIndexes, rowIdx) > -1),
      |		    toSelect = [], toRemove = [],
      |		    startIdx, i, j, len;
      |
      |		if (!this.options.allowMultipleSelections || !selIndexes.length || (!ctrlPressed && !shiftPressed)) {
      |			toSelect = (rowIdxSelected && selIndexes.length === 1) ? [] : [rowIdx];
      |			toRemove = selIndexes.concat();
      |		} else if (ctrlPressed) {
      |			toSelect = rowIdxSelected ? [] : [rowIdx];
      |			toRemove = rowIdxSelected ? [rowIdx] : [];
      |		} else if (shiftPressed) {
      |			if ((startIdx = selIndexes[0]) <= rowIdx) {
      |				for (i=startIdx + 1, j=0; i<=rowIdx; i++) {
      |					if (indexOf(selIndexes, i) === -1) { toSelect[j++] = i; }
      |				}
      |			} else {
      |				for (i=startIdx - 1, j=0; i>=rowIdx; i--) {
      |					if (indexOf(selIndexes, i) === -1) { toSelect[j++] = i; }
      |				}
      |			}
      |		}
      |
      |		for (i=0, len=toRemove.length; i<len; i++) {
      |			if ((j = indexOf(selIndexes, toRemove[i])) > -1) { selIndexes.splice(j, 1); }
      |		}
      |		this.selectedIndexes = selIndexes.concat(toSelect);
      |		this.highlightRows(toSelect, toRemove);
      |		if (ctrlPressed || shiftPressed) {
      |			(!msie) ? clearTextSelections() : window.setTimeout(clearTextSelections, 25);
      |		}
      |		this.options.onRowSelect.apply(this, [toSelect, toRemove, rowIdx]);
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.highlightRows = function(toSelect, toRemove) {
      |		var nodes = [this.bodyFixed2.children, this.bodyStatic.children],
      |		    fixedSelBgColor = this.options.fixedSelectedBgColor,
      |		    selBgColor = this.options.selectedBgColor,
      |		    fixedCols = this.options.fixedCols,
      |		    colIdx = this.columns,
      |		    bgColor, rows, inputs, i;
      |
      |		while (colIdx) {
      |			rows = (((--colIdx) < fixedCols) ? nodes[0] : nodes[1])[colIdx].children;
      |			bgColor = (colIdx < fixedCols) ? fixedSelBgColor : selBgColor;
      |
      |			i = toRemove.length;
      |			while (i) { rows[toRemove[--i]].style.backgroundColor = ""; }
      |
      |			i = toSelect.length;
      |			while (i) { rows[toSelect[--i]].style.backgroundColor = bgColor; }
      |		}
      |		if (this.options.showSelectionColumn) {
      |			inputs = nodes[(!this.usesTouch) ? 0 : 1][0].getElementsByTagName("INPUT");
      |
      |			i = toRemove.length;
      |			while (i) { inputs[toRemove[--i]].checked = false; }
      |
      |			i = toSelect.length;
      |			while (i) { inputs[toSelect[--i]].checked = true; }
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.preventSelectionInputStateChange = function(event) {
      |		var event = event || window.event,
      |		    target = event.target || event.srcElement,
      |		    targetClass = target.className || "",
      |		    rowIdx;
      |
      |		if (event.button !== 2) {
      |			if (targetClass.indexOf("g_Cb") > -1 || targetClass.indexOf("g_Rd") > -1) {
      |				do {
      |					targetClass = (target = target.parentNode).className || "";
      |				} while (targetClass.indexOf("g_BR") === -1 && targetClass !== "g_Body");
      |
      |				if (targetClass.indexOf("g_BR") > -1) {
      |					rowIdx = parseInt(/g_R(\d+)/.exec(targetClass)[1], 10);
      |					(event.target || event.srcElement).checked = (indexOf(this.selectedIndexes, rowIdx) > -1);
      |				}
      |			}
      |		}
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	GridProto.cleanUp = function() {
      |		this.alignTimer = (this.alignTimer) ? window.clearTimeout(this.alignTimer) : null;
      |		this.element.innerHTML = "";
      |		try { this.css.sheet.parentNode.removeChild(this.css.sheet); } catch (e) {}
      |		return null;
      |	};
      |
      |	//////////////////////////////////
      |	//
      |	// Utility Methods
      |	//
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var getIEVersion = function() {
      |		var nav, version;
      |
      |		if ((nav = navigator).appName === "Microsoft Internet Explorer") {
      |			if (new RegExp("MSIE ([0-9]{1,}[\.0-9]{0,})").exec(nav.userAgent)) {
      |				version = parseFloat(RegExp.$1);
      |			}
      |		}
      |		return (version > 5) ? version : undefined;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var parseJSON = function(source) {
      |		var sourceType, json, win;
      |
      |		if ((sourceType = typeof(source)) === "string") {
      |			if (((win = window).JSON || {}).parse) {
      |				json = win.JSON.parse(source);
      |			} else {
      |				json = (function() { try { return (new Function("return " + source))(); } catch (e) { return; } })();
      |			}
      |		}
      |
      |		return json || (sourceType === "object" && (json = source)) || null;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var parseXML = function(source) {
      |		var sourceType, dE, xml;
      |
      |		if ((sourceType = typeof(source)) === "string") {
      |			if (window.DOMParser) {
      |				xml = new DOMParser().parseFromString(source, "text/xml");
      |			} else if (window.ActiveXObject) {
      |				xml = new ActiveXObject("Microsoft.XMLDOM");
      |				xml.async = false;
      |				xml.loadXML(source);
      |			}
      |		} else if (sourceType === "object") {
      |			dE = (source.ownerDocument || source).documentElement || {};
      |			if (dE.nodeName && dE.nodeName.toUpperCase() !== "HTML") {
      |				xml = source;
      |			}
      |		}
      |
      |		return xml || null;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var addEvent = (document.addEventListener) ?
      |	  function(elem, type, listener) { elem.addEventListener(type, listener, false); } :
      |	  function(elem, type, listener) { elem.attachEvent("on" + type, listener); };
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var stopEvent = function(event) {
      |		if (event.stopPropagation) {
      |			event.stopPropagation();
      |			event.preventDefault();
      |		} else {
      |			event.returnValue = false;
      |			event.cancelBubble = true;
      |		}
      |		return false;
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var removeEvent = (document.addEventListener) ?
      |	  function(elem, type, listener) { elem.removeEventListener(type, listener, false); } :
      |	  function(elem, type, listener) { elem.detachEvent("on" + type, listener); };
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var getEventPositions = function(event, type) {
      |		var pageX = event.pageX,
      |		    pageY = event.pageY,
      |		    doc, elem;
      |
      |		// Client position:
      |		if (type === "client") {
      |			if (pageX !== undefined || pageY !== undefined) {
      |				return { x : pageX - window.pageXOffset, y : pageY - window.pageYOffset };
      |			}
      |			return { x : event.clientX, y : event.clientY };
      |		}
      |
      |		// Page position:
      |		if (pageX === undefined || pageY === undefined) {
      |			elem = ((doc = document).documentElement.scrollLeft !== undefined) ? doc.documentElement : doc.body;
      |			return { x : event.clientX + elem.scrollLeft, y : event.clientY + elem.scrollTop };
      |		}
      |		return { x : pageX, y : pageY };
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var bind = function(func, that) {
      |		var a = slice.call(arguments, 2);
      |		return function() { return func.apply(that, a.concat(slice.call(arguments))); };
      |	};
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var indexOf = ([].indexOf) ?
      |	  function(arr, item) { return arr.indexOf(item); } :
      |	  function(arr, item) {
      |	  	for (var i=0, len=arr.length; i<len; i++) { if (arr[i] === item) { return i; } } return -1;
      |	  };
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var clearTextSelections = (window.getSelection) ?
      |		function() { window.getSelection().removeAllRanges(); return false; } : (document.selection) ?
      |		function() { document.selection.empty(); return false; } :
      |		function() { return false; };
      |
      |	//////////////////////////////////////////////////////////////////////////////////
      |	var $ = function(elemId) { return document.getElementById(elemId); },
      |	    slice = Array.prototype.slice,
      |	    msie = getIEVersion();
      |
      |	// Expose:
      |	window.Grid = Grid;
      |
      |})(this, this.document);
      |
      |
    """.stripMargin

}
