open BetterErrorsTypes;

open Helpers;

let indent = (prefixStr, lines) => List.map((s) => prefixStr ++ s, lines);

let numberOfDigits = (n) => {
  let digits = ref(1);
  let nn = ref(n);
  while (nn^ / 10 > 0) {
    nn := nn^ / 10;
    digits := digits^ + 1
  };
  digits^
};

let pad = (~ch=' ', content, n) => String.make(n - String.length(content), ch) ++ content;

let startingSpacesCount = (str) => {
  let rec startingSpacesCount' = (str, idx) =>
    if (idx == String.length(str)) {
      idx
    } else if (str.[idx] != ' ') {
      idx
    } else {
      startingSpacesCount'(str, idx + 1)
    };
  startingSpacesCount'(str, 0)
};

/* row and col 0-indexed; endColumn is 1 past the actual end. See
   Main.compilerLineColsToRange */
let _printFile =
    (
      ~titleLine,
      ~highlightColor as color,
      ~highlight as ((startRow, startColumn), (endRow, endColumn)),
      content
    ) => {
  let displayedStartRow = max(0, startRow - 3);
  /* we display no more than 3 lines after startRow. Some endRow are rly far
     away */
  let displayedEndRow = min(List.length(content) - 1, startRow + 3);
  let lineNumWidth = numberOfDigits(List.length(content));
  /* sometimes the snippet of file we show is really indented. We de-indent it
     for nicer display by trimming out the maximum amount of leading spaces we can. */
  let rowsForCountingStartingSpaces =
    listDrop(displayedStartRow, content)
    |> listTake(displayedEndRow - displayedStartRow + 1)
    |> List.filter((row) => row != "");
  let minIndent =
    switch rowsForCountingStartingSpaces {
    | [] => 0
    | _ =>
      let startingSpaces = List.map(startingSpacesCount, rowsForCountingStartingSpaces);
      List.fold_left((acc, num) => num < acc ? num : acc, List.hd(startingSpaces), startingSpaces)
    };
  /*
   * TODO: Create a better vertical separator for the second case.
   */
  let sep = minIndent === 0 ? " ¦ " : " ¦ ";
  let startColumn = startColumn - minIndent;
  let endColumn = endColumn - minIndent;
  let revResult = ref([titleLine]);
  for (i in displayedStartRow to displayedEndRow) {
    let currLine = List.nth(content, i) |> stringSlice(~first=minIndent);
    if (i >= startRow && i <= endRow) {
      if (startRow == endRow) {
        let highlighted =
          highlight(
            ~underline=true,
            ~bold=true,
            ~color,
            ~first=startColumn,
            ~last=endColumn,
            currLine
          );
        revResult.contents = [
          pad(string_of_int(i + 1), lineNumWidth) ++ sep ++ highlighted,
          ...revResult.contents
        ]
      } else if (i == startRow) {
        revResult.contents = [
          pad(string_of_int(i + 1), lineNumWidth)
          ++ sep
          ++ highlight(~underline=true, ~bold=true, ~color, ~first=startColumn, currLine),
          ...revResult.contents
        ]
      } else if (i == endRow) {
        revResult.contents = [
          pad(string_of_int(i + 1), lineNumWidth)
          ++ sep
          ++ highlight(~underline=true, ~bold=true, ~color, ~last=endColumn, currLine),
          ...revResult.contents
        ]
      } else {
        revResult.contents = [
          pad(string_of_int(i + 1), lineNumWidth)
          ++ sep
          ++ highlight(~underline=true, ~bold=true, ~color, currLine),
          ...revResult.contents
        ]
      }
    } else {
      revResult.contents = [
        pad(string_of_int(i + 1), lineNumWidth) ++ sep ++ currLine,
        ...revResult.contents
      ]
    }
  };
  revResult.contents
};

let printFile = (~isWarningWithCode=?, {cachedContent, filePath, range}) => {
  let ((startRow, startColumn), (endRow, endColumn)) = range;
  let (isWarning, labelColor, label, warningCodeStr) =
    switch isWarningWithCode {
    | None => (false, red, "Error", "")
    | Some(i) => (true, yellow, "Warning", " [Warning Code " ++ string_of_int(i) ++ "] ")
    };
  let titleLine =
    if (startRow === endRow) {
      sp(
        "%s %s %s",
        labelColor(~bold=true, label ++ ":"),
        cyan(~underline=true, sp("%s:%d %d-%d", filePath, startRow + 1, startColumn, endColumn)),
        labelColor(~bold=true, warningCodeStr)
      )
    } else {
      sp(
        "%s %s %s",
        labelColor(~bold=true, label ++ ":"),
        cyan(
          ~underline=true,
          sp("%s:%d:%d-%d:%d", filePath, startRow + 1, startColumn, endRow + 1, endColumn)
        ),
        labelColor(~bold=true, warningCodeStr)
      )
    };
  _printFile(~titleLine, ~highlightColor=isWarning ? yellow : red, ~highlight=range, cachedContent)
};

/**
 * Processes standard log lines that don't have anything to do with
 * errors/warnings, or files. Useful for hiding/transforming/relativizing file
 * paths etc.
 */
let processLogOutput = (~customLogOutputProcessors, str) =>
  List.fold_left(
    (strSoFar, nextProcessor) => nextProcessor(strSoFar),
    str,
    customLogOutputProcessors
  );

let prettyPrintParsedResult =
    (~originalRevLines: list(string), ~refmttypePath, result: result)
    : list(string) =>
  switch result {
  | Unparsable => originalRevLines
  /* output the line without any decoration around. We previously had some
     cute little ascii red x mark to say "we couldn't parse this but there's
     probably an error". But it's very possible that this line's a continuation
     of a previous error, just that we couldn't parse it. So we try to bolt this
     line right after our supposedly parsed and pretty-printed error to make them
     look like one printed error. */
  /* the effing length we'd go for better errors... someone gimme a cookie */
  | ErrorFile(NonexistentFile) =>
    /* this case is never reached because we don't ever return `ErrorFile NonexistentFile` from
       `ParseError.specialParserThatChecksWhetherFileEvenExists` */
    originalRevLines
  | ErrorFile(Stdin(original)) => [
      sp("%s (from stdin - see message above)", red(~bold=true, "Error:")),
      original
    ]
  | ErrorFile(CommandLine(moduleName)) => [
      "",
      sp(
        "%s module %s not found.",
        red(~bold=true, "Error:"),
        red(~underline=true, ~bold=true, moduleName)
      ),
      ...originalRevLines
    ]
  | ErrorFile(NoneFile(filename)) =>
    /* TODO: test case for this. Forgot how to repro it */
    if (Filename.check_suffix(filename, ".cmo")) {
      [
        "Cmo files are artifacts the compiler looks for when compiling/linking dependent files.",
        sp(
          "%s Cannot find file %s.",
          red(~bold=true, "Error:"),
          red(~bold=true, ~underline=true, filename)
        ),
        ...originalRevLines
      ]
    } else {
      [
        sp("%s Cannot find file %s.", red(~bold=true, "Error:"), red(~bold=true, filename)),
        ...originalRevLines
      ]
    }
  | ErrorContent(withFileInfo) =>
    List.concat([
      ["", ""],
      ReportError.report(~refmttypePath, withFileInfo.parsedContent),
      [""],
      printFile(withFileInfo),
      [""],
      indent("  > ", originalRevLines),
      [sp("  > %s", underline("Unformatted Error Output:"))]
    ])
  | Warning(withFileInfo) =>
    List.concat([
      ["", ""],
      ReportWarning.report(
        ~refmttypePath,
        withFileInfo.parsedContent.code,
        withFileInfo.filePath,
        withFileInfo.parsedContent.warningType
      ),
      [""],
      printFile(~isWarningWithCode=withFileInfo.parsedContent.code, withFileInfo),
      [""],
      indent("  > ", originalRevLines),
      [sp("  > %s", underline("Unformatted Warning Output:"))]
    ])
  };
