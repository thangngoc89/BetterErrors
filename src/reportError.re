open BetterErrorsTypes;

open Helpers;

let suggestifyList = suggestions =>
  suggestions |> List.map(sug => yellow("- " ++ sug));

let highlightPart = (~color, ~part, str) => {
  let indexOfPartInStr = Helpers.stringFind(str, part);
  highlight(
    ~color,
    ~first=indexOfPartInStr,
    ~last=indexOfPartInStr + String.length(part),
    str,
  );
};

let refmttypeNewlineR = Re_pcre.regexp({|\\n|});

let formatOutputSyntax = (~refmttypePath, types) =>
  switch (refmttypePath) {
  | None => types
  | Some(path) =>
    let types = String.concat("\\\"", types);
    let cmd = path ++ (sp({| "%s"|}))(types);
    let input = Unix.open_process_in(cmd);
    let result = {contents: []};
    try (
      while (true) {
        result.contents = [
          Re_pcre.substitute(
            ~rex=refmttypeNewlineR,
            ~subst=(_) => "\n",
            input_line(input),
          ),
          ...result.contents,
        ];
      }
    ) {
    | End_of_file => ignore(Unix.close_process_in(input))
    };
    List.rev(result^);
  };

let report = (~refmttypePath, parsedContent) : list(string) => {
  let formatOutputSyntax = formatOutputSyntax(~refmttypePath);
  /*
   * For some reason refmttype outputs the string (backslash followed by n)
   * instead of actual newlines.
   */
  switch (parsedContent) {
  | NoErrorExtracted => []
  | Type_MismatchTypeArguments({typeConstructor, expectedCount, actualCount}) => [
      sp(
        "You must pass exactly %d argument%s to this variant. You have passed %d argument%s.",
        expectedCount,
        expectedCount == 1 ? "" : "s",
        actualCount,
        actualCount == 1 ? "" : "s",
      ),
    ]
  | Type_IncompatibleType({
      actual,
      expected,
      differingPortion,
      actualEquivalentType,
      expectedEquivalentType,
      extra,
    }) =>
    /* let (diffA, diffB) = differingPortion; */
    let (actual, expected) =
      switch (formatOutputSyntax([actual, expected])) {
      | [a, b] => (a, b)
      | _ => (actual, expected)
      };
    let main = [
      "",
      sp("%s %s", bold("Expecting:"), highlight(~color=green, expected)),
      sp(
        "%s %s",
        bold("This type:"),
        highlight(~color=red, ~bold=true, actual),
      ),
      "",
      "This type doesn't match what is expected.",
    ];
    switch (extra) {
    | Some(e) => ["Extra info: " ++ e, ...main]
    | None => main
    };
  | Type_NotAFunction({actual}) =>
    let actual =
      switch (formatOutputSyntax([actual])) {
      | [a] => a
      | _ => actual
      };
    [
      "This has type " ++ actual ++ ", but you are calling it as a function.",
      "Perhaps you have forgoten a semicolon, or a comma somewhere.",
    ];
  | Type_AppliedTooMany({functionType, expectedArgCount}) =>
    let functionType =
      switch (formatOutputSyntax([functionType])) {
      | [a] => a
      | _ => functionType
      };
    [
      sp(
        "It accepts only %d arguments. You gave more. Maybe you forgot a ; somewhere?",
        expectedArgCount,
      ),
      sp("This function has type %s", functionType),
    ];
  | File_SyntaxError({offendingString, hint}) => [
      "Note: the location indicated might not be accurate.",
      switch (offendingString) {
      | ";" => "Make sure all imperative statements, as well as let/type bindings have exactly one semicolon separating them."
      | "else" =>
        "Did you happen to have put a semicolon on the line before else?"
        ++ " Also, `then` accepts a single expression. If you've put many, wrap them in parentheses."
      | _ => ""
      },
      switch (hint) {
      | Some(a) => "This is a syntax error: " ++ a
      | None => "This is a syntax error."
      },
    ]
  | File_IllegalCharacter({character}) => [
      sp("The character `%s` is illegal.", character),
    ]
  | Type_UnboundTypeConstructor({namespacedConstructor, suggestion}) =>
    let namespacedConstructor =
      switch (formatOutputSyntax([namespacedConstructor])) {
      | [a] => a
      | _ => namespacedConstructor
      };
    let main =
      sp(
        "The type %s can't be found.",
        red(~bold=true, namespacedConstructor),
      );
    switch (suggestion) {
    | None => [main]
    | Some(h) => [sp("Hint: did you mean %s?", yellow(h)), "", main]
    };
  | Type_ArgumentCannotBeAppliedWithLabel({functionType, attemptedLabel}) =>
    let formattedFunctionType =
      switch (formatOutputSyntax([functionType])) {
      | [a] => a
      | _ => functionType
      };
    [
      sp(
        "This function doesn't accept an argument named ~%s.",
        attemptedLabel,
      ),
      "",
      sp("The function has type %s", formattedFunctionType),
    ];
  | Type_UnboundValue({unboundValue, suggestions}) =>
    switch (suggestions) {
    | None => [
        sp(
          "The value named %s can't be found. Could it be a typo?",
          red(~bold=true, unboundValue),
        ),
      ]
    | Some([hint]) => [
        sp(
          "The value named %s can't be found. Did you mean %s?",
          red(~bold=true, unboundValue),
          yellow(hint),
        ),
      ]
    | Some([hint1, hint2]) => [
        sp(
          "%s can't be found. Did you mean %s or %s?",
          red(~bold=true, unboundValue),
          yellow(hint1),
          yellow(hint2),
        ),
      ]
    | Some(hints) =>
      List.concat([
        suggestifyList(hints),
        [
          sp(
            "%s can't be found. Did you mean one of these?",
            red(~bold=true, unboundValue),
          ),
        ],
      ])
    }
  | Type_UnboundRecordField({recordField, suggestion}) =>
    let recordField =
      switch (formatOutputSyntax([recordField])) {
      | [a] => a
      | _ => recordField
      };
    let main =
      switch (suggestion) {
      | None =>
        sp(
          "Record field %s can't be found in any record type.",
          red(~bold=true, recordField),
        )
      | Some(hint) =>
        sp(
          "Record field %s can't be found in any record type. Did you mean %s?",
          red(~bold=true, recordField),
          yellow(hint),
        )
      };
    [
      "Alternatively, instead of opening a module, you can prefix the record field name like {TheModule.x: 0, y: 100}.",
      "Record fields must be \"in scope\". That means you need to `open TheModule` where the record type is defined.",
      "",
      main,
    ];
  | Type_RecordFieldNotBelongPattern({
      expressionType,
      recordField,
      suggestion,
    }) =>
    let expressionType =
      switch (formatOutputSyntax([expressionType])) {
      | [a] => a
      | _ => expressionType
      };
    let main = [
      sp("The field %s doesn't belong to it", red(~bold=true, recordField)),
      sp("This record has type: %s", bold(expressionType)),
    ];
    switch (suggestion) {
    | None => main
    | Some(hint) => [sp("Did you mean %s?", yellow(hint)), ...main]
    };
  | Type_SomeRecordFieldsUndefined(recordField) => [
      "record is of some other type - one that does have a "
      ++ bold(recordField)
      ++ " field. Where else is it used?",
      "if you use this record in a way that would make the type checker think this",
      "If you are certain this record shouldn't have a field named "
      ++ bold(recordField)
      ++ " then check ",
      "",
      sp(
        "You forgot to include the record field named %s.",
        red(~bold=true, recordField),
      ),
    ]
  | Type_UnboundModule({unboundModule, suggestion}) =>
    let unboundModule =
      switch (formatOutputSyntax([unboundModule])) {
      | [a] => a
      | _ => unboundModule
      };
    let main =
      sp(
        "Module %s not found in included libraries.\n",
        red(~bold=true, unboundModule),
      );
    switch (suggestion) {
    | Some(s) => [sp("Hint: did you mean %s?", yellow(s)), main]
    | None => [
        sp(
          " - ocamlbuild: make sure you have `-pkgs libraryName` in your build command.",
        ),
        " - ocamlfind: make sure you have `-package libraryName -linkpkg` in your build command.",
        sp(
          " - For jbuilder: make sure you include the library that contains %s in your jbuild file's (libraries ...) section.",
          unboundModule,
        ),
        "You can see which libraries are available by doing `ocamlfind list` (or `esy ocamlfind list` inside your esy project)",
        sp(
          "Hint: You might need to tell your build system to depend on a library that contains %s.",
          unboundModule,
        ),
        main,
      ]
    };
  | Type_SignatureItemMismatch({missing, values, types, notes: _}) =>
    let finalMessage =
      switch (missing, values, types) {
      | ([], [], []) => "This module doesn't match its signature. See the original error output"
      | ([hd, ...tl], _, _) =>
        String.concat(
          "\n",
          [
            "",
            sp(
              "%s %s",
              purple(~bold=true, "Learn:"),
              "\"Signatures\" are interfaces that modules may implement.",
            ),
            "  You can indicate that a Reason file (.re) implements a signature by",
            "  creating an \"interface file\" (.rei) of the same name.",
            "  Modules nested inside of a Reason file may also opt into being checked",
            "  against any signature using the type constraint syntax `module M : Sig = ...`",
          ],
        )
      | _ => ""
      };
    let whatStr = (
      fun
      | Type => "type"
      | Value => "value"
    );
    let missingMsg = ((what, named, declaredAtFile, declaredAtLine)) =>
      String.concat(
        "\n",
        [
          sp(
            "%s %s",
            bold("This module is missing the " ++ whatStr(what) ++ " named"),
            red(~bold=true, named),
          ),
          "",
          sp(
            "  The following signature requires that %s be defined:",
            bold(named),
          ),
          sp("  %s%s", cyan(declaredAtFile), dim(":" ++ declaredAtLine)),
          "",
        ],
      );
    let badValueMsg = info => {
      let (what, named, good, goodFile, goodLn, badName, bad, badFile, badLn) = info;
      let (bad, good) =
        switch (formatOutputSyntax([bad, good])) {
        | [a, b] => (a, b)
        | _ => (bad, good)
        };
      String.concat(
        "\n",
        [
          sp(
            "%s %s %s %s",
            bold("This module doesn't match its signature because the"),
            bold(whatStr(what)),
            red(~bold=true, named),
            bold("has the wrong type"),
          ),
          "",
          sp(
            "  %s %s%s",
            "At",
            cyan(goodFile),
            dim(":" ++ goodLn),
          ),
          sp("  the signature required that %s be of type:", bold(named)),
          "",
          highlight(~bold=true, ~color=green, indentStr("  ", good)),
          "",
          "",
          sp(
            "  %s %s%s",
            "At",
            cyan(badFile),
            dim(":" ++ badLn),
          ),
          sp("  your module defined %s having type:", bold(named)),
          "",
          highlight(~bold=true, ~color=red, indentStr("  ", bad)),
          "",
        ],
      );
    };
    let badTypeMsg = info => {
      let (good, goodFile, goodLn, bad, badFile, badLn, arity) = info;
      let (bad, good) =
        switch (formatOutputSyntax([bad, good])) {
        | [a, b] => (a, b)
        | _ => (bad, good)
        };
      String.concat(
        "\n",
        [
          arity ?
            bold(
              "This module contains a type definition with the wrong number of type parameters ",
            ) :
            bold(
              "This module contains a type definition that contradicts its signature",
            ),
          "",
          sp(
            "  %s %s%s",
            "At",
            cyan(goodFile),
            dim(":" ++ goodLn),
          ),
          "  the signature required that the type be defined as:",
          "",
          "  " ++ highlight(~bold=true, ~color=green, good),
          "",
          "",
          sp(
            "  %s %s%s",
            "At",
            cyan(badFile),
            dim(":" ++ badLn),
          ),
          "  your module defined the type to be:",
          "",
          "  " ++ highlight(~bold=true, ~color=red, bad),
          "",
        ],
      );
    };
    let missingString = List.map(missingMsg, missing);
    let badValueString = List.map(badValueMsg, values);
    let badTypeString = List.map(badTypeMsg, types);
    List.concat([
      [finalMessage],
      missingString,
      badValueString,
      badTypeString,
    ]);
  | _ => ["Error beautifier not implemented for this."]
  };
};
/* DesignGuide.show(); */
