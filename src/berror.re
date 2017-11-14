let usage = {|BetterErrors

[Usage]:
myBuildOutput 2>&1 | berror

Output errors in Reason syntax:
myBuildOutput 2>&1 | berror --path-to-refmttype <refmttype binary here>
|};

let refmttypePath = ref(None);

let options = [
  (
    "--path-to-refmttype",
    Arg.String((x) => refmttypePath := Some(x)),
    "<parse>, parse AST as <parse> (either 'ml', 're', 'binary_reason(for interchange between Reason versions)', 'binary (from the ocaml compiler)')"
  )
];

let () =
  Arg.parse(
    options,
    (arg) =>
      prerr_endline(
        "BetterErrors (berror) doesn't accept anonymous arguments in the command line."
      ),
    usage
  );

1;

let longStorePath = Re_pcre.regexp({|\.esy\/\d[_]+\/|});

let prettifyGlobalBuildStores = (logLine) =>
  Re_pcre.substitute(
    ~rex=longStorePath,
    ~subst=(s) => ".esy/" ++ String.make(1, s.[5]) ++ "/",
    logLine
  );

Index.parseFromStdin(
  ~refmttypePath=refmttypePath^,
  ~customLogOutputProcessors=[prettifyGlobalBuildStores],
  ~customErrorParsers=[]
);
