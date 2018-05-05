/* records that are only used by their variant tag of similar name below. We
   need inline record type declarations... */
type mismatchTypeArguments = {
  typeConstructor: string,
  expectedCount: int,
  actualCount: int,
};

type unboundValue = {
  unboundValue: string,
  suggestions: option(list(string)),
};

type signatureItemWhatsMissing =
  | Value
  | Type;

type signatureItemMismatch = {
  notes: string,
  /* (whatsWrong, name, originalFile, lineNumber) */
  missing: list((signatureItemWhatsMissing, string, string, string)),
  /* (whatsWrong, goodName, good, goodFile, goodLn, badName, bad, badFile, badLn) */
  values:
    list(
      (
        signatureItemWhatsMissing,
        string,
        string,
        string,
        string,
        string,
        string,
        string,
        string,
      ),
    ),
  /* (goodType, goodFile, goodln, badType, badFile, badln, arity) */
  types: list((string, string, string, string, string, string, bool)),
};

type unboundModule = {
  unboundModule: string,
  suggestion: option(string),
};

type unboundConstructor = {
  constructor: string,
  expectedCount: int,
  observedCount: int,
};

type unboundTypeConstructor = {
  namespacedConstructor: string,
  suggestion: option(string),
};

type argumentCannotBeAppliedWithLabel = {
  functionType: string,
  attemptedLabel: string,
};

type labelIssue =
  | HasOptionalLabel(string)
  | HasLabel(string)
  | HasNoLabel
  | Unknown;

type functionWrongLabel = {
  functionType: string,
  labelIssue,
};

type appliedTooMany = {
  functionType: string,
  expectedArgCount: int,
};

type recordFieldNotInExpression = {
  constructor: string,
  expectedCount: int,
  observedCount: int,
};

type recordFieldNotBelong = {
  expressionType: string,
  recordField: string,
  suggestion: option(string),
};

type recordFieldError = {
  constructor: string,
  expectedCount: int,
  observedCount: int,
};

type inconsistentAssumptions = {
  constructor: string,
  expectedCount: int,
  observedCount: int,
};

type catchAll = {
  warningCode: int,
  message: string,
};

type unusedVariable = {
  constructor: string,
  expectedCount: int,
  observedCount: int,
};

type fieldNotBelong = {
  actual: string,
  expected: string,
};

type badFileName =
  | Leading(string)
  | Contains(string)
  | UnknownIllegalChar;

type incompatibleType = {
  actual: string,
  expected: string,
  differingPortion: (string, string),
  actualEquivalentType: option(string),
  expectedEquivalentType: option(string),
  extra: option(string),
};

type notAFunction = {actual: string};

type syntaxError = {
  offendingString: string,
  hint: option(string),
};

type illegalCharacter = {character: string};

type patternNotExhaustive = {unmatched: list(string)};

type unparsableButWithFileInfo = {error: string};

type unboundRecordField = {
  recordField: string,
  suggestion: option(string),
};

type optionalArgumentNotErased = {argumentName: string};

/* -------------------------- */
type warningType =
  | Warning_UnusedVariable(unusedVariable)
  | Warning_PatternNotExhaustive(patternNotExhaustive)
  | Warning_PatternUnused(unusedVariable)
  | Warning_OptionalArgumentNotErased(optionalArgumentNotErased)
  | Warning_BadFileName(badFileName)
  | NoWarningExtracted;

type error =
  | Type_MismatchTypeArguments(mismatchTypeArguments)
  | Type_UnboundValue(unboundValue)
  | Type_SignatureItemMismatch(signatureItemMismatch)
  | Type_UnboundModule(unboundModule)
  | Type_UnboundRecordField(unboundRecordField)
  | Type_UnboundConstructor(unboundConstructor)
  | Type_UnboundTypeConstructor(unboundTypeConstructor)
  | Type_ArgumentCannotBeAppliedWithLabel(argumentCannotBeAppliedWithLabel)
  | Type_FunctionWrongLabel(functionWrongLabel)
  | Type_AppliedTooMany(appliedTooMany)
  | Type_RecordFieldNotInExpression(recordFieldNotInExpression)
  | Type_RecordFieldNotBelongPattern(recordFieldNotBelong)
  | Type_SomeRecordFieldsUndefined(string)
  | Type_RecordFieldError(recordFieldError)
  /* might be the same thing as above? jordan wrote "record expression" instead of "pattern" */
  | Type_FieldNotBelong(fieldNotBelong)
  | Type_IncompatibleType(incompatibleType)
  | Type_NotAFunction(notAFunction)
  | File_SyntaxError(syntaxError)
  | Build_InconsistentAssumptions(inconsistentAssumptions)
  | File_IllegalCharacter(illegalCharacter)
  | NoErrorExtracted;

type fileError =
  | NoneFile(string)
  | NonexistentFile
  | CommandLine(string)
  | Stdin(string);

type warning = {
  code: int,
  warningType,
};

type withFileInfo('a) = {
  filePath: string,
  cachedContent: list(string),
  range: Atom.Range.t,
  parsedContent: 'a,
};

type result =
  | Unparsable
  | ErrorFile(fileError)
  | ErrorContent(withFileInfo(error))
  | Warning(withFileInfo(warning));
