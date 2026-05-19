# Proposal: Use `FUNC: initialization` / `FUNC: finalization` instead of synthetic `METH:` for module init sections

## Summary

Change the CCG report labeling for Delphi unit `initialization` and `finalization` sections from the current synthetic `METH: ModuleName.Initialization` pattern to `FUNC: initialization` and `FUNC: finalization`.

## Current behavior

`ClassInfoUnit.pas` maps the initialization/finalization section of a unit to a synthetic class name by replacing dots in the module name with underscores:

```delphi
// ClassInfoUnit.GetClassName (paraphrased)
if SameText(QualifiedNameParts[0], 'initialization') or
   SameText(QualifiedNameParts[0], 'finalization') then
   Result := StringReplace(AModuleName, '.', '_', [rfReplaceAll]);
```

And `GetClassProcedureName` renames the procedure to `Initialization`.

This means a unit named `My.Utils` produces:

```
METH: 42-55 My_Utils.Initialization
```

## Problems with the current approach

1. **`My_Utils` is not a real class.** An AI reading the report has no way to know this is a synthetic label for module-level code. It will look for a `TMyUtils` or `My_Utils` class in the source and find nothing.

2. **`METH:` is semantically wrong.** `initialization`/`finalization` sections are not methods of any class. Using `METH:` for them conflicts with the intended semantics of that keyword in the CCG format.

3. **Dot-to-underscore mangling loses information.** The module name `My.Utils` becomes `My_Utils`, which is ambiguous — it could equally be a unit named `My_Utils` (no dots). The original qualified name is lost.

4. **Inconsistency across tools.** DWScript's CCG implementation, which follows this spec, uses `FUNC: initialization` / `FUNC: finalization` for these sections (they are not class members). Having two tools emit different labels for the same semantic concept makes the format less reliable as a standard.

## Proposed change

Emit a `FUNC:` entry with the fixed name `initialization` or `finalization`:

```
FUNC: 42-55 initialization
```

Or, for disambiguation when a report covers multiple units, qualify with the unit name:

```
FUNC: 42-55 My.Utils initialization
```

The simpler single-keyword form is preferred because:
- The enclosing `UNIT:` block already identifies which unit the section belongs to.
- `initialization` and `finalization` are reserved keywords — they cannot collide with user-defined function names.
- It matches what an AI would naturally call these sections when reading Delphi source.

## Suggested implementation

In `CCGCoverageReport.pas`, the existing condition already handles the `Global` class specially for standalone functions. Extend it to also handle the synthesized module-name class:

```delphi
// Before (paraphrased):
if (ClassInfo.TheClassName <> '') and
   (ClassInfo.TheClassName <> 'Global') and ... then
  CCGFile.Add('  METH: ...')
else
  CCGFile.Add('  FUNC: ...');

// After: also treat module-init synthetic class as FUNC:
function IsModuleInitClass(const className, moduleName : String) : Boolean;
begin
  Result := SameText(className, StringReplace(moduleName, '.', '_', [rfReplaceAll]));
end;

if (ClassInfo.TheClassName <> '') and
   (ClassInfo.TheClassName <> 'Global') and
   not IsModuleInitClass(ClassInfo.TheClassName, ModuleName) and ... then
  CCGFile.Add('  METH: ...')
else begin
  // For init/finalization, use the reserved keyword as name
  var procName := ProcInfo.Name;
  if SameText(procName, 'Initialization') then procName := 'initialization';
  if SameText(procName, 'Finalization')   then procName := 'finalization';
  CCGFile.Add('  FUNC: ' + IntToStr(MinLine) + '-' + IntToStr(MaxLine) + ' ' + procName);
end;
```

## Impact

- **Breaking change for tooling** that parses CCG files and expects `METH: ModuleName.Initialization`. The change is intentional: the current output is misleading.
- **DWScript alignment**: DWScript's `dwsCoverage.pas` already emits `FUNC: initialization` / `FUNC: finalization` for these sections; this change makes DCC consistent with that.
- **AI tooling**: Any AI consuming CCG reports will correctly identify these as module-level lifecycle hooks rather than confusing them with class method gaps.

## Alternatives considered

- **Keep `METH:` but use a fixed synthetic class name** (e.g., `METH: $module.initialization`): Still semantically wrong — init sections are not methods.
- **Omit init/finalization entirely**: Loses useful coverage data; developers do write meaningful code there.
- **Use the full qualified unit name** (e.g., `FUNC: My.Utils.initialization`): Valid, but redundant given the enclosing `UNIT:` context.
