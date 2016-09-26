{**********************************************************************}
{                                                                      }
{    The contents of this file are subject to the GNU General          }
{    Public License version 3 (the "License") as published by          }
{    the Free Software Foundation. You may obtain a copy of            }
{    the License at https://www.gnu.org/licenses/gpl-3.0.txt           }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsJSLibModule;

{$I dws.inc}

interface

uses
   Classes, SysUtils, Variants,
   dwsLanguageExtension, dwsComp, dwsCompiler, dwsDataContext, dwsConnectorSymbols,
   dwsExprs, dwsTokenizer, dwsSymbols, dwsErrors, dwsCoreExprs,
   dwsStrings, dwsXPlatform, StrUtils, dwsUtils, dwsOperators, dwsUnitSymbols,
   dwsFunctions, dwsMagicExprs, dwsPascalTokenizer;

type

   // TdwsJSLibModule
   //
   TdwsJSLibModule = class (TdwsCustomLangageExtension)
      private
         FSymbolMarker : TTokenType;

      protected
         function CreateExtension : TdwsLanguageExtension; override;
         function StoreSymbolMarker : Boolean;

      public
         constructor Create(AOwner: TComponent); override;

      published
         property SymbolMarker : TTokenType read FSymbolMarker write FSymbolMarker stored StoreSymbolMarker;
   end;

   // TdwsJSLanguageExtension
   //
   TdwsJSLanguageExtension = class (TdwsLanguageExtension)
      private
         FSymbolMarker : TTokenType;

      public
         function CreateBaseVariantSymbol(table : TSystemSymbolTable) : TBaseVariantSymbol; override;
         procedure CreateSystemSymbols(table : TSystemSymbolTable); override;
         function StaticSymbols : Boolean; override;
         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; override;
         function RootExternalClass(compiler : TdwsCompiler; const externalName : String) : TClassSymbol; override;
         procedure ApplyConditionalDefines(defines : TStrings); override;

         property SymbolMarker : TTokenType read FSymbolMarker write FSymbolMarker;
   end;

   TdwsJSBlockExprSymbols = record
      Offset : Integer;
      Symbol : TSymbol;
      PrefixSymbol : TSymbol;
   end;

   // TdwsJSBlockExpr
   //
   TdwsJSBlockExpr = class (TNoResultExpr)
      private
         FCode : String;
         FSymbols : array of TdwsJSBlockExprSymbols;

      protected
         function GetSymbol(idx : Integer) : TSymbol;
         function GetPrefixSymbol(idx : Integer) : TSymbol;
         function GetOffset(idx : Integer) : Integer;

      public
         function RegisterSymbol(aSymbol : TSymbol; anOffset : Integer) : Integer;
         procedure RegisterPrefix(prefixSymbol : TSymbol);

         property Offsets[idx : Integer] : Integer read GetOffset;
         property Symbols[idx : Integer] : TSymbol read GetSymbol;
         property PrefixSymbols[idx : Integer] : TSymbol read GetPrefixSymbol;
         function SymbolsCount : Integer;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property Code : String read FCode write FCode;
   end;

   TdwsJSConnectorType = class(TInterfacedObject, IUnknown, IConnectorType)
      private
         FTable : TSymbolTable;

      protected
         function ConnectorCaption : String;
         function AutoVarParams : Boolean;
         function AcceptsParams(const params : TConnectorParamArray) : Boolean;
         function WritableReads(const memberName : UnicodeString) : Boolean;

         function HasMethod(const methodName : String; const params : TConnectorParamArray;
                            var typSym : TTypeSymbol) : IConnectorCall;
         function HasMember(const memberName : String; var typSym : TTypeSymbol;
                            isWrite : Boolean) : IConnectorMember;
         function HasIndex(const propName : String; const params : TConnectorParamArray;
                           var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
         function HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
         function HasCast(typSym: TTypeSymbol) : IConnectorCast;

      public
         constructor Create(table : TSymbolTable);
   end;

   TdwsJSConnectorCall = class(TInterfacedSelfObject, IUnknown, IConnectorCall, IConnectorArgsCall)
      private
         FMethodName : String;

      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData;
         function NeedDirectReference : Boolean;

      public
         constructor Create(const methodName : String);

         property CallMethodName : String read FMethodName write FMethodName;
   end;

   TdwsJSIndexCall = class(TdwsJSConnectorCall)
      private
         FIsWrite : Boolean;

      public
         constructor Create(const methodName : String; isWrite : Boolean);

         property IsWrite : Boolean read FIsWrite write FIsWrite;
   end;

   TdwsJSConnectorMember = class(TInterfacedSelfObject, IUnknown, IConnectorMember, IConnectorDataMember)
      protected
         FMemberName : String;

         function Read(const base : Variant) : TData;
         procedure Write(const base : Variant; const data : TData);

      public
         constructor Create(const memberName : String);

         property MemberName : String read FMemberName write FMemberName;
   end;

   TdwsJSConnectorEnumerator = class(TInterfacedSelfObject, IUnknown, IConnectorEnumerator)
      protected
         function NewEnumerator(const base : Variant; const args : TConnectorArgs) : IUnknown;
         function Step(const enumerator : IInterface; var data : TData) : Boolean;
   end;

   TdwsJSConnectorCast = class(TInterfacedSelfObject, IUnknown, IConnectorCast)
      protected
         function CastVariant(const base : Variant) : Variant;
   end;

   TJSConnectorSymbol = class(TConnectorSymbol)
      public
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function SupportsEmptyParam : Boolean; override;
   end;

   TTypeOfSymbol = class sealed(TFuncSymbol)
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cDefaultSymbolMarker = ttAT;

   SYS_JOBJECT = 'JObject';
   SYS_Undefined = 'Undefined';

// ------------------
// ------------------ TdwsJSLibModule ------------------
// ------------------

// Create
//
constructor TdwsJSLibModule.Create(AOwner: TComponent);
begin
   FSymbolMarker:=cDefaultSymbolMarker;
   inherited;
end;

// CreateExtension
//
function TdwsJSLibModule.CreateExtension : TdwsLanguageExtension;
var
   ext : TdwsJSLanguageExtension;
begin
   ext:=TdwsJSLanguageExtension.Create;
   ext.SymbolMarker:=SymbolMarker;
   Result:=ext;
end;

// StoreSymbolMarker
//
function TdwsJSLibModule.StoreSymbolMarker : Boolean;
begin
   Result:=(FSymbolMarker<>cDefaultSymbolMarker)
end;

// ------------------
// ------------------ TdwsJSLanguageExtension ------------------
// ------------------

// ReadInstr
//
function TdwsJSLanguageExtension.ReadInstr(compiler : TdwsCompiler) : TNoResultExpr;
var
   tok : TTokenizer;
   startPos : PChar;
   hotPos : TScriptPos;
   jsCode, name : String;
   sym : TSymbol;
   table : TSymbolTable;
   blockExpr : TdwsJSBlockExpr;
   firstSymbol : Boolean;
   ptsr : TPascalTokenizerStateRules;

   function FlushCode(drop : Integer) : String;
   begin
      SetString(Result, startPos, (NativeUInt(tok.PosPtr)-NativeUInt(startPos)) div SizeOf(Char)-NativeUInt(drop));
      startPos:=tok.PosPtr;
   end;

begin
   Result:=nil;
   tok:=compiler.Tokenizer;

   if not tok.Test(ttASM) then Exit;

   hotPos:=tok.HotPos;
   tok.KillToken;
   startPos:=tok.PosPtr;
   tok.TestName;

   ptsr := (tok.Rules as TPascalTokenizerStateRules);

   blockExpr:=TdwsJSBlockExpr.Create(hotPos);
   try

      jsCode:='';
      ptsr.CurlyComments:=False;
      ptsr.DollarNames:=True;

      // collect everything until 'end'
      while tok.HasTokens do begin

         if tok.Test(SymbolMarker) then begin
            tok.KillToken;
            jsCode:=jsCode+FlushCode(1);

            table:=compiler.CurrentProg.Table;
            firstSymbol:=True;

            repeat

               if not tok.TestDeleteNamePos(name, hotPos) then
                  compiler.Msgs.AddCompilerStop(hotPos, CPE_NameExpected);

               tok.KillToken;
               FlushCode(0);

               sym:=table.FindSymbol(name, cvMagic);
               if sym=nil then begin

                  compiler.Msgs.AddCompilerErrorFmt(hotPos, CPE_UnknownName, [name]);

               end else begin

                  compiler.RecordSymbolUseReference(sym, hotPos, True);

                  blockExpr.RegisterSymbol(sym, Length(jsCode)+1);
                  if firstSymbol then begin
                     firstSymbol:=False;
                     if sym.ClassType=TFieldSymbol then
                        blockExpr.RegisterPrefix(table.FindSymbol(SYS_SELF, cvMagic));
                  end;

                  if sym is TDataSymbol then
                     sym:=sym.Typ;
                  if sym is TStructuredTypeSymbol then
                     table:=TStructuredTypeSymbol(sym).Members;

               end;

               if not tok.TestDelete(ttDOT) then Break;

               jsCode:=jsCode+'.';

            until False;

         end;

         if tok.Test(ttEND) then begin
            jsCode:=jsCode+FlushCode(3);
            tok.KillToken;
            Break;
         end;

         tok.KillToken;
      end;

      blockExpr.Code:=jsCode;
      ptsr.CurlyComments:=True;
      ptsr.DollarNames:=False;

   except
      blockExpr.Free;
      raise;
   end;

   Result:=blockExpr;

   if not tok.HasTokens then
      compiler.Msgs.AddCompilerErrorFmt(tok.HotPos, 'Incomplete asm block%s', [tok.HotPos.AsInfo]);
end;

// RootExternalClass
//
function TdwsJSLanguageExtension.RootExternalClass(compiler : TdwsCompiler; const externalName : String) : TClassSymbol;
begin
   Result:=compiler.CurrentProg.Root.SystemTable.SymbolTable.FindTypeLocal(SYS_JOBJECT) as TClassSymbol;
end;

// ApplyConditionalDefines
//
procedure TdwsJSLanguageExtension.ApplyConditionalDefines(defines : TStrings);
begin
   defines.Add('SMARTPASCAL');
end;

// CreateBaseVariantSymbol
//
function TdwsJSLanguageExtension.CreateBaseVariantSymbol(table : TSystemSymbolTable) : TBaseVariantSymbol;
begin
   Result:=TJSConnectorSymbol.Create(SYS_VARIANT, TdwsJSConnectorType.Create(table));
   table.AddSymbol(Result);
   table.TypVariant:=Result;
end;

// CreateSystemSymbols
//
procedure TdwsJSLanguageExtension.CreateSystemSymbols(table : TSystemSymbolTable);
var
   jObject : TClassSymbol;
   meth : TMethodSymbol;
   typeOf : TFuncSymbol;
begin
   jObject:=TClassSymbol.Create(SYS_JOBJECT, nil);
   jObject.InheritFrom(table.TypObject);
   table.AddSymbol(jObject);
   jObject.ExternalName:='Object';
   jObject.IsExternal:=True;
   jObject.SetNoVirtualMembers;

   meth:=TMethodSymbol.Create('', fkConstructor, jObject, cvPublic, False);
   meth.Executable:=ICallable(TEmptyFunc.Create);
   meth.IsDefault:=True;
   jObject.AddMethod(meth);

   table.AddSymbol(TConstSymbol.CreateValue('Undefined', table.TypVariant, Unassigned));

   typeOf:=TTypeOfSymbol.Create('TypeOf', fkFunction, 0);
   typeOf.Typ:=table.TypString;
   typeOf.AddParam(TParamSymbol.Create('v', table.TypVariant));
   typeOf.IsExternal:=True;
   table.AddSymbol(typeOf);
end;

// StaticSymbols
//
function TdwsJSLanguageExtension.StaticSymbols : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TdwsJSBlockExpr ------------------
// ------------------

// RegisterSymbol
//
function TdwsJSBlockExpr.RegisterSymbol(aSymbol : TSymbol; anOffset : Integer) : Integer;
begin
   Result:=Length(FSymbols);
   SetLength(FSymbols, Result+1);
   FSymbols[Result].Offset:=anOffset;
   FSymbols[Result].Symbol:=aSymbol;
   FSymbols[Result].PrefixSymbol:=nil;
end;

// RegisterPrefix
//
procedure TdwsJSBlockExpr.RegisterPrefix(prefixSymbol : TSymbol);
begin
   FSymbols[High(FSymbols)].PrefixSymbol:=prefixSymbol;
end;

// SymbolsCount
//
function TdwsJSBlockExpr.SymbolsCount : Integer;
begin
   Result:=Length(FSymbols);
end;

// EvalNoResult
//
procedure TdwsJSBlockExpr.EvalNoResult(exec : TdwsExecution);
begin
   Assert(False, ClassName+' cannot be executed');
end;

// GetSymbol
//
function TdwsJSBlockExpr.GetSymbol(idx : Integer) : TSymbol;
begin
   Result:=FSymbols[idx].Symbol;
end;

// GetPrefixSymbol
//
function TdwsJSBlockExpr.GetPrefixSymbol(idx : Integer) : TSymbol;
begin
   Result:=FSymbols[idx].PrefixSymbol;
end;

// GetOffset
//
function TdwsJSBlockExpr.GetOffset(idx : Integer) : Integer;
begin
   Result:=FSymbols[idx].Offset;
end;

// ------------------
// ------------------ TdwsJSConnectorType ------------------
// ------------------

// Create
//
constructor TdwsJSConnectorType.Create(table : TSymbolTable);
begin
   inherited Create;
   FTable:=table;
end;

// ConnectorCaption
//
function TdwsJSConnectorType.ConnectorCaption : String;
begin
   Result:='JS Connector 1.0';
end;

// AutoVarParams
//
function TdwsJSConnectorType.AutoVarParams : Boolean;
begin
   Result:=False;
end;

// AcceptsParams
//
function TdwsJSConnectorType.AcceptsParams(const params : TConnectorParamArray) : Boolean;
begin
   Result:=True;
end;

// WritableReads
//
function TdwsJSConnectorType.WritableReads(const memberName : UnicodeString) : Boolean;
begin
   Result := True;
end;

// HasMethod
//
function TdwsJSConnectorType.HasMethod(const methodName : String; const params : TConnectorParamArray;
                                       var typSym : TTypeSymbol) : IConnectorCall;
begin
   typSym:=FTable.FindTypeSymbol(SYS_VARIANT, cvMagic);
   Result:=TdwsJSConnectorCall.Create(methodName);
end;

// HasMember
//
function TdwsJSConnectorType.HasMember(const memberName : String; var typSym : TTypeSymbol;
                                       isWrite : Boolean) : IConnectorMember;
begin
   typSym:=FTable.FindTypeSymbol(SYS_VARIANT, cvMagic);
   Result:=TdwsJSConnectorMember.Create(memberName);
end;

// HasIndex
//
function TdwsJSConnectorType.HasIndex(const propName : String; const params : TConnectorParamArray;
                                      var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
begin
   typSym:=FTable.FindTypeSymbol(SYS_VARIANT, cvMagic);
   Result:=TdwsJSIndexCall.Create(propName, isWrite);
end;

// HasEnumerator
//
function TdwsJSConnectorType.HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
begin
   typSym:=FTable.FindTypeSymbol(SYS_STRING, cvMagic);
   Result:=TdwsJSConnectorEnumerator.Create;
end;

// HasCast
//
function TdwsJSConnectorType.HasCast(typSym: TTypeSymbol) : IConnectorCast;
begin
   typSym:=typSym.UnAliasedType;
   if typSym is TArraySymbol then
      Result:=TdwsJSConnectorCast.Create
   else Result:=nil;
end;

// ------------------
// ------------------ TdwsJSConnectorCall ------------------
// ------------------

// Create
//
constructor TdwsJSConnectorCall.Create(const methodName : String);
begin
   inherited Create;
   FMethodName:=methodName;
end;

// Call
//
function TdwsJSConnectorCall.Call(const base : Variant; const args : TConnectorArgs) : TData;
begin
   Assert(False, 'Not executable');
end;

// NeedDirectReference
//
function TdwsJSConnectorCall.NeedDirectReference : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TdwsJSIndexCall ------------------
// ------------------

// Create
//
constructor TdwsJSIndexCall.Create(const methodName : String; isWrite : Boolean);
begin
   inherited Create(methodName);
   Self.IsWrite:=isWrite;
end;

// ------------------
// ------------------ TdwsJSConnectorMember ------------------
// ------------------

// Create
//
constructor TdwsJSConnectorMember.Create(const memberName : String);
begin
   inherited Create;
   FMemberName:=memberName;
end;

// Read
//
function TdwsJSConnectorMember.Read(const base : Variant) : TData;
begin
   Assert(False, 'Not executable');
end;

// Write
//
procedure TdwsJSConnectorMember.Write(const base : Variant; const data : TData);
begin
   Assert(False, 'Not executable');
end;

// ------------------
// ------------------ TdwsJSConnectorEnumerator ------------------
// ------------------

// NewEnumerator
//
function TdwsJSConnectorEnumerator.NewEnumerator(const base : Variant; const args : TConnectorArgs) : IUnknown;
begin
   Assert(False, 'Not executable');
   Result:=nil;
end;

// Step
//
function TdwsJSConnectorEnumerator.Step(const enumerator : IInterface; var data : TData) : Boolean;
begin
   Assert(False, 'Not executable');
   Result:=False;
end;

// ------------------
// ------------------ TJSConnectorSymbol ------------------
// ------------------

// IsCompatible
//
function TJSConnectorSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited IsCompatible(typSym)
           or (typSym.AsFuncSymbol<>nil)
           or (typSym is TRecordSymbol)
           or (typSym is TStaticArraySymbol);
end;

// SupportsEmptyParam
//
function TJSConnectorSymbol.SupportsEmptyParam : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TdwsJSConnectorCast ------------------
// ------------------

// CastVariant
//
function TdwsJSConnectorCast.CastVariant(const base : Variant) : Variant;
begin
   // nothing
end;

end.
