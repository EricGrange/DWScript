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
unit dwsJSSymbolWriters;

{$I dws.inc}

interface

uses
   SysUtils, Math,
   dwsUtils, dwsDataContext, dwsJSON, dwsSymbols,
   dwsCodeGen, dwsCodeGenWriters;

type

   TJSBaseNumberSymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
   end;
   TJSBaseIntegerSymbol = class (TJSBaseNumberSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSBaseFloatSymbol = class (TJSBaseNumberSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSBaseStringSymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSBaseBooleanSymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSBaseVariantSymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;

   TJSEnumerationSymbol = class (TJSBaseIntegerSymbol)
      procedure WriteDefaultValue(sym : TSymbol); override;
   end;
   TJSSetOfSymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;

   TJSPointerSymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
   end;
   TJSNilSymbol = class (TJSPointerSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSClassSymbol = class (TJSPointerSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSClassOfSymbol = class (TJSPointerSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSInterfaceSymbol = class (TJSPointerSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSFuncSymbol = class (TJSPointerSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSSourceFuncSymbol = class (TJSPointerSymbol)
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;

   TJSRecordSymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;

   TJSStaticArraySymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSDynamicArraySymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;
   TJSAssociativeArraySymbol = class (TdwsCodeGenSymbolWriter)
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
   end;

implementation

const
   cBoolToJSBool : array [False..True] of String = ('false', 'true');
   cFormatSettings : TFormatSettings = ( DecimalSeparator : '.' );
   cInlineStaticArrayLimit = 20;

// ------------------
// ------------------ TJSBaseNumberSymbol ------------------
// ------------------

procedure TJSBaseNumberSymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteString('0');
end;

// ------------------
// ------------------ TJSBaseIntegerSymbol ------------------
// ------------------

procedure TJSBaseIntegerSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
begin
   CodeGen.WriteInteger(dataPtr.AsInteger[0]);
end;

// ------------------
// ------------------ TJSBaseFloatSymbol ------------------
// ------------------

procedure TJSBaseFloatSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
begin
   CodeGen.WriteFloat(dataPtr.AsFloat[0], cFormatSettings);
end;

// ------------------
// ------------------ TJSBaseStringSymbol ------------------
// ------------------

procedure TJSBaseStringSymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteString('""');
end;

procedure TJSBaseStringSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   s : String;
begin
   dataPtr.EvalAsString(0, s);
   CodeGen.WriteLiteralString(s);
end;

// ------------------
// ------------------ TJSBaseBooleanSymbol ------------------
// ------------------

procedure TJSBaseBooleanSymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteString(cBoolToJSBool[False]);
end;

procedure TJSBaseBooleanSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
begin
   CodeGen.WriteString(cBoolToJSBool[dataPtr.AsBoolean[0]])
end;

// ------------------
// ------------------ TJSBaseVariantSymbol ------------------
// ------------------

procedure TJSBaseVariantSymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteString('undefined');
end;

procedure TJSBaseVariantSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   v : Variant;
begin
   dataPtr.EvalAsVariant(0, v);
   CodeGen.WriteVariant(sym.Typ, v);
end;

// ------------------
// ------------------ TJSEnumerationSymbol ------------------
// ------------------

procedure TJSEnumerationSymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteInteger((sym as TEnumerationSymbol).DefaultValue);
end;

// ------------------
// ------------------ TJSSetOfSymbol ------------------
// ------------------

procedure TJSSetOfSymbol.WriteDefaultValue(sym : TSymbol);
var
   i : Integer;
   setOfSym : TSetOfSymbol;
begin
   setOfSym := sym as TSetOfSymbol;
   CodeGen.WriteString('[0');
   for i := 1 to (setOfSym.CountValue div 32) do
      CodeGen.WriteString(',0');
   CodeGen.WriteString(']');
end;

procedure TJSSetOfSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   i : Integer;
   setOfSym : TSetOfSymbol;
begin
   setOfSym := sym as TSetOfSymbol;
   CodeGen.WriteString('[');
   for i := 0 to (setOfSym.CountValue div 32) do begin
      if i>0 then
         CodeGen.WriteString(',');
      if (i and 1)=0 then
         CodeGen.WriteInteger(dataPtr.AsInteger[i shr 1] and $FFFFFFFF)
      else CodeGen.WriteInteger(dataPtr.AsInteger[i shr 1] shr 32);
   end;
   CodeGen.WriteString(']');
end;

// ------------------
// ------------------ TJSPointerSymbol ------------------
// ------------------

procedure TJSPointerSymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteString('null');
end;

// ------------------
// ------------------ TJSNilSymbol ------------------
// ------------------

procedure TJSNilSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
begin
   CodeGen.WriteString('null');
end;

// ------------------
// ------------------ TJSClassSymbol ------------------
// ------------------

procedure TJSClassSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   intf : IInterface;
begin
   intf := dataPtr.AsInterface[0];
   if intf = nil then
      CodeGen.WriteString('null')
   else raise ECodeGenUnsupportedSymbol.Create('Non nil class symbol');
end;

// ------------------
// ------------------ TJSClassOfSymbol ------------------
// ------------------

procedure TJSClassOfSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   v : Variant;
   classSymbol : TClassSymbol;
begin
   dataPtr.EvalAsVariant(0, v);
   case VariantType(v) of
      varNull, varEmpty :
         CodeGen.WriteString('null');
      varUnknown : begin
         Assert(TVarData(v).VUnknown=nil);
         CodeGen.WriteString('null');
      end;
      varInt64 : begin
         if TVarData(v).VInt64=0 then
            CodeGen.WriteString('null')
         else begin
            classSymbol := TObject(TVarData(v).VInt64) as TClassSymbol;
            CodeGen.WriteString(CodeGen.SymbolMappedName(classSymbol, cgssGlobal));
         end;
      end;
   else
      raise ECodeGenException.CreateFmt('Unsupported VarType %d in TJSClassOfSymbol.WriteValue', [ VariantType(v) ]);
   end;
end;

// ------------------
// ------------------ TJSInterfaceSymbol ------------------
// ------------------

procedure TJSInterfaceSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   intf : IInterface;
begin
   intf := dataPtr.AsInterface[0];
   if intf = nil then
      CodeGen.WriteString('null')
   else raise ECodeGenUnsupportedSymbol.Create('Non nil interface symbol');
end;

// ------------------
// ------------------ TJSFuncSymbol ------------------
// ------------------

procedure TJSFuncSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
begin
   raise ECodeGenUnsupportedSymbol.Create('Generic function pointer symbol');
end;

// ------------------
// ------------------ TJSSourceFuncSymbol ------------------
// ------------------

procedure TJSSourceFuncSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   intf : IInterface;
begin
   intf := dataPtr.AsInterface[0];
   if intf = nil then
      CodeGen.WriteString('null')
   else raise ECodeGenUnsupportedSymbol.Create('Non nil function pointer symbol');
end;

// ------------------
// ------------------ TJSRecordSymbol ------------------
// ------------------

procedure TJSRecordSymbol.WriteDefaultValue(sym : TSymbol);
var
   recSym : TRecordSymbol;
   comma : Boolean;
   i : Integer;
   member : TFieldSymbol;
begin
   recSym := sym as TRecordSymbol;
   CodeGen.WriteString('{');
   comma := False;
   for i := 0 to recSym.Members.Count-1 do begin
      if not (recSym.Members[i] is TFieldSymbol) then continue;
      if comma then
         CodeGen.WriteString(',')
      else comma := True;
      member := TFieldSymbol(recSym.Members[i]);
      CodeGen.WriteString(CodeGen.SymbolMappedName(member, cgssGlobal));
      CodeGen.WriteString(':');
      Owner.WriteDefaultValue(member.Typ);
   end;
   CodeGen.WriteString('}');
end;

procedure TJSRecordSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   recSym : TRecordSymbol;
   comma : Boolean;
   i : Integer;
   member : TFieldSymbol;
   memberData : IDataContext;
begin
   recSym := sym as TRecordSymbol;
   CodeGen.WriteString('{');
   comma := False;
   for i := 0 to recSym.Members.Count-1 do begin
      if recSym.Members[i].ClassType <> TFieldSymbol then continue;
      member := TFieldSymbol(recSym.Members[i]);
      if (recSym.Name = '') and (member.Visibility <> cvPublished) then continue;
      if comma then
         CodeGen.WriteString(',')
      else comma := True;
      CodeGen.WriteString(CodeGen.SymbolMappedName(member, cgssGlobal));
      CodeGen.WriteString(':');
      dataPtr.CreateOffset(member.Offset, memberData);
      Owner.WriteValue(member.Typ, memberData);
   end;
   CodeGen.WriteString('}');
end;

// ------------------
// ------------------ TJSStaticArraySymbol ------------------
// ------------------

procedure TJSStaticArraySymbol.WriteDefaultValue(sym : TSymbol);
var
   sas : TStaticArraySymbol;
   i : Integer;
   elementTyp : TTypeSymbol;
   elementWriter : TdwsCodeGenSymbolWriter;
begin
   sas := sym as TStaticArraySymbol;

   if sas.ElementCount < cInlineStaticArrayLimit then begin
      // initialize "small" static arrays inline
      elementTyp := sas.Typ.UnAliasedType;
      elementWriter := Owner.Writer(elementTyp.ClassType);
      CodeGen.WriteString('[');
      for i:=0 to sas.ElementCount-1 do begin
         if i > 0 then
            CodeGen.WriteString(',');
         elementWriter.WriteDefaultValue(elementTyp);
      end;
      CodeGen.WriteString(']');
   end else begin
      // for large static arrays, use a loop
      // this is about twice faster than Array.fill in 2019 in Chrome,
      // and also alleviates the need to clone value type items
      CodeGen.WriteString('(function(){var a=[],i=0;while(i++<' + IntToStr(sas.ElementCount) + ')a.push(');
      Owner.WriteDefaultValue(sas.Typ);
      CodeGen.WriteString(');return a})()');
   end;
end;

procedure TJSStaticArraySymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   sas : TStaticArraySymbol;
   i : Integer;
   elementTyp : TTypeSymbol;
   elementWriter : TdwsCodeGenSymbolWriter;
   locData : IDataContext;
begin
   sas := sym as TStaticArraySymbol;
   elementTyp := sas.Typ.UnAliasedType;
   elementWriter := Owner.Writer(elementTyp.ClassType);

   CodeGen.WriteString('[');
   for i := 0 to sas.ElementCount-1 do begin
      if i > 0 then
         CodeGen.WriteString(',');
      dataPtr.CreateOffset(i*elementTyp.Size, locData);
      elementWriter.WriteValue(elementTyp, locData);
   end;
   CodeGen.WriteString(']');
end;

// ------------------
// ------------------ TJSDynamicArraySymbol ------------------
// ------------------

procedure TJSDynamicArraySymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteString('[]');
end;

procedure TJSDynamicArraySymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   intf : IInterface;
begin
   intf := dataPtr.AsInterface[0];
   if IScriptDynArray(intf).ArrayLength = 0 then
      CodeGen.WriteString('[]')
   else raise ECodeGenUnsupportedSymbol.Create('Non-empty dynamic array symbol');
end;

// ------------------
// ------------------ TJSAssociativeArraySymbol ------------------
// ------------------

procedure TJSAssociativeArraySymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.WriteString('{}');
end;

// WriteValue
//
procedure TJSAssociativeArraySymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   intf : IInterface;
begin
   intf := dataPtr.AsInterface[0];
   if IScriptDynArray(intf).ArrayLength = 0 then
      CodeGen.WriteString('[]')
   else raise ECodeGenUnsupportedSymbol.Create('Non-empty associative array symbol');
end;

end.
