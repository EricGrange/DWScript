{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
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
unit dwsJSONScript;

{$I dws.inc}

interface

uses
   dwsJSON, dwsUtils,
   dwsExprList, dwsDataContext, dwsSymbols, dwsExprs, dwsStrings, dwsScriptSource;

type

   // JSONScript
   //
   JSONScript = class {static sealed}
      class procedure StringifyExpr(expr : TTypedExpr; exec : TdwsExecution; writer : TdwsJSONWriter); static;
      class procedure StringifyArgs(const args : TExprBaseListExec; var Result : String); static;

      class procedure StringifyVariant(exec : TdwsExecution; writer : TdwsJSONWriter; const v : Variant); static;
      class procedure StringifyUnknown(exec : TdwsExecution; writer : TdwsJSONWriter;
                                       const unk : IUnknown); static;
      class procedure StringifySymbol(exec : TdwsExecution; writer : TdwsJSONWriter;
                                      sym : TSymbol; const dataPtr : IDataContext); static;
      class procedure StringifyDynamicArray(exec : TdwsExecution; writer : TdwsJSONWriter;
                                            dynArray : TScriptDynamicArray); static;
      class procedure StringifyArray(exec : TdwsExecution; writer : TdwsJSONWriter; elemSym : TTypeSymbol;
                                     const dataPtr : IDataContext; nb : Integer); static;
      class procedure StringifyAssociativeArray(exec : TdwsExecution; writer : TdwsJSONWriter;
                                                assocArray : TScriptAssociativeArray); static;
      class procedure StringifyComposite(exec : TdwsExecution; writer : TdwsJSONWriter;
                                         compSym : TCompositeTypeSymbol;
                                         const dataPtr : IDataContext); static;
      class procedure StringifyClass(exec : TdwsExecution; writer : TdwsJSONWriter;
                                     const obj : IScriptObj); static;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCompilerUtils, dwsConstExprs;

// ------------------
// ------------------ JSONScript ------------------
// ------------------

// StringifyExpr
//
class procedure JSONScript.StringifyExpr(expr : TTypedExpr; exec : TdwsExecution; writer : TdwsJSONWriter);
var
   dataExpr : TDataExpr;
   v : Variant;
   dc : IDataContext;
begin
   if expr.Typ.Size=1 then begin
      expr.EvalAsVariant(exec, v);
      if expr.Typ.UnAliasedTypeIs(TRecordSymbol) or expr.Typ.UnAliasedTypeIs(TStaticArraySymbol) then begin
         exec.DataContext_CreateValue(v, dc);
         StringifySymbol(exec, writer, expr.Typ, dc);
      end else StringifyVariant(exec, writer, v);
   end else begin
      dataExpr:=(expr as TDataExpr);
      StringifySymbol(exec, writer, expr.Typ, dataExpr.DataPtr[exec]);
   end;
end;

// StringifyArgs
//
class procedure JSONScript.StringifyArgs(const args : TExprBaseListExec; var Result : String);
var
   writer : TdwsJSONWriter;
   expr : TTypedExpr;
begin
   writer:=TdwsJSONWriter.Create;
   try
      expr:=(args.ExprBase[0] as TTypedExpr);
      StringiFyExpr(expr, args.Exec, writer);
      Result:=writer.ToString;
   finally
      writer.Free;
   end;
end;

// StringifyVariant
//
class procedure JSONScript.StringifyVariant(exec : TdwsExecution; writer : TdwsJSONWriter; const v : Variant);

   procedure StringifyString(writer : TdwsJSONWriter; const v : Variant);
   var
      s : UnicodeString;
   begin
      VariantToString(v, s);
      writer.WriteString(s);
   end;

var
   p : PVarData;
begin
   p:=PVarData(@v);
   case p^.VType of
      varInt64 :
         writer.WriteInteger(p^.VInt64);
      varDouble :
         writer.WriteNumber(p^.VDouble);
      varBoolean :
         writer.WriteBoolean(p^.VBoolean);
      varNull :
         writer.WriteNull;
      varUnknown :
         if p^.VUnknown=nil then
            writer.WriteNull
         else StringifyUnknown(exec, writer, IUnknown(p^.VUnknown));
   else
      StringifyString(writer, v);
   end;
end;

// StringifyUnknown
//
class procedure JSONScript.StringifyUnknown(exec : TdwsExecution; writer : TdwsJSONWriter; const unk : IUnknown);
var
   getSelf : IGetSelf;
   selfObj : TObject;
   writeable : IJSONWriteAble;
   scriptObj : IScriptObj;
begin
   if unk = nil then

      writer.WriteNull

   else if unk.QueryInterface(IJSONWriteAble, writeable) = 0 then begin

      writeable.WriteToJSON(writer);

   end else begin

      if unk.QueryInterface(IGetSelf, getSelf)=0 then begin

         selfObj:=getSelf.GetSelf;
         if selfObj is TScriptObjInstance then begin

            scriptObj:=TScriptObjInstance(selfObj);
            StringifyClass(exec, writer, scriptObj);

         end else if selfObj is TScriptDynamicArray then begin

            StringifyDynamicArray(exec, writer, TScriptDynamicArray(selfObj))

         end else if selfObj is TScriptAssociativeArray then begin

            StringifyAssociativeArray(exec, writer, TScriptAssociativeArray(selfObj))

         end else if selfObj<>nil then begin

            writer.WriteString(selfObj.ToString)

         end else writer.WriteString('null');

      end else writer.WriteString('IUnknown');

   end;
end;

// StringifySymbol
//
class procedure JSONScript.StringifySymbol(exec : TdwsExecution; writer : TdwsJSONWriter; sym : TSymbol; const dataPtr : IDataContext);
var
   ct : TClass;
begin
   sym:=sym.BaseType;
   ct:=sym.ClassType;
   if ct.InheritsFrom(TBaseSymbol) then
      StringifyVariant(exec, writer, dataPtr[0])
   else if ct=TDynamicArraySymbol then
      StringifyDynamicArray(exec, writer, IScriptDynArray(dataPtr.AsInterface[0]).GetSelf as TScriptDynamicArray)
   else if ct.InheritsFrom(TStaticArraySymbol) then
      StringifyArray(exec, writer, TStaticArraySymbol(sym).Typ, dataPtr, TStaticArraySymbol(sym).ElementCount)
   else if ct=TRecordSymbol then
      StringifyComposite(exec, writer, TRecordSymbol(sym), dataPtr)
   else if ct=TClassSymbol then
      StringifyClass(exec, writer, IScriptObj(dataPtr.AsInterface[0]))
   else if ct=TAssociativeArraySymbol then
      StringifyAssociativeArray(exec, writer, IScriptAssociativeArray(dataPtr.AsInterface[0]).GetSelf as TScriptAssociativeArray)
   else if ct=TNilSymbol then
      writer.WriteNull
   else writer.WriteString(sym.ClassName);
end;

// StringifyArray
//
class procedure JSONScript.StringifyArray(exec : TdwsExecution;
   writer : TdwsJSONWriter; elemSym : TTypeSymbol; const dataPtr : IDataContext; nb : Integer);
var
   i, s : Integer;
   locData : IDataContext;
   unaliasedClassType : TClass;
begin
   s:=elemSym.Size;
   writer.BeginArray;
   unaliasedClassType:=elemSym.UnAliasedType.ClassType;
   if unaliasedClassType=TBaseIntegerSymbol then begin
      for i:=0 to nb-1 do
         writer.WriteInteger(dataPtr.AsInteger[i]);
   end else if unaliasedClassType=TBaseFloatSymbol then begin
      for i:=0 to nb-1 do
         writer.WriteNumber(dataPtr.AsFloat[i]);
   end else if unaliasedClassType=TBaseStringSymbol then begin
      for i:=0 to nb-1 do
         writer.WriteString(dataPtr.AsString[i]);
   end else begin
      for i:=0 to nb-1 do begin
         dataPtr.CreateOffset(i*s, locData);
         StringifySymbol(exec, writer, elemSym, locData);
      end;
   end;
   writer.EndArray;
end;

// StringifyDynamicArray
//
class procedure JSONScript.StringifyDynamicArray(exec : TdwsExecution;
   writer : TdwsJSONWriter; dynArray : TScriptDynamicArray);
var
   locData : IDataContext;
begin
   exec.DataContext_Create(dynArray.AsData, 0, locData);
   StringifyArray(exec, writer, dynArray.ElementTyp, locData, dynArray.ArrayLength);
end;

// StringifyAssociativeArray
//
class procedure JSONScript.StringifyAssociativeArray(exec : TdwsExecution;
   writer : TdwsJSONWriter; assocArray : TScriptAssociativeArray);
var
   i : Integer;
   key : TData;
   elementData : IDataContext;
   name : UnicodeString;
begin
   writer.BeginObject;
   if (assocArray.Count>0) and assocArray.KeyType.UnAliasedTypeIs(TBaseSymbol) then begin
      SetLength(key, assocArray.KeyType.Size);
      for i := 0 to assocArray.Capacity-1 do begin
         if assocArray.ReadBucket(i, key, elementData) then begin
            VariantToString(key[0], name);
            writer.WriteName(name);
            StringifySymbol(exec, writer, assocArray.ElementType, elementData);
         end;
      end;
   end;
   writer.EndObject;
end;

// StringifyComposite
//
class procedure JSONScript.StringifyComposite(exec : TdwsExecution;
   writer : TdwsJSONWriter; compSym : TCompositeTypeSymbol; const dataPtr : IDataContext);
var
   i : Integer;
//   bufData : TData;
   sym : TSymbol;
   fieldSym : TFieldSymbol;
   propSym : TPropertySymbol;
   locData : IDataContext;
begin
   writer.BeginObject;
   while compSym <> nil do begin
      for i:=0 to compSym.Members.Count-1 do begin
         sym:=compSym.Members[i];
         if sym.ClassType=TPropertySymbol then begin
            propSym:=TPropertySymbol(sym);
            if (propSym.Visibility>=cvPublished) and (propSym.ReadSym<>nil) then
               sym:=propSym.ReadSym
            else continue;
            writer.WriteName(propSym.ExternalName);
         end else if sym.ClassType=TFieldSymbol then begin
            if TFieldSymbol(sym).Visibility<cvPublished then
               continue;
            writer.WriteName(sym.Name);
         end else continue;

         if sym.ClassType=TFieldSymbol then begin
            fieldSym:=TFieldSymbol(sym);
            dataPtr.CreateOffset(fieldSym.Offset, locData);
            StringifySymbol(exec, writer, fieldSym.Typ, locData);
         end else begin
   //         SetLength(bufData, sym.Typ.Size);
            Assert(False, 'published method getters not supported yet');
         end;
      end;
      compSym := compSym.Parent;
   end;
   writer.EndObject;
end;

// StringifyClass
//
class procedure JSONScript.StringifyClass(exec : TdwsExecution;
   writer : TdwsJSONWriter; const obj : IScriptObj);
var
   stringifyMeth : TMethodSymbol;
   classSym : TClassSymbol;
   progExec : TdwsProgramExecution;
   methExpr : TFuncExprBase;
   selfExpr : TTypedExpr;
   buf : UnicodeString;
begin
   if (obj=nil) or (obj.Destroyed) then begin
      writer.WriteNull;
      Exit;
   end;
   classSym := obj.ClassSym;
   stringifyMeth := TMethodSymbol(classSym.Members.FindSymbol(SYS_JSON_STRINGIFY, cvPublic, TMethodSymbol));
   if    (stringifyMeth=nil)
      or stringifyMeth.IsClassMethod
      or (stringifyMeth.Params.Count <> 0)
      or (not stringifyMeth.Typ.UnAliasedTypeIs(TBaseStringSymbol)) then begin

      StringifyComposite(exec, writer, obj.ClassSym, obj);

   end else begin

      progExec := (exec as TdwsProgramExecution);
      methExpr := nil;
      selfExpr := TConstExpr.Create(classSym, IUnknown(obj));
      try
         methExpr := CreateMethodExpr(
            progExec.CompilerContext, stringifyMeth,
            selfExpr, rkObjRef, cNullPos, []
            );
         methExpr.EvalAsUnicodeString(exec, buf);
         writer.WriteJSON(buf);
      finally
         methExpr.Free;
         selfExpr.Free;
      end;

   end;
end;

end.
