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
unit dwsCompilerUtils;

interface

uses
   dwsSymbols, dwsUnitSymbols;

type

   TdwsCompilerUtils = class
      public
         class procedure AddProcHelper(const name : String;
                                       table : TSymbolTable; func : TFuncSymbol;
                                       unitSymbol : TUnitMainSymbol); static;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// AddProcHelper
//
class procedure TdwsCompilerUtils.AddProcHelper(const name : String;
                                                table : TSymbolTable; func : TFuncSymbol;
                                                unitSymbol : TUnitMainSymbol);
var
   helper : THelperSymbol;
   param : TParamSymbol;
   meth : TAliasMethodSymbol;
   i : Integer;
   sym : TSymbol;
begin
   param:=func.Params[0];

   // find a local anonymous helper for the 1st parameter's type
   helper:=nil;
   for sym in table do begin
      if     (sym.Name='')
         and (sym.ClassType=THelperSymbol)
         and (THelperSymbol(sym).ForType=param.Typ) then begin
         helper:=THelperSymbol(sym);
         Break;
      end;
   end;

   // create anonymous helper if necessary
   if helper=nil then begin
      helper:=THelperSymbol.Create('', unitSymbol, param.Typ, table.Count);
      table.AddSymbol(helper);
   end;

   // create helper method symbol
   meth:=TAliasMethodSymbol.Create(name, func.Kind, helper, cvPublic, False);
   meth.SetIsStatic;
   meth.Typ:=func.Typ;
   for i:=0 to func.Params.Count-1 do
      meth.Params.AddSymbol(func.Params[i].Clone);
   meth.Alias:=func;
   helper.AddMethod(meth);
end;

end.
