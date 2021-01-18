{**************************************************************************}
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{     file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Copyright Eric Grange / Creative IT                                   }
{                                                                          }
{**************************************************************************}
unit dwsJITRegisterAllocator;

{$I ../dws.inc}

interface

uses
   SysUtils, dwsUtils, dwsSymbols;

type

   TRegisterStatus = record
      DataSymbol : TDataSymbol;
      Expr : TExprBase;
      Lock : Integer;

      procedure Reset;
      procedure Flush;

      function ToString : String;
   end;
   PRegisterStatus = ^TRegisterStatus;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TRegisterStatus ------------------
// ------------------

// Reset
//
procedure TRegisterStatus.Reset;
begin
   DataSymbol := nil;
   Expr := nil;
   Lock := 0;
end;

// Flush
//
procedure TRegisterStatus.Flush;
begin
   Assert(Lock = 0, ToString);
   DataSymbol := nil;
   Expr := nil;
end;

// ToString
//
function TRegisterStatus.ToString : String;
begin
   Result := 'Lock = ' + IntToStr(Lock)
           + ', DataSymbol = ' + DataSymbol.Caption
           + ', Expr = ';
   if Expr <> nil then
      Result := Result + Expr.ClassName
   else Result := Result + 'nil';
end;

end.
