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

{$I dws.inc}

unit dwsJSFilter;

interface

uses Classes, dwsComp, dwsCompiler, dwsErrors, dwsJSCodeGen, dwsUtils,
   dwsExprs, StrUtils, SysUtils;

type

   // TdwsJSFilter
   //
   TdwsJSFilter = class(TdwsFilter)
      private
         FPatternOpen : String;
         FPatternClose : String;
         FCodeGen : TdwsJSCodeGen;
         FCompiler : TDelphiWebScript;

      protected
         procedure SetCompiler(const val : TDelphiWebScript);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         function Process(const tText : String; msgs : TdwsMessageList) : String; override;

      published
         property Compiler : TDelphiWebScript read FCompiler write SetCompiler;
         property PatternOpen : String read FPatternOpen write FPatternOpen;
         property PatternClose : String read FPatternClose write FPatternClose;
   end;

implementation

// ------------------
// ------------------ TdwsJSFilter ------------------
// ------------------

// Create
//
constructor TdwsJSFilter.Create(AOwner: TComponent);
begin
   inherited;
   FPatternOpen:='<%pas2js';
   FPatternClose:='%>';
   FCodeGen:=TdwsJSCodeGen.Create;
end;

// Destroy
//
destructor TdwsJSFilter.Destroy;
begin
   inherited;
   FCodeGen.Free;
end;

// Process
//
function TdwsJSFilter.Process(const tText : String; msgs : TdwsMessageList) : String;

   function Convert(const dwsCode : String) : String;
   var
      prog : IdwsProgram;
   begin
      FCodeGen.Clear;

      prog:=FCompiler.Compile(dwsCode);
      if prog.Msgs.HasErrors then
         Exit(prog.Msgs.AsInfo);

      FCodeGen.CompileProgram(prog);
      Result:=FCodeGen.CompiledOutput(prog);
   end;

var
   output : TWriteOnlyBlockStream;
   input : String;
   p, start, stop : Integer;
begin
   input:=inherited Process(tText, msgs);

   if FPatternOpen='' then raise Exception.Create('PatternOpen must be defined');
   if FPatternClose='' then raise Exception.Create('PatternClose must be defined');
   if FCompiler=nil then raise Exception.Create('Compiler must be defined');

   output:=TWriteOnlyBlockStream.Create;
   try
      p:=1;
      repeat
         start:=PosEx(PatternOpen, input, p);
         if start<=0 then begin
            output.WriteSubString(input, p);
            Break;
         end else output.WriteSubString(input, p, start-p);
         start:=start+Length(PatternOpen);
         stop:=PosEx(PatternClose, input, start);
         if stop<=0 then begin
            output.WriteString(Convert(Copy(input, start, MaxInt)));
            Break;
         end else begin
            output.WriteString(Convert(Copy(input, start, stop-start)));
            p:=stop+Length(PatternClose);
         end;
      until False;

      Result:=output.ToString;
   finally
      output.Free;
   end;
end;

// SetCompiler
//
procedure TdwsJSFilter.SetCompiler(const val : TDelphiWebScript);
begin
   if Assigned(FCompiler) then
      FCompiler.RemoveFreeNotification(Self);
   FCompiler:=val;
   if Assigned(FCompiler) then
      FCompiler.FreeNotification(Self);
end;

// Notification
//
procedure TdwsJSFilter.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FCompiler) then
      FCompiler:=nil;
end;

end.
