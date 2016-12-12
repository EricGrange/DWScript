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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsEvaluate;

{$I dws.inc}

interface

uses
   dwsCompiler, dwsExprs, dwsErrors, dwsStrings, dwsConstExprs, dwsScriptSource,
   dwsContextMap;

type

   IdwsEvaluateExpr = interface
      ['{43410A86-3D04-4201-ABD5-02B935D6C6AF}']
      function GetExecution : IdwsProgramExecution;
      function GetRootProgram : IdwsProgram;
      function GetContextProcedure : TdwsProcedure;
      function GetExpression : TTypedExpr;
      function GetEvaluationError : Boolean;

      function ContextIsValid : Boolean;

      property Execution : IdwsProgramExecution read GetExecution;
      property RootProgram : IdwsProgram read GetRootProgram;
      property ContextProcedure : TdwsProcedure read GetContextProcedure;
      property Expression : TTypedExpr read GetExpression;
      property EvaluationError : Boolean read GetEvaluationError;
   end;

   TdwsEvaluateOption = (eoRootContext);
   TdwsEvaluateOptions = set of TdwsEvaluateOption;

   // holds and evaluated expression
   TdwsEvaluateExpr = class (TInterfacedObject, IdwsEvaluateExpr)
      private
         FExecution : IdwsProgramExecution;
         FContextProcedure : TdwsProcedure;
         FExpression : TTypedExpr;
         FEvaluationError : Boolean;

      protected
         function GetExecution : IdwsProgramExecution;
         function GetRootProgram : IdwsProgram;
         function GetContextProcedure : TdwsProcedure;
         function GetExpression : TTypedExpr;
         function GetEvaluationError : Boolean;

      public
         destructor Destroy; override;

         class function Evaluate(exec : IdwsProgramExecution; const anExpression : UnicodeString;
                                 options : TdwsEvaluateOptions = [];
                                 const scriptPos : PScriptPos = nil) : IdwsEvaluateExpr; static;

         function ContextIsValid : Boolean;

         property Execution : IdwsProgramExecution read FExecution;
         property RootProgram : IdwsProgram read GetRootProgram;
         property ContextProcedure : TdwsProcedure read FContextProcedure;
         property Expression : TTypedExpr read FExpression;
         property EvaluationError : Boolean read FEvaluationError;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TdwsCompilerForEvaluate = class (TdwsCompiler)
   end;

// ------------------
// ------------------ TdwsEvaluateExpr ------------------
// ------------------

// Evaluate
//
class function TdwsEvaluateExpr.Evaluate(exec : IdwsProgramExecution;
                                     const anExpression : UnicodeString;
                                     options : TdwsEvaluateOptions = [];
                                     const scriptPos : PScriptPos = nil) : IdwsEvaluateExpr;
var
   sourceFile : TSourceFile;
   compiler : TdwsCompilerForEvaluate;
   expr : TTypedExpr;
   resultObj : TdwsEvaluateExpr;
   contextProgram : TdwsProgram;
   sourceContext : TdwsSourceContext;
   config : TdwsConfiguration;
   gotError : Boolean;
   previousMsgCount, i : Integer;
   messageString : String;
begin
   { This will evaluate an expression by tokenizing it evaluating it in the
     Context provided. }

   gotError:=False;
   expr:=nil;
   compiler := TdwsCompilerForEvaluate.Create;
   try
      if exec=nil then begin
         config:=TdwsConfiguration.Create(nil);
         try
            exec:=compiler.Compile('', config).CreateNewExecution;
         finally
            config.Free;
         end;
      end;
      if (eoRootContext in options) then
         contextProgram:=exec.Prog.ProgramObject
      else begin
         contextProgram:=TdwsProgram((exec.ExecutionObject as TdwsProgramExecution).CurrentProg);
         if contextProgram=nil then
            contextProgram:=exec.Prog.ProgramObject;
      end;
      compiler.AttachContextProgram(contextProgram);
      try
         previousMsgCount := contextProgram.CompileMsgs.Count;

         sourceFile:=TSourceFile.Create;
         try
            sourceFile.Code:=anExpression;
            sourceFile.Name:=MSG_MainModule;
            compiler.AttachTokenizer(compiler.TokenizerRules.CreateTokenizer(compiler.Msgs, nil));
            try
               compiler.Tokenizer.BeginSourceFile(sourceFile);
               try
                  if scriptPos<>nil then begin
                     sourceContext:=compiler.SourceContextMap.FindContext(scriptPos^);
                     while sourceContext<>nil do begin
                        if sourceContext.LocalTable<>nil then begin
                           compiler.CurrentProg.EnterSubTable(sourceContext.LocalTable);
                           Break;
                        end;
                        sourceContext:=sourceContext.Parent;
                     end;
                  end else sourceContext:=nil;
                  try
                     expr := compiler.ReadExpr;
                  finally
                     if sourceContext<>nil then
                        compiler.CurrentProg.LeaveSubTable;
                  end;
               except
                  gotError:=True;
               end;
               if compiler.Msgs.Count > previousMsgCount then begin
                  gotError := True;
                  messageString := compiler.Msgs[previousMsgCount].AsInfo;
                  for i := previousMsgCount+1 to compiler.Msgs.Count-1 do begin
                     messageString := messageString + #13#10
                                    + compiler.Msgs[i].AsInfo;
                  end;
                  expr := TConstStringExpr.Create(contextProgram.Root.CompilerContext.TypString,
                                                  messageString);
               end;
               while compiler.Msgs.Count > previousMsgCount do
                  compiler.Msgs.Delete(compiler.Msgs.Count-1);
            finally
               compiler.Tokenizer.EndSourceFile;
               compiler.DetachTokenizer.Free;
            end;
         finally
            sourceFile.Free;
         end;
      finally
         compiler.DetachContextProgram;
      end;
   finally
      compiler.Free;
   end;

   resultObj:=TdwsEvaluateExpr.Create;
   resultObj.FExecution:=exec;
   if contextProgram is TdwsProcedure then
      resultObj.FContextProcedure:=TdwsProcedure(contextProgram);
   resultObj.FExpression:=expr;
   resultObj.FEvaluationError:=gotError;
   Result:=resultObj;
end;

// ------------------
// ------------------ TdwsEvaluateExpr ------------------
// ------------------

// Destroy
//
destructor TdwsEvaluateExpr.Destroy;
begin
   FExpression.Free;
   inherited;
end;

// ContextIsValid
//
function TdwsEvaluateExpr.ContextIsValid : Boolean;
begin
   Result:=(FContextProcedure=(FExecution.ExecutionObject as TdwsProgramExecution).CurrentProg);
end;

// GetExecution
//
function TdwsEvaluateExpr.GetExecution : IdwsProgramExecution;
begin
   Result:=FExecution;
end;

// GetRootProgram
//
function TdwsEvaluateExpr.GetRootProgram : IdwsProgram;
begin
   Result:=FExecution.Prog;
end;

// GetContextProcedure
//
function TdwsEvaluateExpr.GetContextProcedure : TdwsProcedure;
begin
   Result:=FContextProcedure;
end;

// GetExpression
//
function TdwsEvaluateExpr.GetExpression : TTypedExpr;
begin
   Result:=FExpression;
end;

// GetEvaluationError
//
function TdwsEvaluateExpr.GetEvaluationError : Boolean;
begin
   Result:=FEvaluationError;
end;

end.
