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
unit dwsJSFilter;

{$I dws.inc}

interface

uses
   Classes, StrUtils, SysUtils,
   dwsComp, dwsCompiler, dwsErrors, dwsJSCodeGen, dwsCodeGen, dwsUtils, dwsFilter,
   dwsExprs, dwsScriptSource;

type

   // TdwsJSFilter
   //
   TdwsJSFilter = class(TdwsFilter)
      private
         FPatternOpen : String;
         FPatternClose : String;
         FCodeGen : TdwsJSCodeGen;
         FCompiler : TDelphiWebScript;
         FCodeGenPrefix : String;
         FCodeGenPostfix : String;
         FCustomCodeGen : TDelphiWebScript;
         FCustomCodeGenCode : TStringList;

      protected
         procedure SetCompiler(const val : TDelphiWebScript);
         function GetCodeGenOptions : TdwsCodeGenOptions;
         function GetIndentChar : Char;
         procedure SetIndentChar(const val : Char);
         function GetIndentSize : Integer;
         procedure SetIndentSize(const val : Integer);
         procedure SetCodeGenOptions(const val : TdwsCodeGenOptions);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         procedure DoCustomCodeGen(compiler : TdwsCompiler; const switchPos : TScriptPos; const code : UnicodeString);

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure CheckPatterns;

         function Process(const tText : String; msgs : TdwsMessageList) : String; override;

         function CompileToJS(var prog : IdwsProgram; const dwsCode : String;
                              const codeFileName : String = '';
                              returnProg : Boolean = False) : String;

         property CodeGen : TdwsJSCodeGen read FCodeGen;
         property IndentChar : Char read GetIndentChar write SetIndentChar;
         property IndentSize : Integer read GetIndentSize write SetIndentSize;

      published
         property Compiler : TDelphiWebScript read FCompiler write SetCompiler;
         property CodeGenOptions : TdwsCodeGenOptions read GetCodeGenOptions write SetCodeGenOptions default [];
         property PatternOpen : String read FPatternOpen write FPatternOpen;
         property PatternClose : String read FPatternClose write FPatternClose;
         property CodeGenPrefix : String read FCodeGenPrefix write FCodeGenPrefix;
         property CodeGenPostfix : String read FCodeGenPostfix write FCodeGenPostfix;
         property CustomCodeGen : TDelphiWebScript read FCustomCodeGen write FCustomCodeGen;
   end;

   EJSFilterException = class (Exception) end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsJSFilter ------------------
// ------------------

// Create
//
constructor TdwsJSFilter.Create(AOwner: TComponent);
begin
   inherited;
   FPatternOpen:='<script type="pascal">';
   FPatternClose:='</script>';
   FCodeGenPrefix:='<script>';
   FCodeGenPostfix:='</script>';
   FCodeGen:=TdwsJSCodeGen.Create;
   FCodeGen.MainBodyName:='';
   FCustomCodeGenCode:=TStringList.Create;
end;

// Destroy
//
destructor TdwsJSFilter.Destroy;
begin
   FCustomCodeGenCode.Free;
   FCodeGen.Free;
   inherited;
end;

// CheckPatterns
//
procedure TdwsJSFilter.CheckPatterns;
begin
   if FPatternOpen='' then
      raise EJSFilterException.Create('PatternOpen must be defined');
   if FPatternClose='' then
      raise EJSFilterException.Create('PatternClose must be defined');
   if FCompiler=nil then
      raise EJSFilterException.Create('Compiler must be defined');
end;

// Process
//
function TdwsJSFilter.Process(const tText : String; msgs : TdwsMessageList) : String;
var
   prog : IdwsProgram;
   lineColPos, lineOffset, colOffset : Integer;
   output : TWriteOnlyBlockStream;
   input : String;
   p, start, stop : Integer;
begin
   CheckPatterns;

   input:=inherited Process(tText, msgs);

   lineColPos:=1;
   lineOffset:=0;
   colOffset:=0;

   FCodeGen.Clear;
   try
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
            while lineColPos<start do begin
               case input[lineColPos] of
                  #10 : begin
                     colOffset:=0;
                     Inc(lineOffset);
                  end;
               else
                  Inc(colOffset);
               end;
               Inc(lineColPos);
            end;
            output.WriteString(CodeGenPrefix);
            if prog=nil then
               prog:=FCompiler.Compile('');
            if stop<=0 then begin
               output.WriteString(CompileToJS(prog, Copy(input, start, MaxInt)));
               Break;
            end else begin
               output.WriteString(CompileToJS(prog, Copy(input, start, stop-start)));
               p:=stop+Length(PatternClose);
            end;
            msgs.AddMsgs(prog.Msgs, lineOffset, colOffset);
            output.WriteString(CodeGenPostfix);
         until False;

         Result:=output.ToUnicodeString;
      finally
         output.Free;
      end;
   finally
      if FCodeGen.Context<>nil then
         FCodeGen.EndProgramSession;
   end;
end;

// CompileToJS
//
function TdwsJSFilter.CompileToJS(var prog : IdwsProgram; const dwsCode : String;
                                  const codeFileName : String = '';
                                  returnProg : Boolean = False) : String;
var
   ownProg : Boolean;
   codeGenProg : IdwsProgram;
   codeGenExec : IdwsProgramExecution;
begin
   FCodeGen.ClearOutput;

   ownProg:=False;
   FCustomCodeGenCode.Clear;
   FCompiler.Config.OnCodeGen:=DoCustomCodeGen;
   try

      if prog=nil then begin
         prog:=FCompiler.Compile(dwsCode, codeFileName);
         ownProg:=True;
      end else if dwsCode<>'' then begin
         FCompiler.RecompileInContext(prog, dwsCode);
      end;

   finally
      FCompiler.Config.OnCodeGen:=nil;
   end;

   if prog.Msgs.HasErrors then
      Exit(prog.Msgs.AsInfo);

   if FCodeGen.Context=nil then begin
      FCodeGen.BeginProgramSession(prog);
      FCodeGen.BeforeCompileProgram(prog.Table, (prog.ProgramObject as TdwsMainProgram).SystemTable.SymbolTable,
                                    (prog as TdwsProgram).UnitMains);
   end;

   try

      if (FCustomCodeGenCode.Count>0) and (CustomCodeGen<>nil) then begin

         codeGenProg:=CustomCodeGen.Compile(FCustomCodeGenCode.Text);
         if codeGenProg.Msgs.HasErrors then
            Exit(codeGenProg.Msgs.AsInfo);

         codeGenExec := codeGenProg.CreateNewExecution;
         codeGenExec.Environment := TdwsJSCodeGenEnvironment.Create(FCodeGen, codeGenExec);
         codeGenExec.BeginProgram;
         codeGenExec.RunProgram(0);

         if codeGenExec.Msgs.Count>0 then
            Exit(codeGenExec.Msgs.AsInfo);

      end;

      try

         FCodeGen.CompileProgramInSession(prog);
         Result:=FCodeGen.CompiledOutput(prog);

      finally

         if codeGenExec<>nil then begin
            codeGenExec.Environment:=nil;
            codeGenExec.EndProgram;
         end;

      end;

   finally
      if ownProg then begin
         FCodeGen.EndProgramSession;
         FCodeGen.Clear;
         if not returnProg then
            prog:=nil;
      end;
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

// GetCodeGenOptions
//
function TdwsJSFilter.GetCodeGenOptions : TdwsCodeGenOptions;
begin
   Result:=FCodeGen.Options;
end;

// GetIndentChar
//
function TdwsJSFilter.GetIndentChar : Char;
begin
   Result:=FCodeGen.IndentChar;
end;

// SetIndentChar
//
procedure TdwsJSFilter.SetIndentChar(const val : Char);
begin
   FCodeGen.IndentChar:=val;
end;

// GetIndentSize
//
function TdwsJSFilter.GetIndentSize : Integer;
begin
   Result:=FCodeGen.IndentSize;
end;

// SetIndentSize
//
procedure TdwsJSFilter.SetIndentSize(const val : Integer);
begin
   FCodeGen.IndentSize:=val;
end;

// SetCodeGenOptions
//
procedure TdwsJSFilter.SetCodeGenOptions(const val : TdwsCodeGenOptions);
begin
   FCodeGen.Options:=val;
end;

// Notification
//
procedure TdwsJSFilter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
   if (Operation=opRemove) and (AComponent=FCompiler) then
      FCompiler:=nil;
end;

// DoCustomCodeGen
//
procedure TdwsJSFilter.DoCustomCodeGen(compiler : TdwsCompiler; const switchPos : TScriptPos; const code : UnicodeString);
var
   line : Integer;
begin
   line:=switchPos.Line-1;
   while FCustomCodeGenCode.Count<line do
      FCustomCodeGenCode.Add('');
   FCustomCodeGenCode.Add(code);
end;

end.
