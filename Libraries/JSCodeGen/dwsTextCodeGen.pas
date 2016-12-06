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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsTextCodeGen;

interface

uses
   dwsCodeGen, dwsUtils, dwsJSON, dwsErrors, dwsExprs, dwsSymbols, dwsCoreExprs,
   dwsComp, dwsScriptSource;

type

   TdwsMapInfo = record
      OutputLine : Integer;
      ScriptPos : TScriptPos;
   end;
   PdwsMapInfo = ^TdwsMapInfo;
   TdwsMapInfos = array of TdwsMapInfo;

   TdwsCodeGenSourceMap = class
      private
         FInfos : TdwsMapInfos;
         FCount : Integer;
         FCapacity : Integer;

      protected
         function GetAsJSON : String;

      public
         procedure Add(outputLine : Integer; const scriptPos : TScriptPos);
         procedure Clear;

         property Infos : TdwsMapInfos read FInfos;
         property Count : Integer read FCount;

         property AsJSON : String read GetAsJSON;
   end;

   TdwsGenericCodeGenType = (gcgExpression, gcgStatement);

   TdwsExprGenericCodeGen = class(TdwsExprCodeGen)
      private
         FTemplate : array of TVarRec;
         FCodeGenType : TdwsGenericCodeGenType;
         FUnWrapable : Boolean;
         FDependency : String;

      protected
         procedure DoCodeGen(codeGen : TdwsCodeGen; expr : TExprBase; start, stop : Integer);

      public
         constructor Create(const template : array of const;
                            codeGenType : TdwsGenericCodeGenType = gcgExpression;
                            const dependency : String = ''); overload;

         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TdwsTextCodeGen = class (TdwsCodeGen)
      private
         FSourceMap : TdwsCodeGenSourceMap;

      protected
         function DoCustomCodeGen(expr : TExprBase) : TdwsExprCodeGen; override;

      public
         constructor Create; override;
         destructor Destroy; override;

         procedure Clear; override;
         procedure ClearOutput; override;

         procedure NotifyCompileExpr(expr : TExprBase); override;

         property SourceMap : TdwsCodeGenSourceMap read FSourceMap;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsCodeGenSourceMap ------------------
// ------------------

// Add
//
procedure TdwsCodeGenSourceMap.Add(outputLine : Integer; const scriptPos : TScriptPos);
begin
   if FCount=FCapacity then begin
      FCapacity:=FCapacity+8+FCapacity div 2;
      SetLength(FInfos, FCapacity);
   end;
   FInfos[FCount].OutputLine:=outputLine;
   FInfos[FCount].ScriptPos:=scriptPos;
   Inc(FCount);
end;

// Clear
//
procedure TdwsCodeGenSourceMap.Clear;
begin
   SetLength(FInfos, 0);
   FCapacity:=0;
   FCount:=0;
end;

// GetAsJSON
//
function TdwsCodeGenSourceMap.GetAsJSON : String;
var
   i : Integer;
   json : TdwsJSONWriter;
   wobs : TWriteOnlyBlockStream;
   info : PdwsMapInfo;
begin
   wobs:=TWriteOnlyBlockStream.Create;
   json:=TdwsJSONBeautifiedWriter.Create(wobs, 0, 1);
   try
      json.BeginArray;

      for i:=0 to Count-1 do begin
         info:=@Infos[i];
         json.BeginObject;
         json.WriteName('OutputLine');
         json.WriteInteger(info.OutputLine);
         json.WriteName('SrcFile');
         json.WriteString(info.ScriptPos.SourceFile.Name);
         json.WriteName('SrcLine');
         json.WriteInteger(info.ScriptPos.Line);
         json.WriteName('SrcColumn');
         json.WriteInteger(info.ScriptPos.Col);
         json.EndObject;
      end;

      json.EndArray;

      Result:=wobs.ToString;
   finally
      json.Free;
      wobs.Free;
   end;
end;

// ------------------
// ------------------ TdwsExprGenericCodeGen ------------------
// ------------------

// Create
//
constructor TdwsExprGenericCodeGen.Create(const template : array of const;
                                          codeGenType : TdwsGenericCodeGenType = gcgExpression;
                                          const dependency : String = '');
var
   i : Integer;
begin
   inherited Create;
   FCodeGenType:=codeGenType;
   SetLength(FTemplate, Length(template));
   for i:=0 to High(template) do
      FTemplate[i]:=template[i];
   if codeGenType<>gcgStatement then begin
      i:=High(template);
      FUnWrapable:=    (FTemplate[0].VType=vtWideChar) and (FTemplate[0].VWideChar='(')
                   and (FTemplate[i].VType=vtWideChar) and (FTemplate[i].VWideChar=')');
   end else FUnWrapable:=False;
   FDependency:=dependency;
end;

// CodeGen
//
procedure TdwsExprGenericCodeGen.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   DoCodeGen(codeGen, expr, 0, High(FTemplate));
end;

// CodeGenNoWrap
//
procedure TdwsExprGenericCodeGen.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   if FUnWrapable then
      DoCodeGen(codeGen, expr, 1, High(FTemplate)-1)
   else DoCodeGen(codeGen, expr, 0, High(FTemplate));
end;

// DoCodeGen
//
procedure TdwsExprGenericCodeGen.DoCodeGen(codeGen : TdwsCodeGen; expr : TExprBase; start, stop : Integer);

   function IsBoundaryChar(var v : TVarRec) : Boolean;
   begin
      Result:=    (v.VType=vtWideChar)
              and (   (v.VWideChar='(')
                   or (v.VWideChar=',')
                   or (v.VWideChar=')'));
   end;

   function IsBlockEndChar(var v : TVarRec) : Boolean;
   begin
      Result:=    (v.VType=vtWideChar)
              and (v.VWideChar='}');
   end;

var
   i, idx : Integer;
   c : Char;
   item : TExprBase;
   noWrap : Boolean;
begin
   if FDependency<>'' then
      codeGen.Dependencies.Add(FDependency);
   for i:=start to stop do begin
      case FTemplate[i].VType of
         vtInteger : begin
            idx:=FTemplate[i].VInteger;
            if idx>=0 then begin
               item:=expr.SubExpr[idx];
               noWrap:=(item is TVarExpr) or (item is TFieldExpr);
               if not noWrap then begin
                  noWrap:=    (i>start) and IsBoundaryChar(FTemplate[i-1])
                          and (i<stop) and IsBoundaryChar(FTemplate[i+1])
                          and (item is TTypedExpr);
               end;
            end else begin
               item:=expr.SubExpr[-idx];
               noWrap:=True;
            end;
            if noWrap then
               codeGen.CompileNoWrap(TTypedExpr(item))
            else codeGen.Compile(item);
         end;
         vtUnicodeString :
            codeGen.WriteString(String(FTemplate[i].VUnicodeString));
         vtWideChar : begin
            c:=FTemplate[i].VWideChar;
            case c of
               #9 : begin
                  codeGen.WriteLineEnd;
                  codeGen.Indent;
               end;
               #8 : codeGen.UnIndent;
            else
               codeGen.WriteString(c);
            end;
         end;
      else
         Assert(False);
      end;
   end;
   if FCodeGenType=gcgStatement then begin
      if IsBlockEndChar(FTemplate[stop]) then
         codeGen.WriteLineEnd
      else codeGen.WriteStatementEnd;
   end;
end;

// ------------------
// ------------------ TdwsTextCodeGen ------------------
// ------------------

// Create
//
constructor TdwsTextCodeGen.Create;
begin
   inherited;
   FSourceMap:=TdwsCodeGenSourceMap.Create;
end;

// Destroy
//
destructor TdwsTextCodeGen.Destroy;
begin
   inherited;
   FSourceMap.Free;
end;

// Clear
//
procedure TdwsTextCodeGen.Clear;
begin
   inherited;
end;

// ClearOutput
//
procedure TdwsTextCodeGen.ClearOutput;
begin
   inherited;
   FSourceMap.Clear;
end;

// NotifyCompileExpr
//
procedure TdwsTextCodeGen.NotifyCompileExpr(expr : TExprBase);
begin
   if expr.ScriptPos.Defined then
      if not (expr is TBlockExprBase) then
         FSourceMap.Add(OutputLine, expr.ScriptPos);
end;

// DoCustomCodeGen
//
function TdwsTextCodeGen.DoCustomCodeGen(expr : TExprBase) : TdwsExprCodeGen;
begin
   Result:=inherited DoCustomCodeGen(expr);
end;

end.
