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
unit dwsJSCodeGenModule;

interface

uses
  SysUtils, Classes, dwsComp, dwsExprs, dwsJScodeGen;

type
  TDMJSCodeGenModule = class(TDataModule)
    CodeGenLibModule: TdwsUnit;
    procedure CodeGenLibModuleFunctionsRegisterSimpleCodeGenEval(
      info: TProgramInfo);
    procedure CodeGenLibModuleClassesCodeGenMethodsDependencyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure CodeGenLibModuleClassesCodeGenMethodsNewUniqueGlobalVarEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure CodeGenLibModuleClassesCodeGenMethodsCustomDependencyEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

type
   TCodeGenHelper = class helper for TProgramInfo
      function Environment : TdwsJSCodeGenEnvironment;
   end;

// Environment
//
function TCodeGenHelper.Environment : TdwsJSCodeGenEnvironment;
begin
   Result:=(Execution.Environment.GetSelf as TdwsJSCodeGenEnvironment);
end;

procedure TDMJSCodeGenModule.CodeGenLibModuleClassesCodeGenMethodsCustomDependencyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Environment.CodeGen.RegisterCustomDependency(Info.ParamAsString[0]);
end;

procedure TDMJSCodeGenModule.CodeGenLibModuleClassesCodeGenMethodsDependencyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Environment.CodeGen.Dependencies.Add(Info.ParamAsString[0]);
end;

procedure TDMJSCodeGenModule.CodeGenLibModuleClassesCodeGenMethodsNewUniqueGlobalVarEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.Environment.CodeGen.NewUniqueGlobalVar(info.ParamAsString[0]);
end;

procedure TDMJSCodeGenModule.CodeGenLibModuleFunctionsRegisterSimpleCodeGenEval(
  info: TProgramInfo);
begin
   Info.Environment.RegisterIntercept(info.ParamAsString[0], info.ParamAsString[1], info.Params[2]);
end;

end.
