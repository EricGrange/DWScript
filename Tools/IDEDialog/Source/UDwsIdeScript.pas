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
{    The Initial Developer of the Original DWS Code is Matthias        }
{    Ackermann.                                                        }
{    For other initial contributors, see DWS contributors.txt          }
{    Ackermann.                                                        }
{    DWS code is currently maintained by Eric Grange.                  }
{                                                                      }
{    Current maintainer of the IDE utility: Brian Frost                }
{                                                                      }
{**********************************************************************}
unit DwsIdeScript;

interface

uses
  dwsComp,
  dwsCompiler,
  dwsUtils,
  dwsErrors,
  dwsExprs,
  dwsVCLGUIFunctions,
  dwsFunctions,
  SysUtils,
  Classes;

type
  EDwsIdeScript = class( Exception );

  IDwsIdeScriptNotifications = interface
    ['{9E9F33AC-C485-4449-83B6-5A80240AB058}']
    // IDwsIdeScriptNotifications
    // -------------------------------------------------------------------------
    procedure DwsIdeScript_NotifyAfterCompile( AMessageList : TdwsMessageList );
    // -------------------------------------------------------------------------
  end;

  IInfo = dwsExprs.IInfo;
  TSymbolPosition = dwsExprs.TSymbolPosition;

  IFunctionInfo = IInfo;
  IFunctionResult = IInfo;

  TExecutableLineCallback = procedure( ALineNum : integer ) of object;


  // This is a list of clients to notify from script events.
  // The IDE will be one such client.
  TNotificationClients = class( TInterfaceList )
  private
    function GetItem( AIndex : integer ) : IDwsIdeScriptNotifications;
    procedure SetItem( AIndex : integer; AValue : IDwsIdeScriptNotifications );
  public
    property Items[ AIndex : integer ] : IDwsIdeScriptNotifications
      read GetItem
      write SetItem; default;

    procedure AddClient( AClient : IDwsIdeScriptNotifications );
    procedure RemoveClient( AClient : IDwsIdeScriptNotifications );

    procedure CallClients_NotifyAfterCompile( AMessageList : TdwsMessageList );
  end;


  // This is the container for the actual script. It is designed to be instantiated
  // in the application wherever required and to not need an IDE.
  TDwsIdeScript = class( TObject )
  PRIVATE

    // The 'main module' of the script.
    FScriptSource     : string;

    // The actual script engine
    FScript           : TDelphiWebScript;

    // Compiler results so that we can call execution without recompiling each time.
    FProgram          : IdwsProgram;
    FProgramExecution : IdwsProgramExecution;

    // Info sent back to clients as required
    FNotifications    : TNotificationClients;

    // location of the script files
    FScriptFileFolder : string;

    procedure NeedCompile;
    procedure NeedProgramExecution;

    function  DoOnNeedUnit(const unitName: string; var unitSource: string): IdwsUnit;

    function  GetIsCompiled: boolean;
    procedure SetIsCompiled(const Value: boolean);
    procedure SetScriptSource(const Value: string);

  PROTECTED

    function    GetScriptFileFolder : string; virtual;
    procedure   SetScriptFileFolder(const Value: string); virtual;

  PUBLIC
    constructor Create;
    destructor  Destroy; override;

    procedure   Compile( ABuild : boolean = False );
    procedure   Execute;
    procedure   GetExecutableLines( const AUnitName : string; ACallback : TExecutableLineCallback );

    property    IsCompiled : boolean
                  read GetIsCompiled
                  write SetIsCompiled;

    property    Notifications : TNotificationClients
                  read FNotifications;

    function    GetFunctionInfo( const AFunctionName : string ) : IFunctionInfo;
    function    CallFunction( AFunctionInfo : IFunctionInfo; AParams : array of variant ) : IFunctionResult;

    procedure   BeginProgram;
    procedure   EndProgram;

    procedure   SymbolsToStrings( AStrings : TStrings );

    function    ExpandScriptFileName(const AFileJustName: string): string;

    property    ScriptFileFolder : string
                  read  GetScriptFileFolder
                  write SetScriptFileFolder;

    property    ScriptProgram  : IdwsProgram
                  read FProgram;

    property    ScriptSource     : string
                  read FScriptSource
                  write SetScriptSource;
  end;


const
  sMainModule = '*MainModule*';

//function DwsIdeScript : TDwsIdeScript;

implementation


uses
  dwsSymbols;


{ TDwsIdeScript }

constructor TDwsIdeScript.Create;
begin
  inherited;

  FScript := TDelphiWebScript.Create( nil );
  FScript.OnNeedUnit := DoOnNeedUnit;
  FScript.Config.CompilerOptions :=
    FScript.Config.CompilerOptions + [coSymbolDictionary];

  // Set the script directory and create it if required
  ScriptFileFolder := 'c:\scratch';
  If not ForceDirectories( ScriptFileFolder ) then
    raise EDwsIdeScript.CreateFmt( 'Unable to create folder "%s"', [ScriptFileFolder]);

end;

destructor TDwsIdeScript.Destroy;
begin
  FreeAndNil( FScript );
  inherited;
end;



procedure TDwsIdeScript.Execute;
//var
//  ExecuteInfo : IdwsProgramExecution;
begin
  Assert( False );
(*  If not IsCompiled then
    Compile;
  If Assigned( FProgram ) then
    begin
      ExecuteInfo := FProgram.Execute;

      if ExecuteInfo.Msgs.Count > 0 then
        ShowMessage(ExecuteInfo.Msgs.AsInfo);
    end; *)
end;


function TDwsIdeScript.ExpandScriptFileName(const AFileJustName: string): string;
begin
  Result := ScriptFileFolder +  AFileJustName;
end;


procedure TDwsIdeScript.GetExecutableLines( const AUnitName : string; ACallBack : TExecutableLinecallback );
var
  I : integer;
  symPos : TSymbolPosition;
begin
  NeedCompile;

  if FProgram.SymbolDictionary.Count = 0 then
    raise EDwsIdeScript.Create( 'No symbols in symbol dictionary' );

  For I := 0 to FProgram.SymbolDictionary.Count-1 do
    begin
    symPos := FProgram.SymbolDictionary.FindSymbolPosList(
      FProgram.SymbolDictionary[I].Symbol ).FindUsage( suReference);
    if symPos <> nil then
      If (AUnitName = sMainModule) and symPos.ScriptPos.IsMainModule then
        ACallback( symPos.ScriptPos.Line )
       else
        if symPos.ScriptPos.IsSourceFile( AUnitName ) then
           ACallback( symPos.ScriptPos.Line );
    end;

end;

function TDwsIdeScript.GetFunctionInfo(const AFunctionName: string): IFunctionInfo;
begin
  NeedCompile;
  NeedProgramExecution;
  Result := FProgramExecution.Info.Func[AFunctionName];
end;

function TDwsIdeScript.GetIsCompiled: boolean;
begin
  Result := Assigned( FProgram );
end;

function TDwsIdeScript.GetScriptFileFolder: string;
begin
  Result := FScriptFileFolder;
end;

procedure TDwsIdeScript.NeedCompile;
begin
  if not IsCompiled then
    raise EDwsIdeScript.Create( 'Script is not compiled' );
end;

procedure TDwsIdeScript.BeginProgram;
begin
  NeedCompile;
  FProgramExecution := FProgram.CreateNewExecution;
  FProgramExecution.BeginProgram;
end;

procedure TDwsIdeScript.EndProgram;
begin
  FProgramExecution.EndProgram;
  FProgramExecution := nil;
end;



procedure TDwsIdeScript.NeedProgramExecution;
begin
  if FProgramExecution = nil then
    raise EDwsIdeScript.Create( 'BeginProgram not called' );
end;


function TDwsIdeScript.DoOnNeedUnit(const unitName: string;
  var unitSource: string): IdwsUnit;
var
  SL : TStrings;
  sFileName : string;
begin
  SL := TStringList.Create;
  try
    sFileName := Format( '%s%s.pas', [ScriptFileFolder, UnitName ] );
    SL.LoadFromFile( sFileName );
    UnitSource := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TDwsIdeScript.SetIsCompiled(const Value: boolean);
begin
  FProgram := nil;
end;

procedure TDwsIdeScript.SetScriptFileFolder(const Value: string);
begin
  FScript.Config.ScriptPaths.Clear;
  FScriptFileFolder := Value;
  FScript.Config.ScriptPaths.Add( GetScriptFileFolder );
end;

procedure TDwsIdeScript.SetScriptSource(const Value: string);
begin
  if Value <> FScriptSource then
    begin
    FScriptSource := Value;
    FProgram := nil; // not compiled now
    end;
end;

procedure TDwsIdeScript.SymbolsToStrings(AStrings: TStrings);

  procedure AddSymbolTable( ATable : TSymbolTable );
  var
    I : integer;
    Sym : TSymbol;
  begin
    for I := 0 to ATable.Count-1 do
      begin
      Sym := ATable.Symbols[I];
      if Sym is TUnitSymbol then
        AddSymbolTable( TUnitSymbol(Sym).Table )
       else
        AStrings.Add( Sym.Name + '   ' + Sym.ToString +  '  (' + Sym.ClassName + ')' );
      end;
  end;

begin
  If not IsCompiled then
    Compile;

  AddSymbolTable( FProgram.Table );
end;



function TDwsIdeScript.CallFunction(AFunctionInfo: IInfo; AParams : array of variant): IInfo;
begin
  Result := AFunctionInfo.Call( AParams );
end;

procedure TDwsIdeScript.Compile( ABuild : boolean = False );
begin
  if ABuild or not IsCompiled then
    begin
    FProgram := FScript.Compile( FScriptSource );

    if FProgram.Msgs.Count = 0 then // no errors
      Notifications.CallClients_NotifyAfterCompile( nil )
     else
       begin
       Notifications.CallClients_NotifyAfterCompile( FProgram.Msgs );
       FProgram := nil;
       end
    end;
end;




{ TNotificationClients }

procedure TNotificationClients.AddClient(  AClient: IDwsIdeScriptNotifications);
begin
  If Assigned( AClient ) then
    If IndexOf( AClient ) = -1 then
       Add( AClient );
end;

procedure TNotificationClients.CallClients_NotifyAfterCompile( AMessageList : TdwsMessageList );
var
  I : integer;
begin
  For I := 0 to Count-1 do
    Items[I].DwsIdeScript_NotifyAfterCompile( AMessageList );
end;

function TNotificationClients.GetItem( AIndex: integer): IDwsIdeScriptNotifications;
begin
  Result := Get(AIndex) as IDwsIdeScriptNotifications;
end;

procedure TNotificationClients.RemoveClient(  AClient: IDwsIdeScriptNotifications);
begin
  Remove( AClient );
end;

procedure TNotificationClients.SetItem(AIndex: integer; AValue: IDwsIdeScriptNotifications);
begin
  Put( AIndex, AValue );
end;

end.