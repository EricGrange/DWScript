unit FMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dwsComp, StdCtrls, dwsExprs, dwsCompiler, dwsSymbols, UPlanets;

type
  TMainForm = class(TForm)
    MECode: TMemo;
    BURun: TButton;
    MEResult: TMemo;
    DelphiWebScript: TDelphiWebScript;
    dwsUnit: TdwsUnit;
    procedure BURunClick(Sender: TObject);
    procedure dwsUnitClassesTPlanetConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsUnitClassesTPlanetMethodsGetNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTPlanetMethodsGetSatelliteEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTEarthConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsUnitClassesTMoonConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsUnitClassesTPlanetCleanUp(ExternalObject: TObject);
    procedure dwsUnitClassesTPlanetMethodsSatelliteCountEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.BURunClick(Sender: TObject);
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=DelphiWebScript.Compile(MECode.Lines.Text);

   if prog.Msgs.Count=0 then begin
      exec:=prog.Execute;
      MEResult.Lines.Text:=exec.Result.ToString;
   end else MEResult.Lines.Text:=prog.Msgs.AsInfo;
end;

procedure TMainForm.dwsUnitClassesTPlanetConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   // An alternative to that message could be to mark the constructor as
   // abstract and let an abstract exception happen
   raise Exception.Create('You can''t create a TPlanet, create a TEarth or TMoon!');
end;

procedure TMainForm.dwsUnitClassesTPlanetCleanUp(ExternalObject: TObject);
begin
   // if you created or defined Delphi-side structures during a script object
   // creation, CleanUp is the event where you should, well, clean them up

   // Detach reference
   (ExternalObject as TPlanet).ScriptObj:=Unassigned;

   // Free the external object we created in our script-side constructors
   // cf. below dwsUnitClassesTEarth/TMoonConstructorsCreateEval
   ExternalObject.Free;
end;

procedure TMainForm.dwsUnitClassesTEarthConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TEarth.Create;
end;

procedure TMainForm.dwsUnitClassesTMoonConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TMoon.Create;
end;

procedure TMainForm.dwsUnitClassesTPlanetMethodsGetNameEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TPlanet).Name;
end;

procedure TMainForm.dwsUnitClassesTPlanetMethodsGetSatelliteEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   planet : TPlanet;
   satellite : TPlanet;
begin
   planet:=ExtObject as TPlanet;
   satellite:=planet.Satellites[Info.ValueAsInteger['position']];

   if VarIsEmpty(satellite.ScriptObj) then begin
      // To create a script-side object, we basically just invoke its
      // script-side constructor while passing our Delphi-side object
      // reference to use as ExternalObject
      satellite.ScriptObj:=Info.Vars[satellite.ClassName].GetConstructor('Create', satellite).Call.Value;
   end;
   Info.ResultAsVariant:=satellite.ScriptObj;
end;

procedure TMainForm.dwsUnitClassesTPlanetMethodsSatelliteCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=(ExtObject as TPlanet).SatelliteCount;
end;

end.
