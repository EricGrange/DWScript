{
   Demonstrates how to use TdwsStringResult to handle standard input/output
}
program ConsoleEcho;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  dwsComp,
  dwsExprs,
  dwsStringResult;

type

   TConsoleStringResultType = class (TdwsStringResultType)
      procedure DoAddString(result : TdwsStringResult; var str : String); override;
      procedure DoReadLn(result : TdwsStringResult; var str : String); override;
      procedure DoReadChar(result : TdwsStringResult; var str : String); override;
   end;

procedure TConsoleStringResultType.DoAddString(result : TdwsStringResult; var str : String);
begin
   Write(str);
end;

procedure TConsoleStringResultType.DoReadLn(result : TdwsStringResult; var str : String);
begin
   ReadLn(str);
end;

procedure TConsoleStringResultType.DoReadChar(result : TdwsStringResult; var str : String);
var
   c : Char;
begin
   Read(c);
   str:=c;
end;

var
   dws : TDelphiWebScript;
   prog : IdwsProgram;
begin
   dws:=TDelphiWebScript.Create(nil);
   dws.Config.ResultType:=TConsoleStringResultType.Create(dws);

   prog:=dws.Compile( 'var buf : String;'#13#10
                     +'writeln("Validate an empty line to exit");'#13#10
                     +'repeat'#13#10
                        +'buf := readln;'#13#10
                        +'writeln("Echo: " + buf);'#13#10
                     +'until buf = "";');
   prog.Execute;

   dws.Free;
end.
