program dwsRunner;

{$SetPEFlags $0001}

{$IFNDEF VER200} // delphi 2009
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$APPTYPE CONSOLE}

uses
   Classes, SysUtils, dwsComp, dwsCompiler, dwsExprs, dwsClassesLibModule,
   dwsMathFunctions, dwsStringFunctions, dwsTimeFunctions, dwsVariantFunctions,
   dwsXPlatform;

var
   source : string;
   script : TDelphiWebScript;
   classesLibModule : TdwsClassesLib;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   i : Integer;
   params : array of Variant;
begin
   if ParamCount<1 then begin
      Writeln('dwsRunner <sourcefile> [param1] [param2] ... [paramN]');
      Exit;
   end;
   if not FileExists(ParamStr(1)) then begin
      Writeln('File "', ParamStr(1), '" not found.');
      Exit;
   end;
   try
      script:=TDelphiWebScript.Create(nil);
      classesLibModule:=TdwsClassesLib.Create(nil);
      try
         classesLibModule.Script:=script;

         source := LoadTextFromFile(ParamStr(1));
         prog:=script.Compile(source);

         if prog.Msgs.Count>0 then begin
            Writeln(prog.Msgs.AsInfo);
         end else begin
            SetLength(params, ParamCount-1);
            for i:=2 to ParamCount do
               params[i-2]:=ParamStr(i);
            exec:=prog.ExecuteParam(params);
            Writeln(exec.Result.ToString);
            if exec.Msgs.Count>0 then
               Writeln(exec.Msgs.AsInfo);
         end;
      finally
         classesLibModule.Free;
         script.Free;
      end;
   except
      on E: Exception do
         Writeln(E.ClassName, ': ', E.Message);
   end;
end.
