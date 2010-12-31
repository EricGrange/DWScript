program dwsRunner;

{$IFNDEF VER200} // delphi 2009
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$APPTYPE CONSOLE}

uses
   Classes, SysUtils, dwsComp, dwsCompiler, dwsExprs, dwsClassesLibModule,
   dwsMathFunctions, dwsStringFunctions, dwsTimeFunctions, dwsVariantFunctions;

var
   sl : TStringList;
   script : TDelphiWebScript;
   classesLibModule : TdwsClassesLib;
   prog : TdwsProgram;
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
      sl:=TStringList.Create;
      try
         classesLibModule.Script:=script;

         sl.LoadFromFile(ParamStr(1));
         prog:=script.Compile(sl.Text);
         try
            if prog.Msgs.Count>0 then begin
               Writeln(prog.Msgs.AsInfo);
            end else begin
               SetLength(params, ParamCount-1);
               for i:=2 to ParamCount do
                  params[i-2]:=ParamStr(i);
               prog.ExecuteParam(params);
               Writeln((prog.Result as TdwsDefaultResult).Text);
               if prog.Msgs.Count>0 then
                  Writeln(prog.Msgs.AsInfo);
            end;
         finally
            prog.Free;
         end;
      finally
         sl.Free;
         classesLibModule.Free;
         script.Free;
      end;
   except
      on E: Exception do
         Writeln(E.ClassName, ': ', E.Message);
   end;
end.
