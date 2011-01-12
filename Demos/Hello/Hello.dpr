program Hello;

uses
   Dialogs,
   dwsComp,             // this is where TDelphiWebScript is found
   dwsExprs,            // this is where TdwsProgram is found
   dwsVCLGUIFunctions;  // registers the ShowMessaqe internal function

procedure DwsHello;
var
   dws : TDelphiWebScript;
   prog : IdwsProgram;
begin
   // create the compiler component
   // internal functions (like ShowMessage) register at a global level
   // specific functions on the other hand are handled via TdwsUnit, and have
   // to be linked to each compiler (we don't have any in this sample)
   dws:=TDelphiWebScript.Create(nil);
   try
      // compiles the script into a program
      prog:=dws.Compile( 'var s : String = ''Hello World!'';'#13#10
                        +'ShowMessage(s);');

      // if there were errors, hints or warnings, you'll find them in the Msgs
      if prog.Msgs.Count=0 then begin

         // no compilation problem, we can run the script
         prog.Execute;
         // in a more complex case, you may want to protect execution with
         // a try-except, to catch script exception (done in the script with a raise)
         // or your own exception (happening in Delphi code invoked from the script)

      end else begin

         // display the compilation problems
         // you'll have to introduce errors in the above script to get there ;)
         ShowMessage(prog.Msgs.AsInfo);

      end;
   finally
      dws.Free;
   end;
end;

begin
   DwsHello;
end.
