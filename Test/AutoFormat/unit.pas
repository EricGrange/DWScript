//
// Some // header
//

unit  Foobar ;

//------------------------------------------------------------------------------
interface
//------------------------------------------------------------------------------

uses Something , AndSomethingElse ;

type TMySubclass = class ( TMyAncestor )
   protected
      function MyMethod(param1, param2 : String) : boolean ; override ;
end;

//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------

uses  SomeMore1 , SomeMore2 ,
   SomeMore3, SomeMore4, SomeMore5, SomeMore6, SomeMore7,
   SomeMore8;

//------------------------------------------------------------------------------
function TMySubclass.MyMethod(param1, param2 : String) : boolean;
begin
   const cNbIteration = 3;
   var startTimeStampMsec := UnixTimeMSec;

   var chrono := PerformanceCounter.Create();
	while True do repeat somethingFun(param1, param2) until we_re_done;
	Log(chrono.Elapsed)
end;