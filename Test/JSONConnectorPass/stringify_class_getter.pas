type
   TSubTest = class
      private 
         FSub : TSubTest;
         FName : String;
         function GetSub : TSubTest; begin Result := FSub; end;

      published
         property Name : String read ('<' + FName + '>') write FName;
         property Sub : TSubtest read GetSub write FSub;
   end;

type
   TTest = class
      protected
         function GetStr : String; begin Result := 'hello'; end;

      published
         property Str : String read GetStr;
         property Int : Integer read (123);
         property Bool : Boolean read (1 <> 0);
         property Num : Float read (3.14);
         property Arr : array of String read (StrSplit('abc,de', ','));
         property Sub : TSubTest;
		 property Indexed[i : Integer] : String read (i.ToString);
   end;

var i := new TTest;
i.Sub := new TSubTest;
i.Sub.Name := 'hello';
i.Sub.Sub := new TSubTest;
i.Sub.Sub.Name := 'world';


PrintLn(JSON.PrettyStringify(i));
