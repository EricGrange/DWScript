type
   TEvent = procedure (i : Integer);

type
   TTest = class
      private 
         FEvent : TEvent;
      published
         property Event : TEvent read FEvent write FEvent;
   end;

procedure Func1(i : Integer);   
begin
   PrintLn(i);
end;

procedure Func2(i : Integer);   
begin
   PrintLn(2*i);
end;

procedure TestIt;
begin
   var t := new TTest;
   t.Event := Func1;

   t.Event(1);
      
   var rtti := RTTIRawAttributes;
   var typeID := TypeOf(TTest);

   for var i:=Low(rtti) to High(rtti) do begin
      var attrib := rtti[i];
      if (attrib.T = typeID) and (attrib.A is RTTIPropertyAttribute) then begin
         var prop := RTTIPropertyAttribute(attrib.A);
         if prop.Name='Event' then begin
            prop.Setter(t, Variant(@Func2));
         end;
      end;
   end;

   t.Event(2);
end;

TestIt;