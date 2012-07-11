type
   TTest = class
      Field : Integer;
      
      function GetField : Integer;
      begin
         Result:=Field;
      end;
      
      procedure SetField(v : Integer);
      begin
         Field:=v;
      end;
      
      function GetArray(i : Integer) : Integer;
      begin
         Result:=i;
      end;

      procedure SetArray(i, v : Integer);
      begin
         Field:=i+v;
      end;

      property PropDirect : Integer read Field write Field; deprecated 'direct is deprecated';
      property PropIndirect : Integer read GetField write SetField; deprecated;
      property PropArray[i : Integer] : Integer read GetArray write SetArray; default; deprecated 'array';
      property PropArray1 : Integer index 1 read GetArray write SetArray; deprecated 'array';   
   end;

var t = new TTest;
var i : Integer;

t.PropDirect:=i;
i:=t.PropDirect;

t.PropIndirect:=i;
i:=t.PropIndirect;

t.PropArray[1]:=i;
i:=t.PropArray[1];

t[2]:=i;
i:=t[2];

t.PropArray1:=i;
i:=t.PropArray1;
