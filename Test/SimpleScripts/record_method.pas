type
   TTest = record
      private
         FHidden : String;

      public
         Field : Integer;

      private
         procedure SetHidden(v : String);
         begin
            FHidden := v;
         end;
         
         function GetIdx(i : Integer) : String;
         
      public
         property Shown : String read FHidden write SetHidden;
         property Idx[i : Integer] : String read GetIdx; default;
   
         procedure SayHello;
         
         class function NewOne(s : String) : TTest;
         begin
            Result.FHidden:=s;
            Result.Field:=123;
         end;
    end;

function TTest.GetIdx(i : Integer) : String;
begin
   Result:=Shown+IntToStr(i);
end;
    
procedure TTest.SayHello;
begin
   PrintLn('Hello '+FHidden);
end;

var t : TTest;

t.Shown:='World';
t.SayHello;

PrintLn(t.Idx[123]);
PrintLn(t[456]);

t:=TTest.NewOne('New one');
t.SayHello;
PrintLn(t.Field);
