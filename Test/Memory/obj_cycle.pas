type
   TChainItem = class
      Next : TChainItem;
   end;

var i1, i2, i3 : TChainItem;

i1:=TChainItem.Create;
i2:=TChainItem.Create;
i3:=TChainItem.Create;

i1.Next:=i2;
i2.Next:=i3;
i3.Next:=i1;

