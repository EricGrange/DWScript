type TWArray    = array of Integer;

type TItem = record
    Name:String;
    GraphicsList:TWArray;
end;

function Item( name:String; graphicsList:TWArray ):TItem; overload;
begin
    Result.Name := name;
    Result.GraphicsList := graphicsList;
end;

type TItemArray = array of TItem;

var
    SET_S:TItemArray;

begin
    SET_S := [ Item( 'Slot', [ $1537, $1538 ] ) ];
    PrintLn(SET_S.Length);
    PrintLn(SET_S[0].Name);
    PrintLn(SET_S[0].GraphicsList.Map(IntToStr).Join(','));
end.