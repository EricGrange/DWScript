unit ToDoModel;

interface

type
  TToDoCategoryList = array of string;

  TToDoModel = partial class
  public
    Category: TToDoCategoryList;
  end;

var
  Model: TToDoModel;

implementation

type
  TToDoModel = partial class
  end;

initialization
  Model := TToDoModel.Create;
  PrintLn(Model.ClassName);
end.