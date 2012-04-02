unit Test;

interface

type
  TLayout = class
  end;

  TLayoutConfig = class
  end;

  TObjectArr = array of TObject;

  Layout = class
  public
    class function Left(controls: TObjectArr): TLayout; overload;
    class function Left(control: TObject): TLayout; overload;
    class function Right(params: TLayoutConfig; control: TObject): TLayout; overload;
    class function Right(controls: TObjectArr): TLayout; overload;
    class function Right(control: TObject): TLayout; overload;
    class function Top(params: TLayoutConfig; controls: TObjectArr): TLayout; overload;
    class function Top(params: TLayoutConfig; control: TObject): TLayout; overload;
    class function Top(controls: TObjectArr): TLayout; overload;
    class function Top(control: TObject): TLayout; overload;
    class function Bottom(params: TLayoutConfig; controls: TObjectArr): TLayout; overload;
    class function Bottom(params: TLayoutConfig; control: TObject): TLayout; overload;
    class function Bottom(controls: TObjectArr): TLayout; overload;
    class function Bottom(control: TObject): TLayout; overload;
    class function Client(params: TLayoutConfig; controls: TObjectArr): TLayout; overload;
    class function Client(params: TLayoutConfig; control: TObject): TLayout; overload;
    class function Client(controls: TObjectArr): TLayout; overload;
  end;

implementation
  
class function Layout.Left(controls: TObjectArr): TLayout;
begin
end;

class function Layout.Left(control: TObject): TLayout;
begin
end;

var
  lm: TLayout = Layout.Left(nil);

class function Layout.Right(params: TLayoutConfig; control: TObject): TLayout;
begin
end;

class function Layout.Right(controls: TObjectArr): TLayout;
begin
end;

class function Layout.Right(control: TObject): TLayout;
begin
end;

class function Layout.Top(params: TLayoutConfig; controls: TObjectArr): TLayout;
begin
end;

class function Layout.Top(params: TLayoutConfig; control: TObject): TLayout;
begin
end;

class function Layout.Top(controls: TObjectArr): TLayout;
begin
end;

class function Layout.Top(control: TObject): TLayout;
begin
end;

class function Layout.Bottom(params: TLayoutConfig; controls: TObjectArr): TLayout;
begin
end;

class function Layout.Bottom(params: TLayoutConfig; control: TObject): TLayout;
begin
end;

class function Layout.Bottom(controls: TObjectArr): TLayout;
begin
end;

class function Layout.Bottom(control: TObject): TLayout;
begin
end;

class function Layout.Client(params: TLayoutConfig; controls: TObjectArr): TLayout;
begin
end;

class function Layout.Client(params: TLayoutConfig; control: TObject): TLayout;
begin
end;

class function Layout.Client(controls: TObjectArr): TLayout;
begin
end;
