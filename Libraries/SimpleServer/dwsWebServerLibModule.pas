unit dwsWebServerLibModule;

interface

uses
   SysUtils, Classes,
   dwsComp, dwsExprs,
   dwsWebServerInfo;

type
  TdwsWebServerLib = class(TDataModule)
    dwsWebServer: TdwsUnit;
  private
    { Private declarations }
    FServer :  IWebServerInfo;
  public
    { Public declaration }
    property Server : IWebServerInfo read FServer write FServer;
  end;

implementation

{$R *.dfm}

end.
