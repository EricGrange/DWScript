unit UDwsIdeCodeProposalForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TOnSelectItem = procedure( const AItemText : string ) of object;

  TDwsIdeCodeProposalForm = class(TForm)
    ListBox1: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FSearchString : string;
    FProposalList : TStrings;
    FOnSelectItem: TOnSelectItem;
    procedure PerformFilter;
    procedure DoOnSelectItem;
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    property ProposalList : TStrings
               read FProposalList;
    property OnSelectItem : TOnSelectItem
               read FOnSelectItem
               write FOnSelectItem;
  end;


implementation

{$R *.dfm}

uses
  UDwsIdeDefs;

{ TDwsIdeCodeProposalForm }

procedure TDwsIdeCodeProposalForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle + WS_EX_NOACTIVATE;
end;

procedure TDwsIdeCodeProposalForm.DoOnSelectItem;
begin
  If Assigned( FOnSelectItem ) then
    If (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then
      FOnSelectItem( ListBox1.Items[ListBox1.ItemIndex] );
  Hide;
end;


procedure TDwsIdeCodeProposalForm.FormCreate(Sender: TObject);
begin
  FProposalList := TStringList.Create;
end;

procedure TDwsIdeCodeProposalForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FProposalList );
end;


procedure TDwsIdeCodeProposalForm.PerformFilter;
var
  I : integer;
begin
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Assign( FProposalList );

    if FSearchString <> '' then
      begin
      ListBox1.Items.Clear;
      for I := 0 to FProposalList.Count-1 do
        If BeginsWith( FSearchString, FProposalList[I] ) then
          ListBox1.Items.Add( FProposalList[I] );
      end;
  finally
    ListBox1.Items.EndUpdate;
  end;

  if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := 0;
end;



procedure TDwsIdeCodeProposalForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Case Key of
    Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord( '0' )..Ord('9') :
      begin
      FSearchString := FSearchString + Char( Key );
      PerformFilter;
      end;

    vk_Left :
      begin
      FSearchString := '';
      PerformFilter;
      end;

    vk_Up, vk_Down :
      begin end;

    vk_Return :
      DoOnSelectItem;
   else
    Hide;
  End;
end;

procedure TDwsIdeCodeProposalForm.FormShow(Sender: TObject);
begin
  FSearchString := '';
  PerformFilter;
end;

procedure TDwsIdeCodeProposalForm.ListBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  DoOnSelectItem;
end;


end.
