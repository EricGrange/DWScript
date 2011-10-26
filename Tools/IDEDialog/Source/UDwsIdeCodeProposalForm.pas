unit UDwsIdeCodeProposalForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  dwsSuggestions,
  UDwsIdeDefs,
  Dialogs, StdCtrls;

type
  TCodeSuggestionMode = ( csAutoComplete, csCodeProposal );

  TOnSelectItem = procedure( const AItemText : string ) of object;

  TDwsIdeCodeProposalForm = class(TForm)
    ListBox1: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
    FSearchString : string;
    FOnSelectItem: TOnSelectItem;
    Fsuggestions  : IDwsSuggestions;
    FCodeSuggestionMode: TCodeSuggestionMode;
    procedure PerformFilter;
    procedure DoOnSelectItem;
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;

    property OnSelectItem : TOnSelectItem
               read FOnSelectItem
               write FOnSelectItem;

    procedure Open( const APoint : TPoint; ACodeSuggestionMode: TCodeSuggestionMode; ASuggestions : IDwsSuggestions );
  end;


implementation

{$R *.dfm}


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


procedure TDwsIdeCodeProposalForm.FormDestroy(Sender: TObject);
begin
  FSuggestions := nil;
end;


procedure TDwsIdeCodeProposalForm.PerformFilter;

  function FormatProposalItem( AIndex : integer ) : string;
  begin
    case FCodeSuggestionMode of
      csAutoComplete : //e.g. invoked by '.'
        Result := FSuggestions.Caption[AIndex];

      csCodeProposal :
        Result := SuggestionCategoryNames[FSuggestions.Category[AIndex]] + #9 + FSuggestions.Caption[AIndex];
     else
       Assert( False );
    end;
  end;

var
  I : integer;
begin
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;

    for I := 0 to FSuggestions.Count-1 do
        If (FSearchString = '') or BeginsWith( FSearchString, FSuggestions.Code[I] ) then
          ListBox1.Items.Add( FormatProposalItem( I ) );

  finally
    ListBox1.Items.EndUpdate;
  end;

  if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := 0;

  if FSearchString = '' then
    Caption := 'Code Proposal'
   else
    Caption := Format( 'Code Proposal "%s"', [FSearchString] );

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

    vk_Up,
    vk_Down,
    vk_Prior,
    vk_Next,
    vk_Home,
    vk_End :
      begin end;

    vk_Return :
      DoOnSelectItem;

    vk_Back :
      If FSearchString <> '' then
        begin
        Delete( FSearchString, Length( FSearchString ), 1);
        PerformFilter;
        end;

   else
    Hide;
  End;
end;

procedure TDwsIdeCodeProposalForm.FormShow(Sender: TObject);
begin
  FSearchString := '';
  PerformFilter;
end;

procedure TDwsIdeCodeProposalForm.ListBox1DblClick(Sender: TObject);
begin
  DoOnSelectItem;
end;

procedure TDwsIdeCodeProposalForm.Open(const APoint : TPoint; ACodeSuggestionMode: TCodeSuggestionMode;
  ASuggestions: IDwsSuggestions);
begin
  Left := APoint.X;
  if APoint.Y + Height > Screen.Height then
    Top := APoint.Y - Height - 20
   else
     Top := APoint.Y + 20;

  FSuggestions := ASuggestions;
  FCodeSuggestionMode := ACodeSuggestionMode;
  Show;
end;

end.
