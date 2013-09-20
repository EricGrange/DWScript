unit UDwsIdeCodeProposalForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, dwsSuggestions, UDwsIdeDefs;

type
  TCodeSuggestionMode = ( csAutoComplete, csCodeProposal );

  TOnSelectItem = procedure( const AItemText : string ) of object;

  TDwsIdeCodeProposalForm = class(TForm)
    ListBox: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
  private
    FSearchString : string;
    FOnSelectItem: TOnSelectItem;
    Fsuggestions  : IDwsSuggestions;
    FCodeSuggestionMode: TCodeSuggestionMode;
    procedure PerformFilter;
    procedure DoOnSelectItem;
  public
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
var
  S : string;
  I : integer;
begin
  if Assigned( FOnSelectItem ) then
    if (ListBox.ItemIndex >= 0) and (ListBox.ItemIndex < ListBox.Items.Count) then
    begin
      S := ListBox.Items[ListBox.ItemIndex];
      I := Pos( ' ', S );
      if I > 0 then
        S := Trim(Copy( S, 1, I-1 ));
      FOnSelectItem( S );
    end;
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
  ListBox.Items.BeginUpdate;
  try
    ListBox.Items.Clear;

    for I := 0 to FSuggestions.Count-1 do
      if (FSearchString = '') or BeginsWith( FSearchString, FSuggestions.Code[I] ) then
        ListBox.Items.Add( FormatProposalItem( I ) );

  finally
    ListBox.Items.EndUpdate;
  end;

  if ListBox.Items.Count > 0 then
    ListBox.ItemIndex := 0;

  if FSearchString = '' then
    Caption := 'Code Proposal'
  else
    Caption := Format( 'Code Proposal "%s"', [FSearchString] );
end;


procedure TDwsIdeCodeProposalForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
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
    vk_End : ;

    vk_Return :
      DoOnSelectItem;

    vk_Back :
      if FSearchString <> '' then
        begin
          Delete( FSearchString, Length( FSearchString ), 1);
          PerformFilter;
        end;

   else
    Hide;
  end;
end;

procedure TDwsIdeCodeProposalForm.FormShow(Sender: TObject);
begin
  FSearchString := '';
  PerformFilter;
end;

procedure TDwsIdeCodeProposalForm.ListBoxDblClick(Sender: TObject);
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
