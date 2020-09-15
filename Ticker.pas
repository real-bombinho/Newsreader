unit Ticker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  System.Generics.Collections;

type

  TDisplayObject = class
  private
    FName: String;
    FText: String;
    FTime: string;
    FLink: String;
    FLabel: TLabel;
    FClass: String;
    procedure setText(const Value: string);
  public
    property Label1: TLabel read FLabel;
    property ListText: string read FText write setText;
    property Text: string read FText write FText;
    constructor Create(const AName: String); virtual;
    destructor Destroy(); override;
  end;

  TForm2 = class(TForm)
    Rectangle1: TRectangle;
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Rectangle1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  private
    List: TObjectList<TDisplayObject>;
    procedure Clear;
    procedure Parse;
  public
    { Public declarations }
    TickerString: string;
    procedure Refresh;
  end;

var
  Form2: TForm2;

implementation
uses WebSite, unit1, Article;

{$R *.fmx}

procedure TForm2.Clear;
begin
  List.DeleteRange(0, List.Count);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption := SiteName + ' Newsticker';
  Label1.Text := SiteName;
  Rectangle1.Height := ClientHeight;
  Rectangle1.Fill.Color := TAlphaColors.White;
  List := TObjectList<TDisplayObject>.Create();
  List.OwnsObjects := true;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  Clear;
  List.Free;
end;

procedure TForm2.Label1Click(Sender: TObject);
var g: TWebSite;
    s: string;
begin
  if sender is TLabel then
  begin
    s := (Sender as TLabel).Hint;
    Form1.Edit1.Text := s;
    if sender <> Label1 then
    begin
      g := TWebSite.Create('','');
      g.URL := s;
      g.Refresh;
      Form3.ArticleString := g.LastResponse;
      g.Free;
      Form3.Refresh;
      Form3.Show;
    end;
  end;
end;

procedure TForm2.Parse;
var s: AnsiString;
    f, t: integer;
    d: integer;
    li: integer;
    obj: TDisplayObject;
begin
  s := AnsiString(TickerString);
  f := 1;
  li := 1;
  repeat
    d := pos('<h2>', s, f);
    f := pos('<li >', s, f);
    if (d < f) and (d <> 0) then
    begin
      t := pos('</h2>', s, d + 1);
      if t <> 0 then
      begin
        obj := TDisplayObject.Create('List' + inttostr(li));
        List.Add(obj);
        obj.FText := copy(s, d, t - d + 4);
        obj.FLabel.Position.X := 16;
        obj.FLabel.Position.Y := 50 + (20 * (li - 1));
        obj.FLabel.Width := 600;
        obj.FLabel.StyledSettings := []; // obj.FLabel.StyledSettings -
         // [TStyledSetting.ssFamily, TStyledSetting.ssSize];
        obj.FLabel.TextSettings.Font.Style := [TFontStyle.fsBold];
        obj.FLabel.TextSettings.Font.Size := 12;
        obj.FLabel.Text := copy(obj.FText, 5, length(obj.FText) - 8);
//        obj.FLabel.OnClick := Label1Click;
        inc(li);
      end;
    end;
    begin
      if f <> 0 then
      begin
        t := pos('/li>', s, f+1) + 0;
        if t <> 0 then
        begin
          obj := TDisplayObject.Create('List' + inttostr(li));
          List.Add(obj);
          obj.ListText := copy(s, f, t - f + 4);
  //        obj.FLabel.Text := copy(s, f + 5, t - f - 5 - 4);
          obj.FLabel.Position.Y := 50 + (20 * (li - 1));
          obj.FLabel.Width := 600;
          obj.FLabel.OnClick := Label1Click;
          inc(li);
          f := t + 4;
        end;
      end;
    end;
  until f = 0;

  if Rectangle1.Height < (80 + (20 * li)) then
  begin    //showmessage(inttostr(trunc(Rectangle1.Height)) + ' : ' + inttostr(80 + (20 * li)));
    Rectangle1.Height := (80 + (20 * li));
  end;
end;

procedure TForm2.Rectangle1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  scrollbar1.Value := scrollbar1.Value - (WheelDelta / 100);
  if scrollbar1.Value < scrollBar1.Min then
    scrollbar1.Value := scrollBar1.Min;
  if scrollbar1.Value > scrollBar1.Max then
    scrollbar1.Value := scrollBar1.Max;
end;

procedure TForm2.Refresh;
begin
  if TickerString ='' then
    clear
  else
  begin
    parse;
  end;

end;

procedure TForm2.ScrollBar1Change(Sender: TObject);
var i: single;
begin
  i := (Rectangle1.Height - ClientHeight) / 100;
  Rectangle1.Position.Y := 0 - (ScrollBar1.Value * i);
end;

{ TDisplayObject }

constructor TDisplayObject.Create(const AName: String);
begin
  FName := AName;
  FClass := 'text1';
  FLabel := TLabel.Create(nil);
  FLabel.Parent := Form2.Rectangle1;
  FLabel.ShowHint := true;
  FLabel.HitTest := true;
  FLabel.Position.X := 16;
end;

destructor TDisplayObject.Destroy;
begin
  FLabel.Parent := nil;
  FLabel.Free;
//  showmessage(FName + ' destroyed');
  inherited;
end;

procedure TDisplayObject.setText(const Value: string);
var f, t: integer;
    s: string;
begin
  FText := value;
  FTime := LocateString(FText, '<p class="meta">', '</p>');
  f := pos('<a href="', FText, 1) + 9;
  if f <> 0 then
  begin
    t := pos('">', FText, f);
    FLink := copy(FText, f, t - f);
    FLabel.Hint := FLink;
  end;
  f := t + 2;
  if t <> 0 then
  begin
    t := pos('</a>', FText, f + 1);
    s := stringReplace(copy(FText, f, t - f), '</span>', '', [rfReplaceAll, rfIgnoreCase]);
    s := FTime + #9 + stringReplace(s, '<span>', '', [rfReplaceAll, rfIgnoreCase]);
    FLabel.Text := RemoveUmlaute(s);
  end;
  FClass := LocateString(FText, '<h3 class="', '"');
  //showmessage(FClass);
end;

end.
