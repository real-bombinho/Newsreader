unit Article;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, System.Generics.Collections,
  Ticker;

type

  TDisplayobject = class(Ticker.TDisplayObject)
  public
    constructor Create(const AName: String); override;
  end;

  TForm3 = class(TForm)
    ScrollBar1: TScrollBar;
    Rectangle1: TRectangle;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Rectangle1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    List: TObjectList<TDisplayObject>;
    procedure Clear;
    procedure Parse;
  public
    { Public declarations }
    ArticleString: string;
    procedure Refresh;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses WebSite;

procedure TForm3.Clear;
begin
  List.DeleteRange(0, List.Count);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  Rectangle1.Height := ClientHeight;
  Rectangle1.Fill.Color := TAlphaColors.White;
  List := TObjectList<TDisplayObject>.Create();
  List.OwnsObjects := true;
end;

procedure TForm3.FormResize(Sender: TObject);
var z: single;
begin
  begin
    z := (clientWidth - scrollBar1.Width) / 617;
    rectangle1.Scale.X := z;
    rectangle1.Scale.Y := z;
  end;

end;

procedure TForm3.Parse;
var s, a: AnsiString;
    f, t: integer;
    li: integer;
    obj: TDisplayObject;
begin
  s := AnsiString(ArticleString);
  li := 1;
  clear;
  Label1.Text := RemoveUmlaute(LocateString(s, '<title>', '</title>'));
  obj := TDisplayObject.Create('List' + inttostr(li));
  List.Add(obj);

  a := RemoveUmlaute(LocateString(s, 'name="description" content="', '">'));
  repeat
    obj := TDisplayObject.Create('List' + inttostr(li));
    List.Add(obj);
    obj.Label1.Position.Y := 50 + (20 * (li - 1));
    obj.Label1.Width := 600;
    if length(a) >= 80 then
    begin
      t := 80;
      while (a[t] <> ' ') and (t > 0) do dec(t);
    end
    else
      t := length(a);
    if (t = 0) and (length(a) >= 80) then
      t := 80;
    obj.Label1.Text := copy(a, 1, t);
    delete(a, 1, t);
    inc(li);
  until length(a) <= 0;

  obj := TDisplayObject.Create('List' + inttostr(li));
  List.Add(obj);
  obj.Text := LocateString(s, '"datePublished": "', '",');
  obj.Label1.Position.Y := 50 + (20 * (li - 1));
  obj.Label1.Width := 600;
  obj.Label1.Text := 'Publiziert : ' + obj.Text;
  inc(li);
  obj := TDisplayObject.Create('List' + inttostr(li));
  List.Add(obj);
  obj.Text := LocateString(s, 'rel="author">', '</a>');
  obj.Label1.Position.Y := 50 + (20 * (li - 1));
  obj.Label1.Width := 600;
  obj.Label1.Text := 'Autor : ' + RemoveUmlaute(obj.Text);
  inc(li);
  obj := TDisplayObject.Create('List' + inttostr(li));
  List.Add(obj);
  obj.Label1.Position.Y := 50 + (20 * (li - 1));
  obj.Label1.Width := 600;
  obj.Label1.Text := ' ';
  inc(li);
  f := 1;
  repeat
    f := pos('<p id="gpar', s, f);
    if f <> 0 then
    begin
      f := pos('">', s, f + 1) + 2;
      t := pos('</p>', s, f + 1);
      a := copy(s, f, t - f);
      repeat
        obj := TDisplayObject.Create('List' + inttostr(li));
        List.Add(obj);
        obj.Label1.Position.Y := 50 + (20 * (li - 1));
        obj.Label1.Width := 600;
        if length(a) >= 80 then
        begin
          t := 80;
          while (a[t] <> ' ') and (t > 0) do dec(t);
        end
        else
          t := length(a);
        if (t = 0) and (length(a) >= 80) then
          t := 80;
        obj.Label1.Text := trim(copy(a, 1, t));
        delete(a, 1, t);
        inc(li);       //showmessage(inttostr(t) + ' : ' + obj.Label1.Text + ' : ' + inttostr(length(a)) + ' "' + a + '"');
      until length(a) <= 0;
      obj := TDisplayObject.Create('List' + inttostr(li));
      List.Add(obj);
      obj.Label1.Position.Y := 50 + (20 * (li - 1));
      obj.Label1.Width := 600;
      obj.Label1.Text := ' ';
      inc(li);
    end;
  until f = 0;

  //showmessage(inttostr(trunc(Rectangle1.Height)) + ' : ' + inttostr(80 + (20 * li)));
  Rectangle1.Height := (80 + (20 * li));

end;

procedure TForm3.Rectangle1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  scrollbar1.Value := scrollbar1.Value - (WheelDelta / 100);
  if scrollbar1.Value < scrollBar1.Min then
    scrollbar1.Value := scrollBar1.Min;
  if scrollbar1.Value > scrollBar1.Max then
    scrollbar1.Value := scrollBar1.Max;
end;

procedure TForm3.Refresh;
begin
  if ArticleString = '' then
    clear
  else
  begin
    parse;
  end;
end;

procedure TForm3.ScrollBar1Change(Sender: TObject);
var i: single;
begin
  i := ((Rectangle1.Height * rectangle1.Scale.X) - ClientHeight) / 100;
  Rectangle1.Position.Y := 0 - (ScrollBar1.Value * i);
end;

{ TDisplayobject }

constructor TDisplayobject.Create(const AName: String);
begin
  inherited;
  Label1.Parent := Form3.Rectangle1;
end;

end.
