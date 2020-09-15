unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    lastString: String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses WebSite, Ticker, Article;

procedure TForm1.Button1Click(Sender: TObject);
var g: TWebSite;
begin
  g := TWebSite.Create('','');
  g.URL := edit1.Text;
  LastString := g.LastResponse;
  memo1.Lines.Clear;
  memo1.Lines.Add(LastString);
  edit2.Text := g.LastUserAgent;
  g.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var s: AnsiString;
    r: String;
    f, t: integer;
    LF: AnsiString;
begin
  memo1.Lines.Clear;
  s := AnsiString(LastString);
  f := 1;
  repeat
  f := pos('<script', s, f);
    if f <> 0 then
    begin
      t := pos('/script>', s, f+1) + 6;
      if t <> 0 then
        delete(s, f, t - f);
    end;
  until f = 0;
  f := 1;
  LF := #10 + #13;
  repeat
    f := pos('<p', s, f);
    if f <> 0 then
    begin
      t := pos('>', s, f+1);
      f := t + 1;
      t := pos('</p>', s, f+1);
      if t<>0 then
      begin
        r := copy(s, f, t - f);
        r := stringReplace(r, '<br />', LF, [rfReplaceAll, rfIgnoreCase]);
        r := stringReplace(r, '<br>', LF, [rfReplaceAll, rfIgnoreCase]);
        r := RemoveUmlaute(r);
        r := stringReplace(r, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
        memo1.Lines.Add(r);
      end;

    end;
  until f = 0;
end;

procedure TForm1.Button3Click(Sender: TObject);
var g: TWebSite;
begin
  g := TWebSite.Create('','');
  Form2.TickerString := g.Ticker;
  g.Free;
  Form2.Refresh;
  Form2.Show;
end;

procedure TForm1.Button4Click(Sender: TObject);
var g: TWebSite;
begin
  g := TWebSite.Create('','');
  g.URL := edit1.Text;
  g.Refresh;
  Form3.ArticleString := g.LastResponse;
  g.Free;
  Form3.Refresh;
  Form3.Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edit1.Text := WebSiteURL;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  button1.Position.X := clientWidth - 100;
  button2.Position.X := button1.Position.X;
  edit1.Width := button1.Position.X - 30;
  memo1.Width := edit1.Width;
  edit2.Width := edit1.Width;
end;

end.
