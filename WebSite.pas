unit WebSite;

interface

uses System.SysUtils, System.DateUtils;

const
  SiteName = ''; // Put site name here i.e. 'website.com'
  WebSiteURL = 'https://www.' + SiteName + '/';
  WebSiteUserAgent = 'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1;' +
    ' Trident/4.0; FDM; MSIECrawler; Media Center PC 5.0)';
  Agents = 174;
  TOR_Port = 9150;       // use TOR browser API , browser needs to be started
  localLoop = '127.0.0.1';
  TORdefault = true;
  IPCheck = 'http://httpbin.org/ip';

type

  RLink = record
  private
    FTarget: string;
    FDescription: string;
    FRawText: string;
    procedure setText(const Value: string);
    procedure clear;
  public
    property RawText: string read FRawText write setText;
  end;

  TWebSite = class
  private
    FLastResponse: String;
    FLastFetched: TDateTime;
    FLastUnsuccessful: TDateTime;
    FLastResponseCode: integer;
    FLastUserAgent: integer;
    FUser: AnsiString;
    FURL: string;
    FPassword: AnsiString;
    FUseTor: boolean;
    bs : array [0..Agents] of string;
    function fetch(const url: string): boolean;
    function getLastResponse: String;
    procedure FillUserAgents;
    function getLastUserAgent: string;
    function getTicker: string;
    function getForum: string;
    procedure setTor(const Value: boolean);
  public
    property UseTOR: boolean read FUseTor write setTor;
    property LastResponse: String read getLastResponse;
    property ResponseCode: integer read FLastResponseCode;
    property URL: string read FURL write FURL;
    property User: AnsiString write FUser;
    property Ticker: string read getTicker;
    property Forum: string read getForum;
    property Password: AnsiString write FPassword;
    property LastUserAgent: string read getLastUserAgent;
    constructor Create(const user: String; const password: string); overload;
    procedure Refresh;
    function TORavailable: string;
  end;

  function RemoveUmlaute(const Source: string; doTrim: boolean = false): string;
  function LocateString(const Source, From, Till: string; start: integer = 1): string;
  function FindTag(var source: string; const tag: string; Offset: integer; const remove: boolean = false): string;

implementation
uses IdHTTP, IdSSLOpenSSL,IdCompressorZLib, IdIOHandlerStack, IdSocks, windows;

const WaitIfFailed = 5;
      CacheTime = 10;

function RemoveUmlaute(const Source: string; doTrim: boolean = false): string ;
var s: string;
begin
  s := stringReplace(Source, '&Auml;', 'Ae', [rfReplaceAll]);
  s := stringReplace(s, '&auml;', 'ae', [rfReplaceAll]);
  s := stringReplace(s, '&ouml;', 'oe', [rfReplaceAll]);
  s := stringReplace(s, '&Ouml;', 'Oe', [rfReplaceAll]);
  s := stringReplace(s, '&uuml;', 'ue', [rfReplaceAll]);
  s := stringReplace(s, '&Uuml;', 'Ue', [rfReplaceAll]);
  s := stringReplace(s, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
  s := stringReplace(s, '&szlig;', 'ss', [rfReplaceAll, rfIgnoreCase]);
  s := stringReplace(s, '&raquo;', '»', [rfReplaceAll, rfIgnoreCase]);
  s := stringReplace(s, '&laquo;', '«', [rfReplaceAll, rfIgnoreCase]);
  s := stringReplace(s, '&oacute;', 'ó', [rfReplaceAll, rfIgnoreCase]);
  s := stringReplace(s, '&amp;', '&&', [rfReplaceAll, rfIgnoreCase]);
  if doTrim then s := trim(s);
  result := s;
end;

function findTag(var source: string; const tag: String; Offset: integer; const remove: boolean = false): string;
var f, t, et: integer;

  function pos_CaseInsensitive(const SubStr: string; const Str: string;
    Offset: integer = 1): integer; // assumes first character is not alphabetic
  var f: integer;
      s: string;
      eos: integer;
  begin
    result := 0;
    f := Offset;
    eos := length(Str);
    repeat
      f := pos(SubStr[1], Str, f);
      s := lowercase(copy(Str, f, length(SubStr)));
      if lowercase(SubStr) = s then
      begin
        result := f;
        f := -1;
      end;
      inc(f);
    until (f = 0) or (f > eos);
  end;

begin
  f := pos_CaseInsensitive('<' + tag, source, Offset);
  if f <> 0 then
  begin
    t := pos('>', source, f + 1);
    et := pos('/>', source, f + 1);
    if (et < t) and (et <> 0) then
    begin
      result := copy(source, f, et + 1 - f);
      if remove then delete(source, f, et + 1 - f);
    end
    else
    begin
      t := pos_CaseInsensitive('</' + tag, source, t + 1);
      t := pos('>', source, t + 2);
      result := copy(source, f, t - f);
      if remove then
        delete(source, f, t - f);
    end;
  end
  else
    result := '';
end;

function LocateString(const Source, From, Till: string; start: integer = 1): string;
var s: AnsiString;
    f, t: integer;
begin
  s := AnsiString(Source);
  result := '';
  f := pos(From, s, start);
  if (f <> 0) then
  begin
    f := f + Length(From);
    t := pos(Till, s, f + 1);
    if t <> 0 then
    begin
      result := copy(s, f, t - f);
    end;
  end;
end;

constructor TWebSite.Create(const user: String; const password: string);
begin
  inherited Create;
  FUser := user;
  FUseTor := TORdefault;
  FURL := WebSiteURL;
  FPassword := password;
  FLastResponse := '';
  FLastFetched := 0;
  FLastUnsuccessful := 0;
  FillUserAgents;
end;

////////////////////////////////////////////////////////////////////////////////
//
// fetches API, if previously unsuccessful, it will fail further attempts for
//   [const WaitIfFailed] minutes and ResponseCode will be -1.
//
//    uses TOR browser on port
//
////////////////////////////////////////////////////////////////////////////////

function TWebSite.fetch(const url: string): boolean;
var Id_HandlerSocket : TIdSSLIOHandlerSocketOpenSSL;
    IdHTTP1: TIdHTTP;
    s: string;
    Tor_IO_Handler: TIdIOHandlerStack;
    IdSocksInfo: TIdSocksInfo;

begin
  result := false;
  if FLastUnsuccessful <> 0 then
    if System.DateUtils.IncSecond(FLastUnsuccessful, WaitIfFailed) < now then
    begin
      FLastResponseCode := -1;
      exit;
    end;
  IdHTTP1 := TIdHTTP.Create(nil);
  if FUseTor then
  begin
    IdSocksInfo := TIdSocksInfo.Create(nil);
  end;
  IdHTTP1.Compressor := TIdCompressorZLib.Create(nil);
  Id_HandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    if FUseTor then
    begin
      IdSocksInfo.Authentication := saNoAuthentication;
      IdSocksInfo.Host := localLoop;
      IdSocksInfo.Port := TOR_Port;
      IdSocksInfo.Version := svSocks5;
    end;
    Id_HandlerSocket.DefaultPort := 443;
    Id_HandlerSocket.SSLOptions.Mode := sslmClient;
    Id_HandlerSocket.SSLOptions.Method := sslvTLSv1_2;
    Id_HandlerSocket.SSLOptions.SSLVersions := [sslvTLSv1_2];
    if FUseTor then
      Id_HandlerSocket.TransparentProxy.Assign(IdSocksInfo);
    idHTTP1.IOHandler := Id_HandlerSocket;
    if (idHTTP1.Compressor = nil) or (not idHTTP1.Compressor.IsReady) then
      idHTTP1.Request.AcceptEncoding := 'identity'
    else
      idHTTP1.Request.AcceptEncoding := 'gzip,deflate,identity';
    idHTTP1.Request.BasicAuthentication := true;
    randomize;
    FLastUserAgent := 150 + random(Agents - 150);
    IdHTTP1.Request.UserAgent := bs[FLastUserAgent];
    IdHTTP1.Request.Username := FUser;
    FLastUnsuccessful := now;
    s := IdHTTP1.Get(url);
    if IdHTTP1.CookieManager <> nil then
    begin
//      if IdHTTP1.CookieManager.CookieCollection.Count > 0 then
//        raise Exception.Create(IdHTTP1.CookieManager.CookieCollection.Cookies[0].CookieText);
//      raise Exception.Create('Crunch ' +
//      inttostr(IdHTTP1.CookieManager.CookieCollection.Count));
    end;
    FLastResponseCode := IdHTTP1.ResponseCode;
    if FLastResponseCode = 200 then
    begin
      result := true;
      FLastResponse := s;
      FLastFetched := now;
    end;
    if FLastResponseCode = 302 then
    begin
      IdHTTP1.HandleRedirects := True;
      s := IdHTTP1.Get(url);
      FLastResponseCode := IdHTTP1.ResponseCode;
      FLastResponse := 'Redirected: ' + s;
    end;
  finally
    Id_HandlerSocket.TransparentProxy.DisposeOf;
//    if IdSocksInfo <> nil then
//      IdSocksInfo.Free;
    Id_HandlerSocket.Free;
    if Assigned(IdHTTP1.Compressor) then IdHTTP1.Compressor.Free;
    IdHTTP1.Free;
  end;
  FLastUnsuccessful := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//
// getLastResponse uses the cached result if existing and not older than
// (const CacheHours)
//
////////////////////////////////////////////////////////////////////////////////

function TWebSite.getLastResponse: String;
begin
  if System.DateUtils.IncMinute(FLastFetched, CacheTime) < now then
    Refresh;
  result := FLastResponse;
end;

function TWebSite.getLastUserAgent: string;
begin
  result := bs[FLastUserAgent];
end;

function TWebSite.getForum: string;
begin
  fetch('https://forum.' + SiteName + '/');
  result := FLastResponse;
end;

function TWebSite.getTicker: string;
begin
  fetch(WebSiteURL + 'ticker/');
  result := FLastResponse;
end;

procedure TWebSite.Refresh;
begin
  fetch(FURL);
end;

procedure TWebSite.setTor(const Value: boolean);
begin
  FUseTor := false;
  if Value  then
    if (TORavailable <> '') then
      FUseTor := true
    else
      raise Exception.Create('TOR browser not found');
end;

function TWebSite.TORavailable: string;
var hWndTemp: hWnd;
    iLenText: Integer;
    cTitletemp: array [0..254] of Char;
    sTitleTemp: string;
    partialTitle: string;
begin
  result := '';
  partialTitle := 'obfs4proxy';
  partialTitle := UpperCase(partialTitle);
  hWndTemp := FindWindow(nil, nil);
  while hWndTemp <> 0 do
  begin
    iLenText := GetWindowText(hWndTemp, cTitletemp, 255);//search after the partial name
    sTitleTemp := cTitletemp;
    sTitleTemp := UpperCase(copy(sTitleTemp, 1, iLenText));
    if pos(partialTitle, sTitleTemp ) <> 0 then
    begin
      result := copy(sTitleTemp, 1, iLenText);
      Break;
    end;
    hWndTemp := GetWindow(hWndTemp, GW_HWNDNEXT);
  end;
end;

procedure TWebSite.fillUseragents;
begin
  bs[0] := 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko';
  bs[1] := 'Mozilla/5.0 (compatible, MSIE 11, Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko';
  bs[2] := 'Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1;' +
    ' .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0';
  bs[3] := 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 7.0; InfoPath.3; .NET CLR 3.1.40767; Trident/6.0; de-AT)';
  bs[4] := 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)';
  bs[5] := 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)';
  bs[6] := 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/5.0)';
  bs[7] := 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/4.0; InfoPath.2; SV1; .NET CLR 2.0.50727; WOW64)';
  bs[8] := 'Mozilla/5.0 (compatible; MSIE 10.0; Macintosh; Intel Mac OS X 10_7_3; Trident/6.0)';
  bs[9] := 'Mozilla/4.0 (Compatible; MSIE 8.0; Windows NT 5.2; Trident/6.0)';
  bs[10] := 'Mozilla/4.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/5.0)';
  bs[11] := 'Mozilla/1.22 (compatible; MSIE 10.0; Windows 3.1)';
  bs[12] := 'Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; de)';
  bs[13] := 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 7.1; Trident/5.0)';
  bs[14] := 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; Media Center PC 6.0;' +
    ' InfoPath.3; MS-RTC LM 8; Zune 4.7)';
  bs[15] := 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Win64; x64; Trident/5.0';
  bs[16] := 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0; SLCC2; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; InfoPath.2; .NET CLR 1.1.4322; .NET4.0C;' +
    ' Tablet PC 2.0)';
  bs[17] := 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.0; Trident/4.0; GTB7.4; InfoPath.3; SV1;' +
    ' .NET CLR 3.1.76908; WOW64; de-CH)';
  bs[18] := 'Mozilla/5.0 ( ; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)';
  bs[19] := 'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/4.0; FDM; MSIECrawler; Media Center PC 5.0)';
  bs[20] := 'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.0; Trident/4.0; GTB7.4; InfoPath.3; SV1;' +
    ' .NET CLR 3.4.53360; WOW64; de)';
  bs[21] := 'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 5.1; Trident/5.0)';
  bs[22] := 'Mozilla/4.0 (compatible; MSIE 9.0; Windows 98; .NET CLR 3.0.04506.30)';
  bs[23] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 7.1; Trident/5.0; .NET CLR 2.0.50727; SLCC2;' +
    ' .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; InfoPath.3; .NET4.0C)';
  bs[24] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; .NET4.0C; .NET4.0E; AskTB5.5)';
  bs[25] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Win64; x64; Trident/5.0; .NET4.0C; .NET4.0E; InfoPath.3)';
  bs[26] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Trident/5.0; SLCC1; .NET CLR 2.0.50727; Media Center PC 5.0; .NET CLR 3.5.30729; .NET CLR 3.0.30729; FDM; .NET4.0C; .NET4.0E; chromeframe/11.0.696.57)';
  bs[27] := 'Mozilla/4.0 (compatible; U; MSIE 9.0; WIndows NT 9.0; de-AT)';
  bs[28] := 'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0; FunWebProducts)';
  bs[29] := 'Mozilla/5.0 (compatible; MSIE 8.0; Windows NT 5.1; SLCC1; .NET CLR 1.1.4322)';
  bs[30] := 'Mozilla/5.0 (compatible; MSIE 8.0; Windows NT 5.0; Trident/4.0; InfoPath.1; SV1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 3.0.04506.30)';
  bs[31] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; WOW64; Trident/4.0; SLCC2; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; InfoPath.2; OfficeLiveConnector.1.3;' +
    ' OfficeLivePatch.0.0; MS-RTC LM 8; Zune 4.0)';
  bs[32] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0; iCafeMedia; QQDownload 667; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; InfoPath.2; .NET4.0C; .NET4.0E)';
  bs[33] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0; GTB7.4; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; InfoPath.3; .NET4.0C; .NET4.0E; AskTbFXTV5/5.15.4.23821)';
  bs[34] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0; .NET CLR 3.5.30729)';
  bs[35] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; SV1; Alexa Toolbar)';
  bs[36] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; SV1; .NET CLR 2.0.50727; .NET CLR 3.0.04506.30; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)';
  bs[37] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1)';
  bs[38] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; WOW64; Trident/4.0; SLCC1; .NET CLR 2.0.50727; Media Center PC 5.0; InfoPath.2; .NET CLR 3.5.21022; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';
  bs[39] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Win64; x64; Trident/4.0)';
  bs[40] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; SLCC1; .NET CLR 2.0.50727;' +
    ' .NET CLR 1.0.3705; .NET CLR 1.1.4322; InfoPath.2; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';
  bs[41] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; SLCC1)';
  bs[42] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; QQDownload 590; SLCC1;' +
    ' .NET CLR 2.0.50727; InfoPath.2; .NET CLR 3.5.30729; .NET CLR 3.0.30729; .NET4.0C)';
  bs[43] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; Mozilla/4.0 (compatible; MSIE 6.0;' +
    ' Windows NT 5.1; SV1) ; SLCC1; .NET CLR 2.0.50727; .NET CLR 3.5.30729; InfoPath.2; .NET CLR 3.0.30729;' +
    ' Zune 4.0; MS-RTC LM 8; MSN Optimized;US)';
  bs[44] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; Mozilla/4.0 (compatible; MSIE 6.0;' +
    ' Windows NT 5.1; SV1) )';
  bs[45] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; GTB7.4; InfoPath.1; SV1; .NET CLR 4.8.88265; WOW64; de)';
  bs[46] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; FBSMTWB; GTB6.3; SLCC1; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729)';
  bs[47] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.2; WOW64; Trident/4.0; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.0.04506.648; .NET CLR 3.5.21022; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)';
  bs[48] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.2; WOW64; Trident/4.0; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.0.04506.30; .NET CLR 3.0.04506.648; InfoPath.1; .NET CLR 1.1.4322)';
  bs[49] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.2; Win64; x64; Trident/4.0)';
  bs[50] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; MathPlayer 2.10b)';
  bs[51] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; iOpus-I-M; .NET CLR 1.1.4322;' +
    ' .NET CLR 2.0.50727; .NET CLR 3.0.04506.648; .NET CLR 3.5.21022; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)';
  bs[52] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; GTB6.6; .NET CLR 2.0.50727; InfoPath.2;' +
    ' .NET CLR 3.0.04506.648; .NET CLR 3.5.21022; .NET CLR 1.1.4322; msn OptimizedIE8;ZHTW)';
  bs[53] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; FunWebProductsp; GTB6;' +
    ' .NET CLR 1.0.3705; .NET CLR 1.1.4322; Media Center PC 4.0; MS-RTC LM 8; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.0.04506.648; .NET CLR 3.5.21022; InfoPath.2; .NET CLR 3.0.45';
  bs[54] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; chromeframe/20.0.1132.57;' +
    ' .NET CLR 1.0.3705; .NET CLR 1.1.4322; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)';
  bs[55] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; SV1; Alexa Toolbar)';
  bs[56] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; InfoPath.1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)';
  bs[57] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; de)';
  bs[58] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; de-AT)';
  bs[59] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152;' +
    ' .NET CLR 3.5.30729)';
  bs[60] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.0; Trident/4.0; SLCC1; .NET CLR 2.0.50727;' +
    ' Media Center PC 5.0; InfoPath.2; .NET CLR 3.5.21022; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';
  bs[61] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows 98; Win 9x 4.90; WOW64; Trident/4.0; SLCC2;' +
    ' .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; Tablet PC 2.0)';
  bs[62] := 'Mozilla/4.0 (compatible; MSIE 8.0; ; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 1.1.4322;' +
    ' .NET CLR 3.0.04506.648; .NET CLR 3.5.21022; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729;' +
    ' OfficeLiveConnector.1.4; OfficeLivePatch.1.3; .NET4.0C; .NET4.0E; InfoPat';
  bs[63] := 'Mozilla/4.0 (compatible; MSIE 8.0; ; Trident/4.0; .NET CLR 1.1.4322; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)';
  bs[64] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; WOW64; Trident/4.0; SLCC2; .NET CLR 2.0.50727; InfoPath.2)';
  bs[65] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Win64; x64; Trident/4.0; .NET CLR 2.0.50727; SLCC2;' +
    ' .NET CLR 3.5.30729; .NET CLR 3.0.30729; .NET4.0C)';
  bs[66] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; WOW64; Trident/4.0; SLCC1; .NET CLR 2.0.50727;' +
    ' .NET CLR 1.1.4322; .NET CLR 3.5.30729; .NET CLR 3.0.30729)';
  bs[67] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Trident/4.0; QQDownload 661; SLCC2; ' +
    '.NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; InfoPath.1)';
  bs[68] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Trident/4.0; (R1 1.6); SLCC1; .NET CLR 2.0.50727; InfoPath.2; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';
  bs[69] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.2; Trident/4.0; SLCC1; .NET CLR 1.0.3705; .NET CLR 2.0.50727)';
  bs[70] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.2; Trident/4.0)';
  bs[71] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; WOW64; Trident/4.0; SLCC2; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; eSobiSubscriber 2.0.4.16; InfoPath.2;' +
    ' OfficeLiveConnector.1.5; OfficeLivePatch.1.3)';
  bs[72] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; SV1; .NET CLR 2.0.50727; .NET CLR 3.0.04506.648; .NET CLR 3.5.21022)';
  bs[73] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; Zune 4.7; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729) chromeframe/8.0.552.237';
  bs[74] := 'Mozilla/4.0 ( ; MSIE 8.0; Windows NT 6.0; Trident/4.0; GTB6.6; .NET CLR 3.5.30729)';
  bs[75] := 'Mozilla/4.0 ( ; MSIE 7.0; Windows NT 6.1; WOW64; Trident/4.0; SLCC2; .NET CLR 2.0.50727;' +
    ' .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729;' +
    ' .NET CLR 3.0.30729; Media Center PC 6.0)';
  bs[76] := 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0)';
  bs[77] := 'Mozilla/4.0(compatible; MSIE 7.0b; Windows NT 6.0)';
  bs[78] := 'Mozilla/4.0 (compatible; MSIE 7.0b; Windows NT 6.0)';
  bs[79] := 'Mozilla/4.0 (compatible; MSIE 7.0b; Windows NT 5.2; .NET CLR 1.1.4322; .NET CLR 2.0.50727; InfoPath.2; .NET CLR 3.0.04506.30)';
  bs[80] := 'Mozilla/4.0 (compatible; MSIE 7.0b; Windows NT 5.1; Media Center PC 3.0; .NET CLR 1.0.3705; .NET CLR 1.1.4322; .NET CLR 2.0.50727; InfoPath.1)';
  bs[81] := 'Mozilla/5.0 (Windows; U; MSIE 7.0; Windows NT 6.0; de)';
  bs[82] := 'Mozilla/5.0 (Windows; U; MSIE 7.0; Windows NT 5.2)';
  bs[83] := 'Mozilla/5.0 (compatible; MSIE 7.0; Windows 98; SpamBlockerUtility 6.3.91; SpamBlockerUtility 6.2.91; .NET CLR 4.1.89;GB)';
  bs[84] := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Trident/6.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; .NET4.0C; .NET4.0E)';
  bs[85] := 'Mozilla/4.0 (compatible; MSIE 6.1; Windows XP; .NET CLR 1.1.4322; .NET CLR 2.0.50727)';
  bs[86] := 'Mozilla/4.0 (compatible;MSIE 6.0;Windows 98;Q312461)';
  bs[87] := 'Mozilla/4.0 (X11; MSIE 6.0; i686; .NET CLR 1.1.4322; .NET CLR 2.0.50727; FDM)';
  bs[88] := 'Mozilla/4.0 (compatible; MSIE 5.5b1; Mac_PowerPC)';
  bs[89] := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1';
  bs[90] := 'Mozilla/5.0 (Windows NT 6.3; rv:36.0) Gecko/20100101 Firefox/36.0';
  bs[91] := 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10; rv:33.0) Gecko/20100101 Firefox/33.0';
  bs[92] := 'Mozilla/5.0 (X11; Linux i586; rv:31.0) Gecko/20100101 Firefox/31.0';
  bs[93] := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:31.0) Gecko/20130401 Firefox/31.0';
  bs[94] := 'Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0';
  bs[95] := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0';
  bs[96] := 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/29.0';
  bs[97] := 'Mozilla/5.0 (X11; OpenBSD amd64; rv:28.0) Gecko/20100101 Firefox/28.0';
  bs[98] := 'Mozilla/5.0 (X11; Linux x86_64; rv:28.0) Gecko/20100101 Firefox/28.0';
  bs[99] := 'Opera/9.80 (X11; Linux i686; Ubuntu/14.10) Presto/2.12.388 Version/12.16';
  bs[100] := 'Opera/9.80 (Windows NT 6.0) Presto/2.12.388 Version/12.14';
  bs[101] := 'Mozilla/5.0 (Windows NT 6.0; rv:2.0) Gecko/20100101 Firefox/4.0 Opera 12.14';
  bs[102] := 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.0) Opera 12.14';
  bs[103] := 'Opera/12.80 (Windows NT 5.1; U; en) Presto/2.10.289 Version/12.02';
  bs[104] := 'Opera/9.80 (Windows NT 6.1; U; es-ES) Presto/2.9.181 Version/12.00';
  bs[105] := 'Opera/9.80 (Windows NT 5.1; U; zh-sg) Presto/2.9.181 Version/12.00';
  bs[106] := 'Opera/12.0(Windows NT 5.2;U;en)Presto/22.9.168 Version/12.00';
  bs[107] := 'Opera/12.0(Windows NT 5.1;U;en)Presto/22.9.168 Version/12.00';
  bs[108] := 'Mozilla/5.0 (Windows NT 5.1) Gecko/20100101 Firefox/14.0 Opera/12.0';
  bs[109] := 'Opera/9.80 (Windows NT 6.1; WOW64; U; pt) Presto/2.10.229 Version/11.62';
  bs[110] := 'Opera/9.80 (Windows NT 6.0; U; pl) Presto/2.10.229 Version/11.62';
  bs[111] := 'Opera/9.80 (Macintosh; Intel Mac OS X 10.6.8; U; fr) Presto/2.9.168 Version/11.52';
  bs[112] := 'Opera/9.80 (Macintosh; Intel Mac OS X 10.6.8; U; de) Presto/2.9.168 Version/11.52';
  bs[113] := 'Opera/9.80 (Windows NT 5.1; U; en) Presto/2.9.168 Version/11.51';
  bs[114] := 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; de) Opera 11.51';
  bs[115] := 'Opera/9.80 (Windows NT 5.1; U;) Presto/2.7.62 Version/11.01';
  bs[116] := 'Opera/9.80 (Windows NT 5.2; U; de) Presto/2.7.62 Version/11.01';
  bs[117] := 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/7.0.3 Safari/7046A194A';
  bs[118] := 'Mozilla/5.0 (iPad; CPU OS 6_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/6.0 Mobile/10A5355d Safari/8536.25';
  bs[119] := 'Mozilla/5.0 (iPad; CPU OS 5_1 like Mac OS X) AppleWebKit/534.46 (KHTML, like Gecko ) Version/5.1 Mobile/9B176 Safari/7534.48.3';
  bs[120] := 'Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_7; de-ch) AppleWebKit/533.21.1 (KHTML, like Gecko) Version/5.0.5 Safari/533.21.1';
  bs[121] := 'Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; de-de) AppleWebKit/533.20.25 (KHTML, like Gecko) Version/5.0.4 Safari/533.20.27';
  bs[122] := 'Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_5; de-de) AppleWebKit/534.15+ (KHTML, like Gecko) Version/5.0.3 Safari/533.19.4';
  bs[123] := 'Mozilla/5.0 (Windows; U; Windows NT 6.0; de-DE) AppleWebKit/533.20.25 (KHTML, like Gecko) Version/5.0.3 Safari/533.19.4';
  bs[124] := 'Mozilla/5.0 (Windows NT 5.1; U; de; rv:1.9.1.6) Gecko/20091201 Firefox/3.5.6 Opera 11.00';
  bs[125] := 'Mozilla/5.0 (X11; Linux x86_64; U; de; rv:1.9.1.6) Gecko/20091201 Firefox/3.5.6 Opera 10.62';
  bs[126] := 'Opera/9.80 (Windows 98; U; de) Presto/2.6.30 Version/10.61';
  bs[127] := 'Opera/9.80 (S60; SymbOS; Opera Tablet/9174; U; en) Presto/2.7.81 Version/10.5';
  bs[128] := 'Opera/9.80 (Windows NT 5.1; U; de) Presto/2.2.15 Version/10.10';
  bs[129] := 'Opera/9.80 (X11; Linux x86_64; U; de) Presto/2.2.15 Version/10.00';
  bs[130] := 'Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_3 like Mac OS X; de-de) AppleWebKit/533.17.9' +
    ' (KHTML, like Gecko) Version/5.0.2 Mobile/8F190 Safari/6533.18.5';
  bs[131] := 'Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_8; de-de) AppleWebKit/533.16 (KHTML, like Gecko) Version/5.0 Safari/533.16';
  bs[132] := 'Mozilla/5.0 (Macintosh; U; PPC Mac OS X 10_4_11; de) AppleWebKit/528.4+ (KHTML, like Gecko) Version/4.0dp1 Safari/526.11.2';
  bs[133] := 'Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_3; de-de) AppleWebKit/531.22.7 (KHTML, like Gecko) Version/4.0.5 Safari/531.22.7';
  bs[134] := 'Mozilla/5.0 (Windows; U; Windows NT 6.1; de-DE) AppleWebKit/531.21.8 (KHTML, like Gecko) Version/4.0.4 Safari/531.21.10';
  bs[135] := 'Mozilla/5.0 (Windows; U; Windows NT 6.2; WOW64; rv:1.8.0.7) Gecko/20110321 MultiZilla/4.33.2.6a SeaMonkey/8.6.55';
  bs[136] := 'Mozilla/5.0 (compatible; MSIE 9.0; AOL 9.7; AOLBuild 4343.19; Windows NT 6.1; WOW64; Trident/5.0; FunWebProducts)';
  bs[137] := 'Mozilla/4.0 (compatible; MSIE 7.0; AOL 9.5; AOLBuild 4337.93; Windows NT 5.1; Trident/4.0; DigExt; .NET CLR 1.1.4322)';
  bs[138] := 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.13) Gecko/20080414 Firefox/2.0.0.13 Pogo/2.0.0.13.6866';
  bs[139] := 'Mozilla/5.0 (Linux; U; Android 4.0.3; ko-kr; LG-L160L Build/IML74K) AppleWebkit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30';
  bs[140] := 'Mozilla/5.0 (Linux; U; Android 4.0.3; de-ch; HTC Sensation Build/IML74K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30';
  bs[141] := 'Mozilla/5.0 (Linux; U; Android 2.3.3; de-ch; HTC Desire Build/FRF91) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1';
  bs[142] := 'Mozilla/5.0 (BlackBerry; U; BlackBerry 9900; de) AppleWebKit/534.11+ (KHTML, like Gecko) Version/7.1.0.346 Mobile Safari/534.11+';
  bs[143] := 'Opera/9.80 (J2ME/MIDP; Opera Mini/9 (Compatible; MSIE:9.0; iPhone; BlackBerry9700; AppleWebKit/24.746; U; en) Presto/2.5.25 Version/10.54';
  bs[144] := 'Opera/12.02 (Android 4.1; Linux; Opera Mobi/ADR-1111101157; U; de-AT) Presto/2.9.201 Version/12.02';
  bs[145] := 'Mozilla/5.0 (PLAYSTATION 3; 3.55)';
  bs[146] := 'Mozilla/5.0 (Windows NT 6.3; WOW64; rv:38.0) Gecko/20100101 Thunderbird/38.2.0 Lightning/4.0.2';
  bs[147] := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:38.0) Gecko/20100101 Thunderbird/38.2.0 Lightning/4.0.2';
  bs[148] := 'Mozilla/5.0 (Windows NT 5.1; rv:38.0) Gecko/20100101 Thunderbird/38.0.1 Lightning/4.0.1.2';
  bs[149] := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';
  bs[150] := 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)';
  bs[151] := 'Mozilla/5.0 (Linux; Android 6.0.1; Nexus 5X Build/MMB29P) AppleWebKit/537.36 ' +
    '(KHTML, like Gecko) Chrome/41.0.2272.96 Mobile Safari/537.36 (compatible; Googlebot/2.1; ' +
    '+http://www.google.com/bot.html)';
  bs[152] := 'Googlebot/2.1 (+http://www.google.com/bot.html)';
  bs[153] := 'Googlebot/2.1 (+http://www.googlebot.com/bot.html)';
  bs[154] := 'Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; Googlebot/2.1; +http://www.google.com/bot.html) Safari/537.36';
  bs[155] := 'Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)';
  bs[156] := 'Mozilla/5.0 (compatible; adidxbot/2.0; +http://www.bing.com/bingbot.htm)';
  bs[157] := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/534+ (KHTML, like Gecko) BingPreview/1.0b';
  bs[158] := 'Mozilla/5.0 (Windows Phone 8.1; ARM; Trident/7.0; Touch; rv:11.0; IEMobile/11.0; NOKIA; Lumia 530) like Gecko BingPreview/1.0b';
  bs[159] := 'Mozilla/5.0 (iPhone; CPU iPhone OS 7_0 like Mac OS X) ' +
    'AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11A465 Safari/9537.53' +
    ' (compatible; adidxbot/2.0; +http://www.bing.com/bingbot.htm)';
  bs[160] := 'Mozilla/5.0 (Linux; Android 6.0.1; Nexus 5X Build/MMB29P) ' +
    'AppleWebKit/537.36 (KHTML, like Gecko) Chrome/W.X.Y.Z Mobile Safari/537.36 ' +
    'Edg/W.X.Y.Z (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)';
  bs[161] := 'Mozilla/5.0 (compatible; DuckDuckGo-Favicons-Bot/1.0; +http://duckduckgo.com)';
  bs[162] := '(compatible; Mediapartners-Google/2.1; +http://www.google.com/bot.html)';
  bs[163] := 'AdsBot-Google (+http://www.google.com/adsbot.html)';
  bs[164] := 'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help.yahoo.com/help/us/ysearch/slurp)';
  bs[165] := 'DuckDuckBot/1.0; (+http://duckduckgo.com/duckduckbot.html)';
  bs[166] := 'Mozilla/5.0 (compatible; Baiduspider/2.0; +http://www.baidu.com/search/spider.html)';
  bs[167] := 'Mozilla/5.0 (compatible; YandexBot/3.0; +http://yandex.com/bots)';
  bs[168] := 'Sogou web spider/4.0(+http://www.sogou.com/docs/help/webmasters.htm#07)';
  bs[169] := 'Sogou Pic Spider/3.0( http://www.sogou.com/docs/help/webmasters.htm#07)';
  bs[170] := 'Mozilla/5.0 (compatible; Exabot/3.0; +http://www.exabot.com/go/robot)';
  bs[171] := 'facebot';
  bs[172] := 'facebookexternalhit/1.0 (+http://www.facebook.com/externalhit_uatext.php)';
  bs[173] := 'facebookexternalhit/1.1 (+http://www.facebook.com/externalhit_uatext.php)';
  bs[174] := 'ia_archiver (+http://www.alexa.com/site/help/webmasters; crawler@alexa.com)';
end;


{ RLink }

procedure RLink.clear;
begin
  FTarget := '';
  FDescription := '';
  FRawText := '';
end;

procedure RLink.setText(const Value: string);
begin
  if lowerCase(copy(Value, 1, 9)) <> '<a href="' then
  begin
    clear;
    raise Exception.Create('String appears to be no valid link');
  end;
  FRawText := Value;

end;

end.
