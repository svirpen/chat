unit clientmanager;

interface

uses System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  IdBaseComponent,
  IdComponent,
  IdCustomTCPServer,
  IdTCPServer,
  IdContext,
  IdTCPConnection,
  IdTCPClient,
  IdIOHandler,
  IdIOHandlerSocket,
  IdGlobal,
  IdIOHandlerStack,
  IdSSL,
  IdSSLOpenSSL,
  IdServerIOHandler,
  IdStack,
  IdException,
  Data.DB,
  Data.Win.ADODB,
  netmessagesparser,
  System.JSON.Serializers;

type
  TEventGetResponse = procedure(MesgType :integer; const Sender :string;
      const TimeStr :string; const Messg:string = '') of object;


  TClientManager = class(TObject)
  private
    TCPClient1 :TIdTCPClient;
    TLSHandler :TIdSSLIOHandlerSocketOpenSSL;
    CertSha256 :string;
    ListenThread: TThread;
    NeedStopWorkThread: boolean;
    JsonReadSerializer: TJsonSerializer;
    JsonWriteSerializer: TJsonSerializer;

    function GetConnectStatus() :boolean;

  public
    EventGetResponse :TEventGetResponse;
    EventLoginAccept, EventLoginReject, EventConnectInformation,
    EventHistoryMesg, EventChatMesg, EventParticipantsInfo,
    EventLeaveParticipant: TEventGetResponse;



    constructor Create(); overload;
    constructor Create(aClient :TIdTcpClient); overload;
    destructor Destroy(); override;
    procedure Start();
    procedure Stop();
    procedure Connect(const IpAddr :string; port :integer; aCertSHA256 :string = '');
    function VerifyServer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
    procedure Disconnect();
    procedure DoWork();
    procedure WaitNetworkMessage();
    procedure MoveToMainThreadQueue(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
 //   function ParseType(JSONEnum: TJSONObject.TEnumerator) :integer;

    procedure Login(const login: string; const password :String);
    procedure SendChatMessage(const text: string);
    property Connected :boolean read GetConnectStatus;
  end;


implementation

uses  System.JSON, daux;

constructor  TClientManager.Create();
begin
  Create(nil);
end;

constructor  TClientManager.Create(aClient :TIdTcpClient);
begin
  inherited Create();
  TCPClient1 := aClient;
  if TCPCLient1 = nil then
  begin
    TCPClient1.Tag := 1;
    TCPCLient1.ConnectTimeout := 0;
  end;

  TLSHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
  TLSHandler.sslOptions.VerifyMode := [sslvrfClientOnce];
  TLSHandler.sslOptions.VerifyDepth := 7;
  TLSHandler.SSLOptions.Mode := sslmUnassigned;
  TLSHandler.SSLOptions.CipherList := 'ECDHE-RSA-AES256-GCM-SHA384:'+
    'ECDHE-RSA-AES128-GCM-SHA256:'+
    'ECDHE-RSA-AES256-SHA384:'+
    'ECDHE-RSA-AES128-SHA256:'+
    'ECDHE-RSA-AES256-SHA:'+
    'ECDHE-RSA-AES128-SHA:'+
    'DHE-RSA-AES256-GCM-SHA384:'+
    'DHE-RSA-AES256-SHA256:'+
    'DHE-RSA-AES256-SHA:'+
    'DHE-RSA-AES128-GCM-SHA256:'+
    'DHE-RSA-AES128-SHA256:'+
    'DHE-RSA-AES128-SHA:'+
    'DES-CBC3-SHA:'+
    '!ADH:!EXP:!RC4:!eNULL@STRENGTH';
  TLSHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
  TLSHandler.OnVerifyPeer := VerifyServer;
  TCPCLient1.IOHandler := TLSHandler;

  ListenThread := nil;
  NeedStopWorkThread := true;
  JsonReadSerializer := TJsonSerializer.Create();
  JsonWriteSerializer := TJsonSerializer.Create();
end;

destructor TClientManager.Destroy();
begin
  if JsonWriteSerializer <> nil then FreeANdNil(JsonWriteSerializer);
  if JsonReadSerializer <> nil then FreeAndNil(JsonReadSerializer);
  if TcpClient1.Tag = 1 then  //создан в этом классе
    FreeAndNil(TcpClient1);
  FreeAndNil(TLSHandler);
  inherited;
end;

procedure TClientManager.Start();
begin
  ListenThread := TThread.CreateAnonymousThread(procedure
    begin
      while not TThread.CurrentThread.CheckTerminated do
         WaitNetworkMessage();
    end);
  ListenThread.FreeOnTerminate := false;
  ListenThread.Priority := tpNormal;
  ListenThread.Start();

end;

procedure TClientManager.Stop();
begin
  if ListenThread = nil then exit;
  ListenThread.Terminate();
  ListenThread.WaitFor();
  FreeAndNil(ListenThread);
end;

procedure TClientManager.Connect(const IpAddr :string; port :integer;
    aCertSHA256 :string = '');
begin
  TcpClient1.Host := IpAddr;
  TcpClient1.Port := port;
  TcpClient1.ReadTimeout := 50;

  CertSha256 := aCertSHA256;
  TIdSSLIOHandlerSocketBase(TcpClient1.IOHandler).PassThrough := False;
  TCPClient1.Socket.DefStringEncoding := IndyTextEncoding_UTF8;
  TcpClient1.Connect();
end;

/// <summary> Проверка сертификата сервера. Сравнивается общая валидность,
/// актуальность, можно проверять SHA256 хэш сертификата по полю CertSha256  
/// </summary>   
function TClientManager.VerifyServer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
var CertServerSha256 :string;
begin
  result := false;
  if (AError <> 18) and (AError <> 0) then
    exit;

  //  для теста только проверяем имя выдавшего сертификат, и, опционально,
  // отпечаток сертификата. Для реального разворачивания инфроструктуры открытых
  // ключей нужно будет проверять также DNS-имя, IP-адрес
  if Copy(Certificate.Issuer.OneLine,
      length(Certificate.Issuer.OneLine) - 10, 11) <> '/CN=svirpen' then
    exit;
  if CertSha256 = '' then
    result := true
  else
  begin
    CertServerSha256 := Certificate.Fingerprints.SHA256AsString;
    if CertSha256 = CertServerSha256 then
      result := true;
  end;
end;


procedure TClientManager.Disconnect();
begin
  if TCPClient1.Connected then
  begin
    TCPClient1.IOHandler.InputBuffer.Clear();
    TCPClient1.Disconnect();
  end;
end;

function TClientManager.GetConnectStatus() :boolean;
begin
  result := TCPClient1.Connected;
end;

procedure TClientManager.Login(const login: string; const password :String);
var str: string;
begin
  str := JsonWriteSerializer.Serialize<TLoginMsg>(
    TLoginMsg.Create(mtLogin, login, password));
  TCPClient1.Socket.WriteLn(str);
end;


procedure TClientManager.SendChatMessage(const text: string);
var str: string;
begin
  str := JsonWriteSerializer.Serialize<TClientMsg>(
      TClientMsg.Create(mtChatMesgFromClient, text));
  TCPClient1.Socket.WriteLn(str);
end;


procedure TClientManager.DoWork();
begin
  NeedStopWorkThread := false;
  while not NeedStopWorkThread and not TThread.CheckTerminated do
    WaitNetworkMessage();
end;

procedure TClientManager.WaitNetworkMessage();
var
  mesg:string;
  MesgType: integer;
  NetMsg: TNetMsg;
  ParticipantInfo: TParticipantIndo;
begin
// ====== Принятие сообщений ======

    try
      if not TCPClient1.Connected then
      begin
        NeedStopWorkThread := true;
        exit;
      end;
      mesg := '';
      mesg := tcpClient1.Socket.ReadLn();


   // ====== Парсинг и обработка =======
      if mesg = '' then
        exit;


      MesgType := GetMesgType(mesg);
      if MesgType in [mtLoginAccept, mtLoginReject] then
        MoveToMainThreadQueue(MesgType, '', '', '')
      else if MesgTYpe in [mtConnectInformation, mtChatMesgFromServer,
          mtHistoryMesg] then
      begin
        NetMsg := JsonReadSerializer.Deserialize<TNetMsg>(mesg);
        MoveToMainThreadQueue(NetMsg.MsgType, NetMsg.Sender, NetMsg.SendingTime,
          NetMsg.Text);
      end
      else if MesgType in [mtLeaveParticipant, mtParticipantsInfo] then
      begin
        ParticipantInfo := TParticipantIndo.Create(MesgType, '#0');
        ParticipantInfo := JsonReadSerializer.Deserialize<TParticipantIndo>(mesg);
        MoveToMainThreadQueue(ParticipantInfo.MsgType, ParticipantInfo.Participant, '',
          '');
      end;

    except
      on e: EIdConnClosedGracefully do
      begin
       // if TCpClient1.Connected then tcpClient1.Disconnect();
        NeedStopWorkThread := true;
      end;
      on e: EInvalidFormatMessageRequest do
      begin
      end;
      on e: Exception do
      begin
        NeedStopWorkThread := true;
      end;
    end;



end;

procedure TClientManager.MoveToMainThreadQueue(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
begin
  TThread.Queue(nil, procedure
  begin
//    if Assigned(EventGetResponse) then
//       EventGetResponse(MesgType, Sender, TimeStr, Text);
    if MesgType = mtLoginAccept then
      EventLoginAccept(MesgType, Sender, TimeStr, Text)
    else if MesgType = mtLoginReject then
      EventLoginReject(MesgType, Sender, TimeStr, Text)
    else if MesgType = mtConnectInformation then
      EventConnectInformation(MesgType, Sender, TimeStr, Text)
    else if MesgType = mtHistoryMesg then
      EventHistoryMesg(MesgType, Sender, TimeStr, Text)
    else if MesgType = mtChatMesgFromServer then
      EventChatMesg(MesgType, Sender, TimeStr, Text)
    else if MesgType = mtParticipantsInfo then
      EventParticipantsInfo(MesgType, Sender, TimeStr, Text)
    else if MesgType = mtLeaveParticipant then
      EventLeaveParticipant(MesgType, Sender, TimeStr, Text);


  end);
end;

end.
