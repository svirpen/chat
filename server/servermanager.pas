unit servermanager;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  SyncObjs,
  IdBaseComponent,
  IdComponent,
  IdCustomTCPServer,
  IdTCPServer,
  IdContext,
  IdServerIOHandlerSocket,
  IdException,
  IdGlobal,
  IdSSL,
  IdSSLOpenSSL,
  ActiveX,
  Data.DB,
  Data.Win.ADODB,
  netmessagesparser,
  chatdbinteraction,
  System.JSON.Serializers,
  Windows;

type
/// <summary> Потокобезопасный Dictionary - связка коннекта с именем пользователя
/// </summary>  
  TConnectionUsernameMap = class
  private
    AcceptedConnections: TDictionary<TIdContext, string>;
    MutexConnections: TMutex;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure AddConnection(AContext: TIdContext; username: string);
    function CheckConnectionExists(const key: TIdContext): boolean;
    function GetConnectionUsernameList(): TStringList;
    function GetConnectionUsername(const key: TIdContext): string;
    procedure RemoveConnection(AContext: TIdContext);

    property Items[const connect: TIdContext]: string
      read GetConnectionUsername; default;
  end;


  TServerManager = class(TObject)
  private
    TCPServer: TIdTCPServer;
    TLSServerHandler: TIdServerIOHandlerSSLOpenSSL;
    CertFileName :string;
    PrivateKeyFileName :string;
    AcceptedConnections: TConnectionUsernameMap;
    MutexConsole: TMutex;

    function ProcessIncludeParticipant(Context: TIdContext;
        const username: string; const password: string): boolean;
    function AuthenticateUser(Context: TIdContext; const username: string;
         const password: string): boolean;
    procedure SendHistory(const Context: TIdContext; const count: integer);
    procedure SendParticipantList(Context: TIdContext);
    function ProcessChatMessage(const Context: TIdContext;
        const text: string = ''): boolean;
    procedure DistributeMesg(const Context: TIdContext;
        const typemesg: integer; const username: string; const CurTime: TDateTime;
        const chatmesg: string = ''); overload;

    procedure WriteToConsole(const str: string);
  public
    constructor Create(aCertFileName :string = '';
        aPrivatekeyFileName :string = '');
    destructor Destroy(); override;
    procedure run(ipaddr: string; port: word);

    procedure incomingConnection(AContext: TIdContext);
    procedure removingConnection(AContext: TIdContext);
    procedure IdTCPServerExecute(AContext: TIdContext);
  end;


implementation

uses
  System.JSON, daux, IdExceptionCore;

type TConnectionData = class
  strict private
    FChatDb: TChatDb;
    FJsonSerializer: TJsonSerializer;
  public
    property ChatDb: TChatDb read FChatDb write FChatDb;
    property JsonSerializer: TJsonSerializer read FJsonSerializer
      write FJsonSerializer;
    constructor Create();
    destructor Destroy(); override;
  end;

{ TConnectionUsernameMap }

constructor TConnectionUsernameMap.Create;
begin
  AcceptedConnections := TDictionary<TIdContext, string>.Create();
  MutexConnections := TMutex.Create();
end;

destructor TConnectionUsernameMap.Destroy;
begin
  FreeAndNil(AcceptedConnections);
  FreeAndNil(MutexConnections);
  inherited;
end;

procedure TConnectionUsernameMap.AddConnection(AContext: TIdContext;
  username: string);
begin
  MutexConnections.Acquire();
  try
    if not AcceptedConnections.ContainsKey(AContext) then
      AcceptedConnections.Add(AContext, username);
  finally
    MutexConnections.Release();
  end;
end;

function TConnectionUsernameMap.CheckConnectionExists(
  const key: TIdContext): boolean;
begin
  result := false;
  MutexConnections.Acquire();
  try
    if AcceptedConnections.ContainsKey(key) then
      exit(true);
  finally
    MutexConnections.Release();
  end;
end;

function TConnectionUsernameMap.GetConnectionUsername(
  const key: TIdContext): string;
begin
  MutexConnections.Acquire();
  try
    if not AcceptedConnections.ContainsKey(key) then
      raise Exception.Create('Указанного соединения не существует');
    result := AcceptedConnections[key];
  finally
    MutexConnections.Release();
  end;
end;

function TConnectionUsernameMap.GetConnectionUsernameList: TStringList;
var
  key: TIdContext;
begin
  MutexConnections.Acquire();
  try
    result := TStringList.Create();
    for key in AcceptedConnections.Keys do
      result.Add(AcceptedConnections[key]);
  finally
    MutexConnections.Release();
  end;
end;

procedure TConnectionUsernameMap.RemoveConnection(AContext: TIdContext);
begin
  MutexConnections.Acquire();
  try
    if AcceptedConnections.ContainsKey(AContext) then
      AcceptedConnections.Remove(AContext);
  finally
    MutexConnections.Release();
  end;
end;

{ TConnectionData }

constructor TConnectionData.Create;
begin
  FChatDb := ChatDbConnectionPicker.GetChatDb();
  FJsonSerializer := TJsonSerializer.Create();
end;

destructor TConnectionData.Destroy;
begin
  if FJsonSerializer <> nil then FreeAndNil(FJsonSerializer);
  if FChatDb <> nil then FreeAndNil(FChatDb);
  inherited;
end;

{TServerManager}


constructor TServerManager.Create(aCertFileName :string;
    aPrivatekeyFileName :string);
begin
  inherited Create();
  CertFileName := aCertFileName;
  PrivateKeyFileName := aPrivatekeyFileName;
  if CertFileName = '' then
    CertFileName := 'cert.pem';
  if PrivateKeyFileName = '' then
    PrivateKeyFileName := 'privatekey.pem';
  TCPServer := TIdTCPServer.Create();
  TCPServer.OnExecute := IdTCPServerExecute;
  TCPServer.OnConnect := incomingConnection;
  TCPServer.OnDisconnect := removingConnection;
  TCPServer.Active := false;

  AcceptedConnections := TConnectionUsernameMap.Create();
  MutexConsole := TMutex.Create();


end;

destructor TServerManager.Destroy();
var list: TList;
  i: integer;
begin
//  list := TCPServer.Contexts.LockList;
//  try
//    for i := 0 to list.Count - 1 do
//      TIdContext(list[i]).Connection.Disconnect()
//  finally
//    if list <> nil then
//      TCPServer.Contexts.UnlockList;
//  end;
  if TCPServer <> nil then FreeAndNil(TCPServer);
  if AcceptedConnections <> nil then FreeAndNil(AcceptedConnections);
  if MutexConsole <> nil then FreeAndNil(MutexConsole);
  inherited;
end;


procedure TServerManager.WriteToConsole(const str: string);
begin
  MutexConsole.Acquire();
  try
    Writeln(str);
  finally
    MutexConsole.Release();
  end;
end;

procedure TServerManager.run(ipaddr: string; port: word);
begin

  try
    TLSServerHandler := TIdServerIOHandlerSSLOpenSSL.Create();
    TLSServerHandler.SSLOptions.KeyFile := 'privatekey.pem';
    TLSServerHandler.SSLOptions.CertFile := 'cert.pem';

    TLSServerHandler.SSLOptions.CipherList := 'ECDHE-RSA-AES256-GCM-SHA384:' +
      'ECDHE-RSA-AES128-GCM-SHA256:' + 'ECDHE-RSA-AES256-SHA384:' +
      'ECDHE-RSA-AES128-SHA256:' + 'ECDHE-RSA-AES256-SHA:' +
      'ECDHE-RSA-AES128-SHA:' + 'DHE-RSA-AES256-GCM-SHA384:' +
      'DHE-RSA-AES256-SHA256:' + 'DHE-RSA-AES256-SHA:' +
      'DHE-RSA-AES128-GCM-SHA256:' + 'DHE-RSA-AES128-SHA256:' +
      'DHE-RSA-AES128-SHA:' + 'DES-CBC3-SHA:' +
      '!ADH:!EXP:!RC4:!eNULL@STRENGTH';
    TLSServerHandler.SSLOptions.Mode := sslmServer;
    TLSServerHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    TLSServerHandler.SSLOptions.VerifyDepth := 7;
    TLSServerHandler.SSLOptions.VerifyMode := [sslvrfClientOnce];

    TCPServer.IOHandler := TLSServerHandler;

    TCPServer.Bindings.Add.IP := ipaddr;
    TCPServer.Bindings.Add.port := port;
    TCPServer.Active := true;


  except
    on E: EIdOSSLLoadingCertError do
    begin
      Writeln('Не удалось загрузить и проверить сертификат сервера. Проверьте '
        + 'правильность указания пути');
    end;
    on E: Exception do
    begin
      Writeln(E.ClassName + ': ' + E.Message);
    end;
  end;

end;


procedure TServerManager.incomingConnection(AContext: TIdContext);
var
  ConnectionData: TConnectionData;
begin
  if (AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase) then
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler)
      .PassThrough := false;

  //WriteLnSafeThr('Установлено соединение с  ' + AContext.Binding.PeerIP);


//  ADOConnection.ConnectionString := 'Provider=MSOLEDBSQL.1;Password=test;' +
//    'Persist Security Info=True;User ID=sa;Initial Catalog=chatDB;' +
//    'Data Source=WINDEV2311EVAL;Initial File Name="";Server SPN="";' +
//    'Authentication="";Access Token=""';

  ConnectionData := TConnectionData.Create();
  ConnectionData.ChatDB.Connect();
  AContext.Data := ConnectionData;
  AContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
end;

procedure TServerManager.removingConnection(AContext: TIdContext);
var
  CurTime: TDateTime;
  username: string;
  ConnectionData: TConnectionData;
begin
  username := AcceptedConnections[AContext];


  CurTime := now;
  DistributeMesg(AContext, mtLeaveParticipant, username, CurTime);
  WriteToConsole(Format('Пользователь %s отключился в %s',
    [username, FormatDateTime('dd.mm.yy hh:mm:ss', CurTime)]));

  AcceptedConnections.RemoveConnection(AContext);
  ConnectionData := AContext.Data AS TConnectionData;
  FreeAndNil(ConnectionData);
  AContext.Data := nil;

end;

procedure TServerManager.IdTCPServerExecute(AContext: TIdContext);
var
  mesg: string;
  MesgType: integer;
  ClientMsg: TClientMsg;
  LoginMsg: TLoginMsg;
  Serializer: TJsonSerializer;
begin
  // ====== Принятие сообщений ======
  try
    mesg := '';
    mesg := AContext.Connection.IOHandler.Readln();


    // ====== Парсинг и обработка =======
    if mesg <> '' then
    begin
      MesgType := GetMesgType(mesg);
      ClientMsg := TClientMsg.Create(MesgType, #0);
      if ClientMsg.MsgType in [mtLogin, mtChatMesgFromClient] then
      begin
        Serializer := TConnectionData(AContext.Data).JsonSerializer;
        if MesgType = mtLogin then
        begin
          LoginMsg := Serializer.Deserialize<TLoginMSg>(mesg);
          ProcessIncludeParticipant(AContext, LoginMsg.Username, LoginMsg.Password);
        end
        else if MesgType = mtChatMesgFromClient then
        begin
          ClientMsg := Serializer.Deserialize<TClientMsg>(mesg);
          ProcessChatMessage(AContext, ClientMsg.Text);
        end;
      end;
    end;

  except
    on e: EIdNotConnected do
    begin
    end;
    on e: EIdConnClosedGracefully do
    begin
      // WriteLnSafeThr('Разорвано соединение c ' + AContext.Binding.Ip) ;
    end;
    on e: Exception do
    begin
      WriteToConsole('Произошла ошибка ' + E.ClassName + ': ' + E.Message);
    end;
  end;

end;

function TServerManager.ProcessIncludeParticipant(Context: TIdContext;
  const username, password: string): boolean;
var
  TimeNowStr: TDateTime;
  JSONtoWrite: TJSONObject;
begin
  result := false;
  if username = '' then
    raise EInvalidFormatMessageRequest.Create('Пользователь не указан');
  // 2                                                            *
  if not AuthenticateUser(Context, username, password )
  then
  begin // отказ во включении в чат
    sleep(200);
    Context.Connection.IOHandler.WriteLn(Format('{"MsgType":%d}',
      [mtLoginReject]));
    FreeAndNil(JSONtoWrite);
    exit;
  end;

  TimeNowStr := now;
  WriteToConsole(Format('Пользователь %s подключен к чату в %s',
    [username, FormatDateTime('dd.mm.yy hh:mm:ss', TimeNowStr)]));

  Context.Connection.IOHandler.WriteLn(Format('{"MsgType":%d}',
      [mtLoginAccept]));


  SendHistory(Context, 25);
  SendParticipantList(Context);

  AcceptedConnections.AddConnection(Context, username);

  DistributeMesg(Context, mtConnectInformation, username, now);

  result := true;

end;

function TServerManager.AuthenticateUser(Context: TIdContext; const username: string;
  const password: string): boolean;
var ChatDb: TChatDb;
  ConnectionData: TConnectionData;
begin
  ConnectionData := Context.Data  as TConnectionData;
  ChatDb := ConnectionData.ChatDb;
  result := chatDb.AuthenticateUser(username, password);
end;

procedure TServerManager.SendHistory(const Context: TIdContext;
    const count: integer);
var
  JSONStr: string;
  time: TDateTime;
  ConnectionData: TConnectionData;
  ChatDb: TChatDb;
  RecordProc: TProc<TDataSet> ;
  Serializer: TJsonSerializer;
begin

  ConnectionData := Context.Data  as TConnectionData;
  ChatDb := ConnectionData.ChatDb;
  Serializer := ConnectionData.JsonSerializer;

  RecordProc := procedure(q: TDataSet)
    begin
      time := q.FieldByName('time_send').AsDateTime;
      JSONStr := Serializer.Serialize<TNetMsg>(TNetMsg.Create(mtHistoryMesg,
         q.FieldByName('username').AsString,
         FormatDateTime('dd.mm.yy hh:mm:ss', time),
         q.FieldByName('message').AsString));
      Context.Connection.IOHandler.WriteLn(JSonStr);
    end;

  ChatDb.ProcessHistoryRecord(RecordProc, 15);


end;

procedure TServerManager.SendParticipantList(Context: TIdContext);
var ListConnectionUsername: TStringList;
  ConnectionUsername: string;
  ConnectionData: TConnectionData;
  Serializer: TJsonSerializer;
  JsonStr: string;
begin
  ConnectionData := Context.Data  as TConnectionData;
  Serializer := ConnectionData.JsonSerializer;
  ListConnectionUsername := AcceptedConnections.GetConnectionUsernameList();
  try
    for ConnectionUsername in ListConnectionUsername do
    begin
//      JSONToWrite := TJSONObject.Create();
//      try
//        JSONToWrite.AddPair('MsgType', IntToStr(mtParticipantsInfo));
//        JSONToWrite.AddPair('sender', ConnectionUsername);
//        Context.Connection.IOHandler.WriteLn(JSONToWrite.ToString());
//      finally
//        FreeAndNil(JSONToWrite);
//      end;
      JSonStr := Serializer.Serialize<TParticipantIndo>(TParticipantIndo.Create
          (mtParticipantsInfo, ConnectionUsername));
      Context.Connection.IOHandler.WriteLn(JSONStr);

    end;
  finally
    FreeAndNil(ListConnectionUsername);
  end;
end;

function TServerManager.ProcessChatMessage(const Context: TIdContext;
    const text: string): boolean;
var
  username: String;
  time: TDateTime;
  ChatDb: TChatDb;
  ConnectionData: TConnectionData;
begin
  ConnectionData := Context.Data  as TConnectionData;
  ChatDb := ConnectionData.ChatDb;
  username := '';
  username := AcceptedConnections[Context];

  time := now;
  ChatDb.SaveMessage(username, time, text);
  DistributeMesg(Context, mtChatMesgFromServer, username, time, text);
  result := true;
end;


procedure TServerManager.DistributeMesg(const Context: TIdContext;
    const typemesg: integer; const username: string; const CurTime: TDateTime;
    const chatmesg: string);
var
  list: TList;
  Connection: TIdContext;
  TimeNowStr: string;
  JsonStr: string;
  i: integer;
  Serializer: TJsonSerializer;
begin
  TimeNowStr := FormatDateTime('dd.mm.yy hh:mm:ss', CurTime);

  Serializer := TConnectionData(Context.Data).JsonSerializer;

  JsonStr :=  Serializer.Serialize<TNetMsg>(TNetMsg.Create(typemesg, username,
      TimeNowStr, chatmesg));

  list := TCPServer.Contexts.LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      Connection := TIdContext(list[i]);
      if AcceptedConnections.CheckConnectionExists(connection) and
          ((typemesg <> mtLeaveParticipant) or (Connection <> Context)) then
        TIdContext(Connection).Connection.IOHandler.WriteLn
          (JsonStr);
    end;

  finally
    if list <> nil then
      TCPServer.Contexts.UnlockList;
  end;
end;


end.
