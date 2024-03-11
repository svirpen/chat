unit chatdbinteraction;

interface

uses System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  ActiveX,
  Data.DB,
  SyncObjs,
  Data.Win.ADODB,
  IniFiles;

type

  EEmptySelection = class(Exception)

  end;

  type TSqlSetting = TPair<string, string>;
  type TSQLSettings = TList<TSqlSetting>;

//  IChatDb =  interface ['{C9182935-5F93-4C21-A5AA-0BA48BA9946C}']
//    procedure Connect(); overload;
//    procedure Connect(const username, password: string); overload;
//    function CheckPassword(): boolean; overload;
//    function CheckPassword(const username, password: string): boolean; overload;
//
//    procedure AddUser(const username: string; const password: string);
//    procedure RemoveUser(const username: string);
//    procedure ResetPasswordUser(const username: string; const password: string);
//
//    function AuthenticateUser(const username: string; const password: string): boolean;
//    procedure ProcessHistoryRecord(const Proc: TProc<TDataSet>; const count: integer);
//    procedure SaveMessage( const username: string; const time: TDateTime;
//        const MessageText: string);
//  end;

//  TChatDb = class(TSingletonImplementation, IChatDb)
  TChatDb = class
  private
    FMainUSer, FMainPassword: string;
  public
    procedure Connect(); overload; virtual; abstract;
    procedure Connect(const username, password: string); overload; virtual; abstract;
    function CheckPassword(): boolean; overload; virtual; abstract;
    function CheckPassword(const username, password: string): boolean; overload; virtual; abstract;

    procedure AddUser(const username: string; const password: string);
        virtual; abstract;
    procedure RemoveUser(const username: string); virtual; abstract;
    procedure ResetPasswordUser(const username: string; const password: string);
        virtual; abstract;
    function AuthenticateUser(const username: string; const password: string):
        boolean; virtual; abstract;
    procedure ProcessHistoryRecord(const Proc: TProc<TDataSet>;
        const count: integer); virtual; abstract;
    procedure SaveMessage(const username: string; const time: TDateTime;
        const MessageText: string); virtual; abstract;
  end;

  TAdoChatDb = class(TChatDb)
  private
    ADOConnection: TADOConnection;
    Command: TADOCommand;
    Query: TAdoQuery;

    const SectionName = 'ADO';

    procedure Init(const ConnectionStr: string);
  public
    constructor Create(const ParamValues: TSqlSettings = nil);
    destructor Destroy(); override;
    procedure Connect(); overload; override;
    procedure Connect(const username, password: string); overload; override;
    function CheckPassword(): boolean; overload; override;
    function CheckPassword(const username, password: string): boolean; overload;
      override;

    procedure AddUser(const username: string; const password: string); override;
    procedure RemoveUser(const username: string); override;
    procedure ResetPasswordUser(const username: string;
         const password: string); override;
    function AuthenticateUser(const username: string; const password: string):
         boolean; override;
    procedure ProcessHistoryRecord(const Proc: TProc<TDataSet>;
         const count: integer); override;
    procedure SaveMessage( const username: string; const time: TDateTime;
        const MessageText: string); override;
  end;


  TChatDbBuilder = class
  protected
    function GetSectionName(): string; virtual; abstract;
    procedure CreateDefaultFile(const filename: string;
      var SqlParams: TSQLSettings); virtual; abstract;
    function PrepareSettingsFromFile(const filename: string):
       TSQLSettings; virtual;
  public
    class function MakeDBClass(const SQlSettings:  TSQLSettings):
       TChatDb; virtual; abstract;

  end;

  TChatDbBuilderRef = class of TChatDbBuilder;

  TADOChatDbBuilder = class(TChatDbBuilder)
  private
    const SECTION_NAME = 'ADO';
  protected
    function GetSectionName(): string; override;
    procedure CreateDefaultFile(const filename: string;
      var SqlParams: TSQLSettings); override;
    function PrepareSettingsFromFile(const filename: string):
      TSQLSettings; override;
  public
    class function MakeDBClass(const SQlSettings:  TSQLSettings): TChatDb; override;
  end;

  TChatDbConnectionPicker = record //singletone
  private
    const DefaultConnect = 'ADO';
    class var
      FSettingFileName: string;
      FSqlConnectionSettings: TSQLSettings;
      FConnTypeStr: string;
      FMainUser: string;
      FMainPassword: string;
      FChatDb: TChatDb;
      Lock: TCriticalSection;

    class procedure MultiThreadLock(proc: TProc); static;
    class function GetConnectionType(): string; static;
    class function MakeChatDb(): TChatDb; static;

    class procedure SetSettingFileName(const aFilename: string); static;
    class function GetSettingFileName(): string; static;
    class procedure SetMainUser(const aUsername: string); static;
    class function GetMainUser(): string; static;
    class procedure SetMainPassword(const aPassword: string); overload; static;
    class function GetMainPassword(): string; static;
  public
    class function GetChatDb(): TChatDb; static;
    class procedure Clear(); static;
    class function CheckPassword(const aMainPassword: string): boolean; static;
    class function SetMainPassword(const aMainPassword: string;
        const IsNeedCheck: boolean = true): boolean; overload; static;

    class property SettingFileName: string read GetSettingFileName
      write SetSettingFileName;
    class property MainUser: string read GetMainUser write SetMainUser;
    class property MainPassword: string read GetMainPassword
      write SetMainPassword;
  end;

var ChatDbConnectionPicker: TChatDbConnectionPicker;

implementation


{ TChatDbInteraction }


constructor TAdoChatDb.Create(const ParamValues: TSqlSettings);
var ConnectionStr: string;
  SqlSetting: TSqlSetting;
  ConnectSettings: TSqlSettings;
begin

  ConnectionStr := '';

  try
   for SqlSetting in ParamValues do
     if SqlSetting.Value = '' then
       ConnectionStr := ConnectionStr + SqlSetting.Key + '="";'
     else
       if  SqlSetting.Key = 'User ID'  then
       begin
         FMainUSer := SqlSetting.Value;
         ConnectionStr := ConnectionStr + 'User ID=%s;';
       end
       else
         ConnectionStr := ConnectionStr + SqlSetting.Key + '=' + SqlSetting.Value + ';'
  finally
    if ParamValues = nil then FreeAndNil(ConnectSettings);
  end;

  ConnectionStr := ConnectionStr + 'Password=%s;';

  Init(ConnectionStr);
end;

destructor TAdoChatDb.Destroy();
begin
  if ADOConnection <> nil then ADOConnection.Connected := false;
  if ADOConnection <> nil then FreeAndNil(Query);
  if ADOConnection <> nil then FreeAndNil(Command);
  if ADOConnection <> nil then FreeAndNil(ADOConnection);
  CoUninitialize();
end;

procedure TAdoChatDb.Init(const ConnectionStr: string);
begin
  CoUninitialize();
  CoInitialize(nil);
  ADOConnection := TADOConnection.Create(Nil);
  ADOConnection.Connected := false;
  ADOConnection.ConnectionString := ConnectionStr;
//  ADOConnection.ConnectionString := 'Provider=MSOLEDBSQL.1;Password=test;' +
//    'Persist Security Info=True;User ID=sa;Initial Catalog=chatDB;' +
//    'Data Source=WINDEV2311EVAL;Initial File Name="";Server SPN="";' +
//    'Authentication="";Access Token=""';

//'Authentication="";Password=passwd;Provider=MSOLEDBSQL.1;' +
//'Persist Security Info=true;Access Token="";Initial File Name="";
//'Server SPN="";Initial Catalog=chatDB;Data Source=WINDEV2311EVAL;'

  ADOConnection.CommandTimeout := 5;
  Command := TADOCommand.Create(nil);
  Command.Connection := ADOConnection;
  Query := TADOQuery.Create(nil);
  Query.Connection := ADOConnection;

end;

procedure TAdoChatDb.Connect();
begin
  connect(FMainUser, FMainPassword);
end;

procedure TAdoChatDb.Connect(const username, password: string);
var OldConnectionStr: string;
begin
  OldCOnnectionStr := ADOConnection.ConnectionString;
  ADOConnection.ConnectionString := Format(ADOConnection.ConnectionString,
    [username, password]);
  try
    ADOConnection.Connected := true;
  except
    on e: exception do
    begin
      ADOConnection.ConnectionString := OldConnectionStr;
      raise;
    end;
  end;
end;

function TAdoChatDb.CheckPassword(): boolean;
begin
  result := CheckPassword(FMainUSer, FMainPassword);
end;

function TAdoChatDb.CheckPassword(const username, password: string): boolean;
var OldConnectionStr: string;
begin

  result := true;
  OldConnectionStr := ADOConnection.ConnectionString;
  try
    try
      ADOConnection.ConnectionString := Format(ADOConnection.ConnectionString,
        [username, password]);
      ADOConnection.Connected := true;
    except
      on e: EDatabaseError do
        if (Copy(e.Message, 1, 22) = 'Login failed for user ') then
          result := false
        else
          raise;
    end;
  finally
    ADOConnection.Connected := false;
    ADOConnection.ConnectionString := OldConnectionStr;
  end;
end;

function TAdoChatDb.AuthenticateUser(const username,
  password: string): boolean;
var
  str: String;
begin

  if ADOConnection.InTransaction then
    ADOConnection.RollbackTrans();
  ADOConnection.BeginTrans();

  query.SQL.Text := 'SELECT dbo.validate_user (''' + username + ''', ''' +
    password + ''') AS status';
  query.Open();
  str := query.FieldByName('status').AsString;
  result := query.FieldByName('status').AsBoolean;

  if ADOConnection.InTransaction then
    ADOConnection.CommitTrans();


end;

procedure TAdoChatDb.AddUser(const username: string; const password: string);
begin

  if ADOConnection.InTransaction then
    ADOConnection.RollbackTrans();
  ADOConnection.BeginTrans();

// вызов хранимой процедуры, генерирующую соль и хэш SHA256
  Command.CommandText := Format('EXEC add_user @_username = ''%s'', ' +
    '@_password = ''%s''', [username, password]);
  Command.Execute();

  ADOConnection.CommitTrans();

end;

procedure TAdoChatDb.RemoveUser(const username: string);
begin

  if ADOConnection.InTransaction then
    ADOConnection.RollbackTrans();
  ADOConnection.BeginTrans();

  Command.CommandText := Format(
    'DELETE ' +
    'FROM participants ' +
    'WHERE username = ''%s''', [username]);
  Command.Execute();

  ADOConnection.CommitTrans();
end;

procedure TAdoChatDb.ResetPasswordUser(const username: string;
  const password: string);
begin
  if ADOConnection.InTransaction then
    ADOConnection.RollbackTrans();
  ADOConnection.BeginTrans();

  Command.CommandText := Format('EXEC reset_password @_username = ''%s'', ' +
    '@_password = ''%s''', [username, password]);
  Command.Execute();


  ADOConnection.CommitTrans();

end;

procedure TAdoChatDb.ProcessHistoryRecord(const Proc: TProc<TDataSet>; const count:
  integer);
begin

  if ADOConnection.InTransaction then
    ADOConnection.RollbackTrans();
  ADOConnection.BeginTrans();

  query.SQL.Text := Format(
    'SELECT TOP %d username, message, time_send ' +
    'FROM history ' +
    'LEFT JOIN participants ON sender = participants.id ' +
    'ORDER BY history.id DESC', [count]);
  query.Open();

  while not query.Eof do
  begin
    Proc(query);
    query.Next();
  end;

  ADOConnection.CommitTrans();


end;

procedure TAdoChatDb.SaveMessage( const username: string;
    const time: TDateTime; const MessageText: string);
var
  timestr: string;
begin

  timestr := FormatDateTime('dd.mm.yyyy hh:mm:ss', time);

  if ADOConnection.InTransaction then
    ADOConnection.RollbackTrans();
  ADOConnection.BeginTrans();

  Command.CommandText :=
    Format('INSERT INTO history (sender, message, time_send) VALUES ' +
   '((SELECT TOP 1 ID FROM participants WHERE username = ''%s''), ' +
    '''%s'', convert(datetime, ''%s'', 104))', [username, MessageText, timestr]);
  Command.Execute();

  if ADOConnection.InTransaction then
    ADOConnection.CommitTrans();

end;

{ TCHatDbBuilder }

function TCHatDbBuilder.PrepareSettingsFromFile(
  const filename: string): TSQLSettings;
var fullname: string;
  ConfFIle: TIniFile;
  ParamVAlues: TSQLSettings;
  StrValues: TStringList;
  str: string;
  DelimPos: integer;
begin
  if filename <> '' then
    fullname := GetCurrentDir + '\' + filename;

  if not FileExists(fullname) then
  begin
    CreateDefaultFile(fullname, ParamValues);
    exit(ParamValues);
  end;

  ParamValues := TSQLSettings.Create();
  ConfFile := TIniFile.Create(fullname);
  try
    ParamValues.Clear();
    StrValues := TStringList.Create();
    try
      ConfFile.ReadSectionValues(GetSectionName(), StrValues);
      for str in StrValues do
      begin
        DelimPos := pos('=', str);
        if (str = '=') or (DelimPos <= 0) then continue;
        ParamValues.Add(TSQLSetting.Create(copy(str, 0, DelimPos - 1),
          copy(str, DelimPos + 1, length(str) - DelimPos)));
      end;
    finally
      FreeAndNil(StrValues);
    end;
    result := ParamValues;
  finally
    FreeAndNil(ConfFIle);
  end;
end;

{ TADOChatDbBuilder }

function TADOChatDbBuilder.PrepareSettingsFromFile(
  const filename: string): TSQLSettings;
begin
  result := inherited PrepareSettingsFromFile(filename);
end;

procedure TADOChatDbBuilder.CreateDefaultFile(const filename: string;
  var SqlParams: TSQLSettings);
var SQLSettingsFile: TIniFile;
  SQlParam: TSqlSetting;
begin
  SQLSettingsFile := TIniFile.Create(filename);
  try
    SqlParams.Clear();

    SqlParams.Add(TSQLSetting.Create('Provider', 'MSOLEDBSQL.1'));
    SqlParams.Add(TSQLSetting.Create('Persist Security Info', 'true'));
    SqlParams.Add(TSQLSetting.Create('Data Source', GetEnvironmentVariable('COMPUTERNAME')));
    SqlParams.Add(TSQLSetting.Create('Initial Catalog', 'chatDB'));
    SqlParams.Add(TSQLSetting.Create('Initial File Name', ''));
    SqlParams.Add(TSQLSetting.Create('Server SPN', ''));
    SqlParams.Add(TSQLSetting.Create('Authentication', ''));
    SqlParams.Add(TSQLSetting.Create('Access Token', ''));

    for SQlParam in SqlParams do
      SQLSettingsFile.WriteString(GetSectionName, SQlParam.key, SqlParam.Value);

  finally
    FreeAndNil(SQLSettingsFile);
  end;
end;

function TADOChatDbBuilder.GetSectionName: string;
begin
  result := SECTION_NAME;
end;

class function TADOChatDbBuilder.MakeDBClass(const SQlSettings: TSQLSettings): TChatDb;
begin
  result := nil;
  try
    result := TAdoChatDb.Create(SQlSettings);
  except
    on e: EDatabaseError do
    begin
      if Copy(e.Message, 1, 22) = 'Login failed for user ' then
        raise;
    end;
  end;
end;

{ TChatDbConnectionPicker }

class function TChatDbConnectionPicker.GetChatDb: TChatDb;
var res: TChatDb;
begin
  MultiThreadLock(procedure
  begin
    res := MakeChatDb;
    FChatDB := nil;
  end);
  result := res;
end;

class function TChatDbConnectionPicker.MakeChatDb(): TChatDb;
var ChatDbBuilder: TChatDbBuilder;
begin

  if FSettingFileName = '' then
    FSettingFileName := 'SqlSettings.conf';

  if FConnTypeStr = '' then
    GetConnectionType();

  if FChatDB <> nil then
    exit(FChatDb);

  if FConnTypeStr = 'ADO' then
    ChatDbBuilder := TADOChatDbBuilder.Create()
  else // здесь будут if с другими коннектами
    ChatDbBuilder := TADOChatDbBuilder.Create();

  try
     if FSqlConnectionSettings = nil then
      FSqlConnectionSettings := ChatDbBuilder.PrepareSettingsFromFile(
        FSettingFileName);

    FChatDb := ChatDbBuilder.MakeDBClass(FSqlConnectionSettings);
    FChatDb.FMainPassword := FMainPassword;
    result := fChatDB;
  finally
    FreeAndNil(ChatDbBuilder);
  end;
end;

class procedure TChatDbConnectionPicker.Clear;
begin
  MultiThreadLock(procedure
  begin
    FConnTypeStr := '';
    if FSqlConnectionSettings <> nil then  FreeAndNil(FSqlConnectionSettings);
    if FChatDb <> nil then FreeAndNil(FChatDb);
    if Lock <> nil then FreeAndNil(Lock);
  end);
end;

class procedure TChatDbConnectionPicker.MultiThreadLock(proc: TProc);
begin
  Lock.Acquire();
  try
    Proc();
  finally
    Lock.Release();
  end;
end;

class function TChatDbConnectionPicker.GetConnectionType(): string;
var  SectionList: TStringList;
  fullname: string;
  SettingFile: TIniFile;
begin
  if FSettingFileNAme <> '' then
    fullname := GetCurrentDir + '\' + FSettingFileName;

  FConnTypeStr := '';
  if FileExists(FSettingFileNAme) then
  begin
    SectionList := TStringList.Create();
    SettingFile := TIniFile.Create(FSettingFileName);
    try
      SettingFile.ReadSections(SectionList);
      if SectionList.Count = 1 then
        FConnTypeStr := SectionList[0]
      else
        FConnTypeStr := SettingFile.ReadString('GENERAL', 'USE', '');
    finally
      FreeAndNil(SettingFile);
      if SectionList <> nil then FreeAndNil(SectionList);
    end;
  end
  else
    FConnTypeStr := DefaultConnect;

  result := FConnTypeStr;
end;

class procedure TChatDbConnectionPicker.SetSettingFileName(
  const aFilename: string);
begin
  MultiThreadLock(procedure
  begin
    FSettingFileName := aFileName;
    GetConnectionType();
  end);
end;

class function TChatDbConnectionPicker.GetSettingFileName(): string;
var res: string;
begin
  MultiThreadLock(procedure
  begin
    res := FSettingFileName;
  end);
end;

class function TChatDbConnectionPicker.SetMainPassword(const aMainPassword: string;
  const IsNeedCheck: boolean): boolean;
begin
  result := true;
  MultiThreadLock(procedure
  begin
    FMainPassword := aMainPassword;
  end);
  if not IsNeedCheck then exit;
  result := CheckPassword(aMainPassword);
  if not result then FMainPassword := '';
end;

class function TChatDbConnectionPicker.CheckPassword(const
    aMainPassword: string): boolean;
var res: boolean;
begin
  MultiThreadLock(procedure
  begin
    if FChatDb = nil then
      FChatDb := MakeChatDb();
    FChatDb.FMainPassword := aMainPassword;
    res := FChatDb.CheckPassword();
    if res then exit;
    FChatDb.FMainPassword := '';
  end);
  result := res;
end;

class function TChatDbConnectionPicker.GetMainPassword: string;
var res: string;
begin
  MultiThreadLock(procedure
  begin
    res := FMainPassword;
  end);
  result := res;
end;


class procedure TChatDbConnectionPicker.SetMainUSer(const aUsername: string);
begin
  MultiThreadLock(procedure
  begin
    FMainUser := aUsername;
  end);
end;

class function TChatDbConnectionPicker.GetMainUser: string;
var username: string;
begin
  MultiThreadLock(procedure
  begin
    username := FMainUser;
  end);
  result := username;
end;

class procedure TChatDbConnectionPicker.SetMainPassword(const aPassword: string);
begin
  MultiThreadLock(procedure
  begin
    FMainPassword := aPassword;
  end);
end;



initialization
  TChatDbConnectionPicker.Lock := TCriticalSection.Create();
finalization
  TChatDbConnectionPicker.Clear();
end.
