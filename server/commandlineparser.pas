unit commandlineparser;

interface

uses System.SysUtils,
  System.Classes;

type
  ECommandLineIncorrectSyntax = class(Exception)
  end;

type
  ECommandLineIncorrectRequest = class(Exception)
  end;


type
  TUserCommand = (CommandStart, CommandAddUser, CommandRemoveUser,
    CommandResetUser, CommandIncorrect, CommandExport);

type
  TServerStartParams = record
    IpAddr: string;
    port: Word;
    KeyFile: string;
    CertFile: string;
  end;

type
  TExportParams = record
    filename: string;
    TimeStart: string;
    TimeEnd: string;
  end;

type
  TCommandLineParser = class
  private
    FTypeCommand: TUserCommand;
    FServerParams: TServerStartParams;
    FUserName: string;
    FExportParams: TExportParams;
    FSqlSettingFileName: string;

    function GetUserName(): string;
    function GetServerStartParams(): TServerStartParams;
  public
    constructor Create();
    procedure ClearParams();
    function Parse(): TUserCommand;
    property TypeCommand: TUserCommand read FTypeCommand;
    property ServerParams: TServerStartParams read GetServerStartParams;
    property UserName: string read GetUserName;
    property SqlSettingFileName: string read FSqlSettingFileName write
      FSqlSettingFileName;

    class function GetPasswordHide(): string;
  end;

implementation

uses  System.Generics.Collections, Windows, Math, System.StrUtils,
    System.RegularExpressions;

constructor TCommandLineParser.Create();

begin
  inherited;
  FTypeCommand := CommandIncorrect;
end;

procedure TCommandLineParser.ClearParams();
begin
  FServerParams.IpAddr := '';
  FServerParams.port := 0;
  FServerParams.KeyFile := '';
  FServerParams.CertFile := '';
  FUserName := '';
  FExportParams.TimeStart := '';
  FExportParams.TimeEnd := '';
end;

function TCommandLineParser.Parse(): TUserCommand;
  function GetNextRegGrpValue(GrpEnum: TGroupCollectionEnumerator): string;
  begin
    // Delphi обёртка не поддерживает нормальную проверку захвата группы по имени,
    // (сейчас можно только проверять, вызовет ли Group[<имя группы>] exception).
    // Также нельзя проести проверку имени группы по индексу.
    // Поэтому будем проходить по TGroupCollectionEnumerator
    result := '';
    if GrpEnum.MoveNext() then
      result := Trim(GrpEnum.Current.Value);
  end;
var

  RegStartServer, RegUser: string;
  RegEx: TRegEx;
  matches: TMatchCollection;
  AllArgStr: string;
  i, val: integer;
  s: string;
  StrValues: TArray<string>;
  GrpEnum: TGroupCollectionEnumerator;
begin
  AllArgStr := '';
  for i := 1 to ParamCount do
    AllArgStr := AllArgStr + ' ' + ParamStr(i);

  RegStartServer := // например, -start 127.0.0.1 2006 d:\is\cert.pem d:\is\key.pem d:\chatserver\main.conf
    '-start(?<address> [^\s]+)?' + '(?<port> \d{1,5})' +
      '(?<cert> [^\s]+ [^\s]+)?' + '(?<setting_file> [^\s]+)?';

  RegUser :=  // например, -adduser admin1 d:\chatserver\main.conf
    '(?<command>(?:-adduser)|(?:-removeuser)|(?:-resetuser)) '+
      '(?<username>[^\s]+)' + '(?<conf_file> [^\s]+)?';

  FTypeCommand := CommandIncorrect;

  ClearParams();
  try
    RegEx := TRegEx.Create(RegStartServer);
    matches:= regex.Matches(AllArgStr);
    if matches.Count > 0 then
      with matches[0] do
      begin
        GrpEnum := Groups.GetEnumerator();
        GrpEnum.MoveNext;

        s := GetNextRegGrpValue(GrpEnum);
        FServerParams.IpAddr := IfThen(s <> '', s, '127.0.0.1');

        s := GetNextRegGrpValue(GrpEnum);
        val := StrToIntDef(s, - 1);
        if ((Val > 0) and (Val < 65535)) then
          FServerParams.port := val;         // опорный элемент (всегда обязателен);

        s := GetNextRegGrpValue(GrpEnum);
        StrValues := SplitString(s, ' ');
        if High(StrValues) > 0 then
        begin
          FServerParams.KeyFile := StrValues[0];
          FServerParams.CertFile := StrValues[1];
        end;

        s:= GetNextRegGrpValue(GrpEnum);
        FSqlSettingFileName := s;

        FTypeCommand := CommandStart;
      end;

    if FTypeCommand <> CommandIncorrect then
      exit(FTypeCommand);

    RegEx := TRegEx.Create(RegUser);
    matches:= regex.Matches(AllArgStr);
    if matches.Count > 0 then
      with matches[0] do
      begin
        GrpEnum := Groups.GetEnumerator();
        GrpEnum.MoveNext;

        s := GetNextRegGrpValue(GrpEnum);
        if s = '-adduser' then
          FTypeCommand := CommandAddUser
        else if s = '-removeuser' then
          FTypeCommand := CommandRemoveUser
        else if s = '-resetuser' then
          FTypeCommand := CommandResetUser;

        s := GetNextRegGrpValue(GrpEnum);
        FUserName := s;

        s := GetNextRegGrpValue(GrpEnum);
        FSqlSettingFileName := s;

      end;

   result := FTypeCommand;
   if FTypeCommand = CommandIncorrect then
    raise ECommandLineIncorrectSyntax.Create('Некорретный синтаксис команды') ;
  except
    on e:exception do
      raise ECommandLineIncorrectSyntax.Create(e.Message);
  end;

end;

function TCommandLineParser.GetServerStartParams(): TServerStartParams;
var
  empty: TServerStartParams;
begin
  empty.IpAddr := '';
  empty.port := 0;
  result := empty;
  if (FTypeCommand = CommandStart) then
    result := FServerParams
  else
    raise ECommandLineIncorrectRequest.Create
      ('Параметры сервера не указываются в данном формате команды');
end;

function TCommandLineParser.GetUserName(): string;
begin
  result := '';
  if (FTypeCommand = CommandAddUser) or (FTypeCommand = CommandRemoveUser) or
    (FTypeCommand = CommandResetUser) then
    result := FUserName
  else
    raise ECommandLineIncorrectRequest.Create
      ('Параметры имени пользователя не указываются в данном формате команды');
end;


class function TCommandLineParser.GetPasswordHide(): string;
var
  OldMode: cardinal;
  c: Char;
begin
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode and
    not(ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT));
  try
    while not Eof do
    begin
      Read(c);
      if c = #13 then
        Break;
      if (c = #8) and (Length(result) > 0) then // Backspace
      begin
        Delete(result, Length(result), 1);
        Write(#8);
      end
      else
      begin
        result := result + c;
      end;
    end;

  finally
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  end;
end;

end.
