program ChatServer;

{$APPTYPE CONSOLE}
//{$R *.res}


{$R *.dres}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  SyncObjs,
  IdBaseComponent,
  IdComponent,
  IdCustomTCPServer,
  IdTCPServer,
  IdContext,
  IdServerIOHandlerSocket,
  IdException,
  IdSSLOpenSSL,
  ActiveX,
  Data.DB,
  Data.Win.ADODB,
  System.JSON,
  daux in 'common\daux.pas',
  netmessagesparser in 'common\netmessagesparser.pas',
  servermanager in 'server\servermanager.pas',
  commandlineparser in 'server\commandlineparser.pas',
  chatdbinteraction in 'server\chatdbinteraction.pas';

function CheckDBPassword(): boolean;
var password: string;
  i: integer;
begin
  result := false;
  Writeln('Введите пароль суперпользователя БД  ' +
      '(во время ввода не будут отображатся символы):');
  password := TCommandLineParser.GetPasswordHide();
  for i := 0 to 2 do
  begin
    if ChatDbConnectionPicker.SetMainPassword(password, true) then
      exit(true);
    password := '';
    if i = 2 then continue;
    read;
    Writeln('Неверный пароль. Попробуйте ввести повторно: ');
    password := TCommandLineParser.GetPasswordHide();
  end;
  Writeln('Трехкратный ввод неправильного пароля');
end;

procedure MainProcedure();
const
  ConsoleMessageIncorrestSyntax = 'Неверный синтаксис команды';
var
  commandlineparser: TCommandLineParser;
  servermanager: TServerManager;
  chatDB: TChatDb;
  Command: TUserCommand;
  ServerParams: TServerStartParams;

  username, password: string;
  c: char;

begin
  // CoInitialize(nil);

  commandlineparser := TCommandLineParser.Create();
  chatDb := nil;
  try
    Command := commandlineparser.Parse();
    if Command = CommandIncorrect then
      raise ECommandLineIncorrectSyntax.Create('');

    if Command = CommandStart then
    begin
      ServerParams := commandlineparser.ServerParams;

      if commandlineparser.SqlSettingFileName <> '' then
        TChatDbConnectionPicker.SettingFileName := commandlineparser.SqlSettingFileName;
      if not CheckDBPassword() then exit;

      servermanager := TServerManager.Create(ServerParams.CertFile,
          ServerParams.KeyFile);
      try
        servermanager.Run(ServerParams.IpAddr, ServerParams.port);
        Writeln(Format('Сервер прослушивает порт %d. Для того, чтобы остановить ' +
          'сервер, нажмите клавишу q',[ServerParams.port]));
        repeat
          c := ReadKey();
        until (c = 'q') or (c = 'Q');

      finally
        FreeAndNil(servermanager);
      end;
    end // ----------------------------------------------------------
    else if (Command = CommandAddUser) or (Command = CommandRemoveUser) or
      (Command = CommandResetUser) then
    begin

      if commandlineparser.SqlSettingFileName <> '' then
        TChatDbConnectionPicker.SettingFileName := commandlineparser.SqlSettingFileName;
      if not CheckDBPassword() then exit;

      chatDb := ChatDbConnectionPicker.GetChatDb();
      chatDb.Connect();

      username := commandlineparser.username;
      try
        if (Command = CommandAddUser) then
        begin
          Writeln('Введите пароль нового пользователя (во время ввода не будут отображатся символы):');
          password := TCommandLineParser.GetPasswordHide();
          Writeln('Повторите ввод пароля:');

          if password <> TCommandLineParser.GetPasswordHide() then
             Writeln('Пароли не совпадают')
          else
          begin
            chatDB.AddUser(username, password);
            Writeln('Пользователь успешно добавлен');
          end;
        end
        else if (Command = CommandRemoveUser) then
        begin
          CHatDb.RemoveUser(username);
          Writeln('Пользователь успешно удален');
        end
        else if (Command = CommandResetUser) then
        begin
          Writeln('Введите пароль (во время ввода не будут отображатся символы):');
          password := TCommandLineParser.GetPasswordHide();
          ChatDb.ResetPasswordUser(username, password);
          Writeln('Пароль пользователя успешно изменен');
        end;

      finally
//        FreeAndNil(usermanager);
      end;
    end // ------------------------------------------------- ---------

  except
    on E: ECommandLineIncorrectSyntax do
    begin
      Writeln(GetResStrWidthFormatted('Description'));
    end;
    on E: ECommandLineIncorrectRequest do
    begin
      Writeln(E.Message)
    end;
    on E: EIdCouldNotBindSocket do
    begin
      Writeln('Невозможно запустить сервер, поскольку адрес или порт занят. '
        + 'Проверьте свободные порты');
    end;
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
  if chatdb <> nil then FreeAndNil(chatDB);
  FreeAndNil(commandlineparser);
  Writeln('Завершение работы. Нажмите Enter для выхода');
  Readln;

  // CoUninitialize;
end;

begin
  MainProcedure();
end.
