unit authenticationForm;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  vcl.StdCtrls,
  Vcl.ExtCtrls,
  IdTCPConnection,
  IdException,
  IdStack,
  IdTCPClient,
  IdBaseComponent,
  IdComponent,
  IdSSLOpenSSL,
  IdSSlOpenSSLHeaders,
  IdSSL,
  System.JSON,
  clientmanager;

type
  TFormLogin = class(TForm)
    GridPanel1: TGridPanel;
    btnOk: TButton;
    btnCancel: TButton;
//    TCPClient1: TIdTCPClient;
    TimerWaitResponse: TTimer;
    GroupBoxUser: TGroupBox;
    GridPanel2: TGridPanel;
    LabelLogin: TLabel;
    EditUsername: TEdit;
    LabelPassword: TLabel;
    EditPassword: TEdit;
    GroupBoxConnect: TGroupBox;
    GroupBoxCert: TGroupBox;
    GridPanel3: TGridPanel;
    LabelServer: TLabel;
    EditServer: TEdit;
    LabelPort: TLabel;
    EditPort: TEdit;
    EditHashCert: TEdit;
    procedure FormResize(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerWaitResponseTimer(Sender: TObject);
    procedure EditPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fOldCursor: TCursor;
    fNowWaiting: boolean;
   // fTcpManager :TTcpClientManager1;
    FTcpClientManager: TClientManager;
   // clientThread :TTcpClientThread;
    fSuccessLogin: boolean;

  public


//    property prTCPClient: TIdTCPClient write TCPClient1;
    property TcpClientManager :TClientManager write FTcpClientManager;

    property NowWaiting :boolean read fNowWaiting write fNowWaiting;
    property OldCursor :TCursor read fOldCursor write fOldCursor;
    property SuccesLogin :boolean read fSuccessLogin write fSuccessLogin;

    procedure StartWaitResponse();
    procedure StopWaitResponse();
  end;

var
  FormLogin: TFormLogin;

implementation

uses  System.Generics.Collections, msgdialogrus, Windows;
{$R *.dfm}

procedure TFormLogin.FormCreate(Sender: TObject);
begin
  NowWaiting := false;
  inherited;
end;


procedure TFormLogin.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if NowWaiting then
  begin
    btnOk.Enabled := true;
    Screen.Cursor := OldCursor;
  end;
end;

procedure TFormLogin.btnOkClick(Sender: TObject);
var
  port :Cardinal;
begin
  port := 0;
  if (not TryStrToUInt(EditPort.Text, port)) or (port > 65535) then
  begin
     MessageDlgRus('Некорректно указан порт', 'Ошибка', mtError, [mbOk]);
     EditPort.SetFocus();
  end;

  StartWaitResponse();
  try
    FTcpClientManager.Connect(EditServer.Text, port, EditHashCert.Text);
    FTcpClientManager.Login(EditUsername.Text, EditPassword.Text);
    FTcpClientManager.Start();

  except
    on e:EIdConnClosedGracefully do
    begin
      MessageDlgRus('Не удалось получить ответ на запрос', 'Ошибка',
        mtError, [mbOk]);
      if FTcpClientManager.Connected then
      begin
        FTcpClientManager.Stop;
        FTcpClientManager.Disconnect();
      end;
    end;
    on e :EIdOSSLUnderlyingCryptoError do
    begin
      MessageDlgRus('Ошибка при проверке сертификата. Сообщение: ' + e.Message,
        'Ошибка', mtError, [mbOk]);
      StopWaitResponse();
      FTcpClientManager.Stop();
    end;
    on e:EIdSocketError do
    begin
      MessageDlgRus('Не удалось подключится - сервер недоступен', 'Ошибка',
          mtError, [mbOk]);
      FTcpClientManager.Stop();
      StopWaitResponse();
    end;
    on e:Exception do
    begin
      MessageDlgRus('Ошибка ' + e.ClassName + ': ' + e.Message, 'Ошибка',
        mtError, [mbOk]);
    end;
  end;

  //Screen.Cursor := OldCursor;

end;

procedure TFormLogin.StartWaitResponse();
begin
  NowWaiting := true;
  OldCursor := Screen.Cursor;
  btnOk.Enabled := false;
  Screen.Cursor := crHourGlass;
  TimerWaitResponse.Enabled := true;
end;

procedure TFormLogin.StopWaitResponse();
begin
  Screen.Cursor := OldCursor;
  btnOk.Enabled := true;
  TimerWaitResponse.Enabled := false;
end;

procedure TFormLogin.TimerWaitResponseTimer(Sender: TObject);
begin
  StopWaitResponse();
  if FTcpClientManager.Connected then
  begin
    FTcpClientManager.Stop();
    FTcpClientManager.Disconnect();
  end;
  MessageDlgRus('Время ожидания ответа вышло. Проверьте ' +
        'работоспособность сервера и повторите попытку позже', 'Внимание',
        mtError, [mbOk]);
end;


procedure TFormLogin.EditPasswordKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) or (Key = 10) then
    btnOkClick(Sender);
end;

procedure TFormLogin.btnCancelClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TFormLogin.FormResize(Sender: TObject);
begin
//  if self.Width < 714 then
//    self.Width := 714;
//  if self.Height < 518 then
//    self.Height := 518;
end;

end.
