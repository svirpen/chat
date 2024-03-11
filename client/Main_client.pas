unit Main_client;

interface

uses
  System.SysUtils,
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
  IdIOHandlerStack,
  IdException,
  IdStack,
  IdSSL,
  IdSSLOpenSSL,
  IdServerIOHandler,
  IdGlobal,
  CommCtrl,
  Data.DB,
  Data.Win.ADODB,
  clientmanager,
  authenticationForm;

type
  TMainForm = class(TForm)
    GridPanel1: TGridPanel;
    GridPanel2: TGridPanel;
    EditMessage: TEdit;
    btnSend: TButton;
    FAdoConnection1: TADOConnection;
    TCPClient1: TIdTCPClient;
    Panel1: TPanel;
    Splitter1: TSplitter;
    GroupBoxMessages: TGroupBox;
    MemoHistory: TMemo;
    ListViewParticipants: TListView;
    procedure btnSendClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditMessageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    SuccessLogin :boolean;
    TcpClientManager :TClientManager;
    procedure ProcessLoginAccept(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
    procedure ProcessLoginReject(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
    procedure ProcessConnectInformation(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
    procedure ProcessHistoryMesg(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
    procedure ProcessChatMesg(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
    procedure ProcessParticipantsInfo(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
    procedure ProcessLeaveParticipant(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string = '');
  public
    property ADOConnection1: TADOConnection read FAdoConnection1 write FAdoConnection1;
  end;

var
  MainForm: TMainForm;

implementation

uses msgdialogrus, daux, System.JSON;
{$R *.dfm}


procedure TMainForm.FormActivate(Sender: TObject);
begin

  try
    TcpClientManager := TClientManager.Create(TCPClient1);
    TcpClientManager.EventLoginAccept := self.ProcessLoginAccept;
    TcpClientManager.EventLoginReject := self.ProcessLoginReject;
    TcpClientManager.EventConnectInformation := self.ProcessConnectInformation;
    TcpClientManager.EventHistoryMesg := self.ProcessHistoryMesg;
    TcpClientManager.EventChatMesg := self.ProcessChatMesg;
    TcpClientManager.EventParticipantsInfo := self.ProcessParticipantsInfo;
    TcpClientManager.EventLeaveParticipant := self.ProcessLeaveParticipant;
    FormLogin.TcpClientManager := TCPClientManager;

    FormLogin.ShowModal();
    if not FormLogin.SuccesLogin then
      Self.Close();
    SuccessLogin := FormLogin.SuccesLogin;

  except
    on e:EIdSocketError  do
    begin
//    Будем избегать MessageBox из WinApi  - в будущем может стать нежелательным
//    из-за кроссплатформенной совместимости
      MessageDlgRus('Не удалось подключиться - сервер не доступен', 'Ошибка', mtError, [mbOk]);
    end;
    on e:Exception do
    begin
      MessageDlgRus('Ошибка ' + e.ClassName + ': ' + e.Message, 'Ошибка',
        mtError, [mbOk]);
    end;
  end;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if TCPClient1.Connected then
    TcpClientManager.Stop;
  FreeAndNil(TcpClientManager);

end;

procedure TMainForm.btnSendClick(Sender: TObject);
begin
  TcpClientManager.SendChatMessage(EditMessage.Text);
  EditMessage.Clear();
end;

procedure TMainForm.EditMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) or (Key = 10) then
    BtnSendClick(Sender);
end;

procedure TMainForm.ProcessLoginAccept(MesgType :integer; const Sender :string;
      const TimeStr :string; const Text:string);
begin
  FormLogin.StopWaitResponse();
  FormLogin.SuccesLogin := true;
  FormLogin.Close();
end;

procedure TMainForm.ProcessLoginReject(MesgType :integer; const Sender :string;
  const TimeStr :string; const Text:string);
begin
  FormLogin.StopWaitResponse();
  MessageDlgRus('Аутентификация не пройдена: проверьте имя пользователя и пароль',
      'Ошибка', mtError, [mbOK]);
  if TcpClientManager.Connected then
  begin
    TcpClientMAnager.Stop();
    TcpClientManager.Disconnect();
  end;
end;

procedure TMainForm.ProcessConnectInformation(MesgType :integer; const Sender :string;
  const TimeStr :string; const Text:string);
var item :TListItem;
begin
 MemoHistory.Lines.Add(Format('%s: Пользователь %s подключен к чату',[
      TimeStr, Sender]));
  Item := ListViewParticipants.Items.Add();
  Item.Caption := Sender;
end;

procedure TMainForm.ProcessHistoryMesg(MesgType :integer; const Sender :string;
  const TimeStr :string; const Text:string = '');
begin
  MemoHistory.Lines.Text := Format('%s: %s: %s', [TimeStr, Sender, Text]) +
    #13#10 + MemoHistory.Lines.Text;
end;

procedure TMainForm.ProcessChatMesg(MesgType :integer; const Sender :string;
  const TimeStr :string; const Text:string = '');
begin
  MemoHistory.Lines.Add(Format('%s: %s: %s', [TimeStr, Sender, Text]));
end;

procedure TMainForm.ProcessParticipantsInfo(MesgType :integer; const Sender :string;
  const TimeStr :string; const Text:string);
var item :TListItem;
begin
  Item := ListViewParticipants.Items.Add();
  Item.Caption := Sender;
end;

procedure TMainForm.ProcessLeaveParticipant(MesgType :integer; const Sender :string;
  const TimeStr :string; const Text:string);
var item :TListItem;
begin
  Item := ListViewParticipants.FindCaption(0, Sender, false, true, false);
  if Assigned(Item) then
    ListViewParticipants.Items.Delete(Item.Index);
  MemoHistory.Lines.Add(Format('Пользователь %s покинул чат', [Sender]));
end;

end.
