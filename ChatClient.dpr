program ChatClient;

uses
  Vcl.Forms,
  Main_client in 'client\Main_client.pas' {MainForm},
  authenticationForm in 'client\authenticationForm.pas' {FormLogin},
  daux in 'common\daux.pas',
  clientmanager in 'client\clientmanager.pas',
  msgdialogrus in 'client\msgdialogrus.pas',
  netmessagesparser in 'common\netmessagesparser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormLogin, FormLogin);
  //Application.CreateForm(TFormAuthentication, formAuthentication);
  Application.Run;
end.
