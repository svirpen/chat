unit msgdialogrus;

interface
uses  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  vcl.StdCtrls,
  Vcl.ExtCtrls;

//  Будем избегать MessageBox из WinApi  - в будущем может стать нежелательным
//  из-за кроссплатформенной совместимости
function MessageDlgRus(const MessageStr: string; FormCaption: string;
    DlgType: TmsgDlgType; buttons: TMsgDlgButtons): Integer;

implementation

function MessageDlgRus(const MessageStr: string; FormCaption: string;
    DlgType: TmsgDlgType; buttons: TMsgDlgButtons): Integer;
var
  Dialog: TForm;
  i: Integer;
  Button: Tbutton;
begin
  Dialog := CreateMessageDialog(MessageStr, DlgType, Buttons);
  Dialog.Caption := FormCaption;
  Dialog.FormStyle := fsStayOnTop;
  for i := 0 to Dialog.ComponentCount - 1 Do
  begin
    if (Dialog.Components[i] is Tbutton) then
    begin
      Button := TButton(Dialog.Components[i]);
      if Button.Caption = 'Cancel' then Button.Caption := 'Отмена'
      else if Button.Caption = 'Yes' then Button.Caption := 'Да'
      else if Button.Caption = 'No' then Button.Caption := 'Нет';
    end;
  end;
  Result := Dialog.ShowModal();
end;

end.
