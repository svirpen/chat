unit authentication;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormAuthentication = class(TForm)
    GridPanel1: TGridPanel;
    Label2: TLabel;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formAuthentication: TFormAuthentication;

implementation

{$R *.dfm}

end.
