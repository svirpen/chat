unit objectthread;

interface
uses System.SysUtils,
  System.Variants,
  System.Classes;

type
  TObjectForThread = class
    constructor Create();
    procedure Terminate(Sender :TObject);
    procedure DoWork(); virtual; abstract;
    protected
      NeedStopWorkThread :boolean;
  end;


  TObjectThread = class(TThread)
    constructor Create(aObj :TObjectForThread; suspended :boolean = false);
    destructor Destroy(); override;
    procedure Execute(); override;
    private
      obj :TObjectForThread;
  end;
implementation

constructor TObjectForThread.Create();
begin
  NeedStopWorkThread := false;
end;

destructor TObjectThread.Destroy();
begin
  inherited;
end;

procedure TObjectForThread.Terminate(Sender :TObject);
begin
  NeedStopWorkThread := true;
end;

constructor TObjectThread.Create(aObj :TObjectForThread; suspended :boolean = false);
begin
  obj := aObj;
  self.OnTerminate := obj.Terminate;
  inherited Create(suspended);
end;

procedure TObjectThread.Execute();
begin

  obj.DoWork();
end;

end.
