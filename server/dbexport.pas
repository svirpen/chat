unit dbexport;

interface

uses System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  ActiveX,
  Data.DB,
  Data.Win.ADODB;

type
  EEmptySelection = class(Exception)

  end;

type
  TDBExport = class
    constructor Create(afilename: string);
    destructor Destroy(); override;
    procedure DoExport(const StartTime: string; const EndTime: string);
  private
    filename: string;
    ADOConnection: TADOConnection;
    Query: TADOQuery;
  end;

implementation

{ TDBExport }

constructor TDBExport.Create(afilename: string);
begin
  CoInitialize(nil);
  ADOConnection := TADOConnection.Create(Nil);
  ADOConnection.Connected := false;
  ADOConnection.ConnectionString := 'Provider=SQLOLEDB.1;Integrated ' +
    'Security=SSPI;Persist Security Info=False;Initial Catalog=chatDb;' +
    'Data Source=IEWIN7\SQLEXPRESS';
  ADOConnection.CommandTimeout := 5;

  filename := afilename;
  inherited Create();
end;

destructor TDBExport.Destroy;
begin

  inherited;
end;

procedure TDBExport.DoExport(const StartTime: string; const EndTime: string);
var
  Writer :TStreamWriter;
  time :TDateTime;
begin

  Query := TADOQuery.Create(nil);
  Writer := TStreamWriter.Create(filename, false, TEncoding.UTF8);
  try
    Query.Connection := ADOConnection;

    ADOConnection.Connected := true;

    if ADOConnection.InTransaction then
      ADOConnection.RollbackTrans();
    ADOConnection.BeginTrans();

    Query.SQL.Text := Format('SELECT username, message, time_send ' +
      'FROM history ' + 'LEFT JOIN participants ON sender = participants.id ' +
      'WHERE (time_send > ''%s'') AND ' + '(time_send < ''%s'')',
      [StartTime, EndTime]);
    Query.Open();
    if Query.RecordCount = 0 then
      raise EEmptySelection.Create('');

    while not Query.Eof do
    begin

      time := query.FieldByName('time_send').AsDateTime;
      Writer.WriteLine(Query.FieldByName('username').AsString + ': ' +
         FormatDateTime('dd.mm.yy hh:mm:ss', time) + ': ');
      Writer.WriteLine(Query.FieldByName('message').AsString);
      Writer.WriteLine('__________________________');

      query.Next();
    end;

    ADOConnection.CommitTrans();
    Writer.Close();
  finally
    FreeAndNil(Query);
    FreeAndNil(Writer);
  end;
end;

end.
