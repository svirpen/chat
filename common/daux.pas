unit daux;

interface
uses System.SysUtils, System.SyncObjs, System.Classes, System.Types,
   Windows;




type TDebugger = class
private
  DebugMutex: TMutex;
public
  constructor Create();
  destructor Destroy(); override;
  procedure WriteToFile(const mesg: string; filename: string = 'DebugOut.txt');
  procedure WriteToFileLn(const mesg: string; filename: string = 'DebugOut.txt');

end;

function ReadKey(): Char;
function GetResStrWidthFormatted(const ResourceName: string;
    const TerminalWidth: integer = 80): string;

//var Debugger: TDebugger;

implementation

uses System.Generics.Collections;

function ReadKey(): Char;
var
  StdIn: THandle;
  InputRec: TInputRecord;
  NumRead: Cardinal;
  KeyMode: DWORD;
begin
  begin
    StdIn :=  GetStdHandle(STD_INPUT_HANDLE);
//    Result := #$FF;
    GetConsoleMode(StdIn, KeyMode);
    while true do
    begin
      ReadConsoleInput(StdIn, InputRec, 1, NumRead);
      if (InputRec.EventType and KEY_EVENT <> 0) and
         InputRec.Event.KeyEvent.bKeyDown then
      begin
        if InputRec.Event.KeyEvent.AsciiChar <> #0 then
        begin
          Result := char(InputRec.Event.KeyEvent.AsciiChar);
          Break;
        end;
      end;
    end;
    SetConsoleMode(StdIn, KeyMode);
  end;
end;

/// <summary> Функция получения текста из ресурса с форматированием ширины
/// вывода (по умолчанию 80 символов)
/// </summary>   
function GetResStrWidthFormatted(const ResourceName: string;
     const TerminalWidth: integer): string;  // форматирование по ширине окна консоли
const crlf = #13#10;
var StrList: TStringList;
  ResStream: TResourceStream;
  i, j, k, OldPos: integer;
  PositionsToInsert: array of integer;
  s: string;
  LenCarShift: integer;
  EndLine: string;
begin
  EndLine := crlf;
  LenCarShift := length(EndLine) - 1; // сдвиг из-за замены пробельного символа
                                   // на возврат каретки

  StrList := TStringList.Create();
  ResStream := TResourceStream.Create(hInstance, ResourceName,
    System.Types.RT_RCDATA);
  try
    StrList.LoadFromStream(ResStream, TEncoding.UTF8);

    for i := 0 to StrList.Count - 1 do
    begin
      SetLength(PositionsToInsert, Length(StrList[i]) +  2);
      for j := 0 to Length(PositionsToInsert) - 1 do
        PositionsToInsert[j] := 0;

      k := 0;
      OldPos := 1;  j := OldPos + TerminalWidth - 1;
      while j < length(StrList[i]) do
      begin
        while not CharInSet(StrList[i][j], [' ', #9]) do // Находим ближайший пробельный
        begin                                            // символ
          dec(j);
          if j = OldPos then
          begin
            j := OldPos + TerminalWidth - 1;
            break;
          end;
        end;
        OldPos := j;
        PositionsToInsert[k] := OldPos + LenCarShift; // запоминаем позицию
        inc(k);                                       // ближайшего пробельного символа
        j := OldPos + TerminalWidth - 1 + LenCarShift;
      end;

      s := '';  j := 0; OldPos := 1;
      while (PositionsToInsert[j] <> 0)  do
      begin
        s := s + Copy(StrList[i], OldPos, PositionsToInsert[j] - OldPos - 1) +
           crlf;
        OldPos := PositionsToInsert[j];
        inc(j);
      end;
       s := s + Copy(StrList[i], OldPos, length(StrList[i]) - OldPos + 1);
       StrList[i] := s;
    end;
    result := StrList.Text;

  finally
    FreeAndNil(ResStream);
    FreeAndNil(StrList);
  end;
end;

{ TDebugger }

constructor TDebugger.Create;
begin
  DebugMutex := TMutex.Create();
end;

destructor TDebugger.Destroy;
begin
  FreeAndNil(DebugMutex);
  inherited;
end;

procedure TDebugger.WriteToFile(const mesg: string; filename: string);
var fileStream: TFileStream;
  oString: UTF8String;
  timestr: string;
begin
  DebugMutex.Acquire();
  if not FileExists(filename) then
  try
    filestream := TFileStream.Create(filename, fmCreate);
  finally
    fileStream.Free;
  end;

  filestream := TFileStream.Create(filename, fmOpenWrite);
  try
    filestream.Seek(0, soFromEnd);
    DateTimeToString(timestr, 'dd.mm.yyyy hh:nn:ss.zzz', now);
    oString := UTF8String(Format('[%d] %s: %s',
      [TThread.CurrentThread.ThreadID, timestr, mesg]));
    filestream.WriteBuffer(oString[1], length(oString)) ;
  finally
    FreeAndNil(FileStream);
    DebugMutex.Release();
  end;
end;

procedure TDebugger.WriteToFileLn(const mesg: string; filename: string);
begin
  WriteToFile(mesg + #13#10, filename);
end;


//initialization
//  Debugger:= TDebugger.Create();
//finalization
//  FreeAndNil(Debugger);
end.
