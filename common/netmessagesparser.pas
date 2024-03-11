unit netmessagesparser;

interface
uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  SyncObjs,
  System.JSON;


// type TMEssageType = mtLogin = 1, mttLoginAccept = 2
const mtLogin = 1;
const mtLoginAccept = 2;
const mtLoginReject = 3;
const mtConnectInformation = 4;
const mtChatMesgFromClient = 5;
const mtChatMesgFromServer = 6;
const mtHistoryMesg = 7;
const mtParticipantsInfo = 8;
const mtLeaveParticipant = 9;
const mtUnsupported = MaxInt;
type EInvalidFormatMessageRequest = class(Exception)

end;

type EAlreadyParsed = class (Exception)

end;

type TMesFieldsLogin = record
  Username :string;
  Password :string;
end;

type TLoginMsg = record
  MsgType: integer;
  Username: string;
  Password: string;
  constructor Create(const _MsgType: integer; const _username, _password: string);
end;

type TNetMsg = record
  MsgType: integer;
  Sender: string;
  SendingTime: string;
  Text: string;
  constructor Create(const _MesgType: integer; const _Sender, _DateTimeStr,
    _MessageText: string);
end;

type TClientMsg = record
  MsgType: integer;
  Text: string;
  constructor Create(const _MsgType: integer; const _text: string);
end;

type TParticipantIndo = record
  MsgType: integer;
  Participant: string;
  constructor Create(const _MsgType: integer; const _Participant: string);
end;

function GetMesgType(const JsonStr: string): integer;

implementation

uses daux, StrUTils;


function GetMesgType(const JsonStr: string): integer;
var                     // Тип распарсим вручную, а дальше будем использовать
  PosQ: integer;                                           // TJsonSerializer
begin
  result := mtUnsupported;
  PosQ := 0;
  if length(JsonStr) >= 12 then
    if copy(JsonStr, 1, 11) = '{"MsgType":' then
      PosQ := PosEx(',', JsonStr, 12);
  if PosQ = 0 then PosQ := PosEx('}', JsonStr, 12);
  if PosQ <> 0 then
    result := StrToIntDef(copy(JsonStr, 12, PosQ - 12), mtUnsupported);

end;

{ TNetMsg }

constructor TNetMsg.Create(const _MesgType: integer; const _Sender,
  _DateTimeStr, _MessageText: string);
begin
  MsgType := _MesgType;
  Sender := _Sender;
  SendingTime := _DateTimeStr;
  Text := _MessageText;
end;

{ TLoginMsg }

constructor TLoginMsg.Create(const _MsgType: integer; const _username,
  _password: string);
begin
  MsgType := _MsgType;
  Username := _username;
  Password := _password;
end;


{ TClientMsg }

constructor TClientMsg.Create(const _MsgType: integer; const _text: string);
begin
  MsgType := _MsgType;
  Text := _text;
end;


{ TParticipantIndo }

constructor TParticipantIndo.Create(const _MsgType: integer;
    const _Participant: string);
begin
  MsgType := _MsgType;
  Participant := _Participant;
end;


end.
