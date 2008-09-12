unit UCommands;

interface
uses classes,UVariables, UEpidataTypes, prEXpr;

Type

  TCommandList =  class;

  TCommand =  class(TExpression)
  private
    fCommandID :word;
    FParameterList: TVarList;
    fOwner: TCommandList;
    fOutput: Boolean;
    function GetParam(n: Integer): IValue;
    function GetParameterList: TVarList;
    function GetParamByname(const ParamName: string): IValue;
    function GetParamExists(const ParamName: string): Boolean;
    function GetNoEcho: Boolean;
  protected
    //
  public
    constructor Create(pCommandID : Word; aParameterList: TVarList;aOwner:TCommandList=nil);
    destructor Destroy; override;
    function ParameterCount: Integer;
    property ParameterList: TVarList read GetParameterList;
    property Param[n: Integer]: IValue read GetParam;
    property ParamExists[const ParamName: string]: Boolean read GetParamExists;
    property ParamByName[const ParamName: string]: IValue read GetParamByname;
    property CommandID : Word read fCommandID write fCommandID;
    property Owner :TCommandList read fOwner write fOwner;
    property NoEcho: Boolean read GetNoEcho;
    property Output: Boolean read fOutput write fOutput;
  end;


TCommandList =  class
private
    fOwner: TObject;
    function GetCount: integer;
    function GetItem(Index: integer): TCommand;
    procedure SetItem(Index: integer; const Value: TCommand);
protected
  List : TList;
public
 constructor Create(pOwner: TObject);
 Destructor Destroy; override;
 function add(CommandID : Word;PList : TVarList):TCommand;overload;
 function add(Cmd : TCommand):TCommand;overload;
 function Find(const CmdId:string):TCommand;
 procedure Clear;
 Property Items[Index:integer]:TCommand read GetItem write SetItem;default;
 property Count: integer read GetCount;
 property Owner: TObject read fOwner;
end;


TCommandBlock=class(TInterfacedObject)
private
    fCommandLines: TStringList;
    fCommandID: string;
    fHint: string;
    foutputCommands: TCommandList;
    fInputCommands: TCommandList;
    fRouted: boolean;
    fOutputFileName: string;
    fRouteMessage: string;
    fNOCLS: boolean;
public
  constructor Create(const pCommandID, pCommandLines,pHint: string);
  Destructor Destroy;override;
  function AddCmdLine(const cmdline:string):boolean;
  property CommandID : string read fCommandID write fCommandID;
  property CommandLines: TStringList read fCommandLines;
  property InputCommands: TCommandList read  fInputCommands write fInputCommands;
  property outputCommands: TCommandList read  foutputCommands write foutputCommands;
  property Hint : string read fHint write fhint;
  property OutputFileName :string read fOutputFileName write fOutputFileName;
  property RouteMessage :string read fRouteMessage write fRouteMessage;
  property Routed       :boolean read fRouted write fRouted;
  property NOCLS       :boolean read fNOCLS write fNOCLS;
end;

TCommandBlockList=class
  private
    function GetItem(Index: integer): TCommandBlock;
    procedure SetItem(Index: integer; const Value: TCommandBlock);
    function GetCount: integer;
protected
  List : TStringList;
public
 constructor Create;
 Destructor Destroy; override;
 function add(const CmdId:string; const CommandLines:string='';const Hint:string=''):TCommandBlock;overload;
 function add(const CmdId: string; cmdblk: TCommandBlock): TCommandBlock;overload;
 function Find(const CmdId:string):TCommandBlock;
 procedure Clear;
 Property Items[Index:integer]:TCommandBlock read GetItem write SetItem;default;
 property Count: integer read GetCount;
end;




implementation


{ TCommandList }

function TCommandList.add(CommandID: Word; PList: TVarList): TCommand;
begin
   result:=TCommand.Create(CommandID,pList,self);
//   result.CommandID:= CommandID;
   add(result);
end;

function TCommandList.add(Cmd: TCommand): TCommand;
begin
   Assert(cmd<>nil, 'Commadn can''t be nil');
   cmd.Owner :=self;
   result:=cmd;
   list.Add(Pointer(result));
   result._AddRef
end;

procedure TCommandList.Clear;
var
 i :integer;
begin
 for i:= 0 to list.count-1 do
   Tcommand(list[i])._release;
 list.clear;
end;

constructor TCommandList.Create;
begin
 inherited Create;
 List := TList.Create;
 fOwner:=pOwner;
end;

destructor TCommandList.Destroy;
begin
 clear;
 list.free;
 inherited Destroy;
end;

function TCommandList.Find(const CmdId: string): TCommand;
begin

end;

function TCommandList.GetCount: integer;
begin
  result:=List.count;
end;

function TCommandList.GetItem(Index: integer): TCommand;
begin
   result:=TCommand(list[Index]);
end;

procedure TCommandList.SetItem(Index: integer; const Value: TCommand);
begin
   list[Index]:=Tobject(value);
end;



function TCommand.GetParam(n: Integer): IValue;
begin
  assert(FParameterList<>nil,'Parameter list is nil');
  Result:= IValue(ParameterList[n]);
end;

function TCommand.ParameterCount: Integer;
begin
  if Assigned(FParameterList) then
    ParameterCount:= FParameterList.Count
  else
    ParameterCount:= 0
end;

constructor TCommand.Create;
begin
  inherited Create;
  FParameterList:= aParameterList;
  fCommandID:=pCommandID;
  fOwner := Owner;
end;

destructor TCommand.Destroy;
begin
  if  FParameterList <> nil then
    FParameterList.Free;
  inherited Destroy
end;

function TCommand.GetParameterList: TVarList;
begin
// assert(FParameterList<>nil,'Parameter list is nil');
 result:=FParameterList;
end;

function TCommand.GetParamByname(const ParamName: string): IValue;
begin
 assert(FParameterList<>nil,'Parameter list is nil');
 result:=ParameterList.VarbyName[ParamName];
end;

function TCommand.GetParamExists(const ParamName: string): Boolean;
begin
  assert(FParameterList<>nil,'Parameter list is nil');
  result := ParameterList.VarExists[ParamName];
end;

function TCommand.GetNoEcho: Boolean;
begin
 result:=false;
 if FParameterList=nil then exit;
 Result := GetParamByname('NOECHO')<>nil;
end;


{ TCommandBlock }

function TCommandBlock.AddCmdLine(const cmdline: string): boolean;
begin
   fCommandLines.add(cmdline);
end;

constructor TCommandBlock.Create(const pCommandID, pCommandLines,pHint: string);
begin
  inherited create;
  foutputCommands:= TCommandList.Create(self) ;
  fInputCommands:= TCommandList.Create(self);
  fCommandId :=pCommandID;
  fCommandLines:= TStringList.create;
  fCommandLines.text :=pCommandLines;
  fHint := pHint;
end;


destructor TCommandBlock.Destroy;
begin
  foutputCommands.free;
  fInputCommands.free;
  fCommandLines.free;
  inherited;
end;

{ TCommandBlockList }

function TCommandBlockList.add(const CmdId: string; const CommandLines, Hint: string): TCommandBlock;
begin
try
   result:=TCommandBlock.Create(CmdID,CommandLines,Hint);
   Add(CmdID, Result);
//   list.AddObject(CmdID,Tobject(Result));
except
   result.free;
   raise;
end;
end;

function TCommandBlockList.add(const CmdId: string; cmdblk: TCommandBlock): TCommandBlock;
begin
  if List.IndexOf(CmdID) > -1 then
    list.Objects[List.IndexOf(CmdID)] := cmdblk
  else
    list.AddObject(CmdID,Tobject(cmdblk));
end;

procedure TCommandBlockList.Clear;
var
 i :integer;
begin
 for i:= 0 to list.count-1 do
  TcommandBlock(list.objects[i]).free;
 list.clear;
end;

constructor TCommandBlockList.Create;
begin
 inherited Create;
 List := TStringList.Create;
 List.Sorted :=true;
 List.Duplicates:= dupIgnore;
end;

destructor TCommandBlockList.Destroy;
begin
 clear;
 list.free;
 inherited Destroy;
end;

function TCommandBlockList.Find(const CmdId: string): TCommandBlock;
var
  idx :integer;
begin
 result:= nil;
 if list.Find(CmdId, idx) then
   result:= TCommandBlock(list.objects[idx]);
end;

function TCommandBlockList.GetCount: integer;
begin
 result:=List.count;
end;

function TCommandBlockList.GetItem(Index: integer): TCommandBlock;
begin
  result:=TCommandBlock(list.Objects[Index]);
end;

procedure TCommandBlockList.SetItem(Index: integer;
  const Value: TCommandBlock);
begin
   list.Objects[Index]:=Tobject(value);
end;



end.
