unit SMMemDx;

{$include uglobals.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, variants;

type
  TfMemDiagnosis = class(TForm)
    tmrHeapMonitor: TTimer;
    BitBtn1: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure tmrHeapMonitorTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetActive: boolean;
    procedure setActive(const Value: boolean);
  public
    property Active:boolean read GetActive write setActive;
  end;

// function ShowMemDiagnosis:boolean;

function ShowMemDiag:string;
 procedure ReduceMemoryOverhead;

implementation

{$R *.DFM}

{function ShowMemDiagnosis:boolean;
var
  fMemDiagnosis: TfMemDiagnosis;
begin
  fMemDiagnosis:= TfMemDiagnosis.Create(application);
  try
  with fMemDiagnosis do
  begin
    active:=true;
    result:=showmodal=mrOK;
  end;
  finally
    fMemDiagnosis.free;
  end;
end;
}


procedure TfMemDiagnosis.FormCreate(Sender: TObject);

  function FindHMCommand: Boolean;
{$ifdef DelphiLessThan4}
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to ParamCount do
      if (UpperCase(ParamStr(I)) = '/HM') or
        (UpperCase(ParamStr(I)) = '-HM') then
        Result := True;
{$else}
  begin
    Result := FindCmdLineSwitch('HM', ['/', '-'], True)
{$endif}
  end;

begin
  if FindHMCommand then
  begin
    //Enable heap monitoring (via timer)
    tmrHeapMonitor.Enabled := True;
    //Make the timer tick straight away
    tmrHeapMonitor.OnTimer(tmrHeapMonitor);
  end;
end;

function CommittedMemorySize: DWord;
var
  MBI: TMemoryBasicInformation;
  SI: TSystemInfo;
  RangeStart: Pointer;
begin
  Result := 0;
  GetSystemInfo(SI);
  RangeStart := SI.lpMinimumApplicationAddress;
  while DWord(RangeStart) < DWord(SI.lpMaximumApplicationAddress) do
  begin
    VirtualQuery(RangeStart, MBI, SizeOf(MBI));
    //Only get committed memory (storage allocated for this)
    if MBI.State = MEM_COMMIT then
      Inc(Result, MBI.RegionSize);
    //Delphi 2 & 3 could only handle $7FFFFFFF as biggest int
    //Last region is likely to end at $80000000. To avoid integer
    //overflow, we'll do a comparison and bypass the addition
    if DWord(SI.lpMaximumApplicationAddress) - MBI.RegionSize >= DWord(RangeStart) then
      Inc(PChar(RangeStart), MBI.RegionSize)
    else
      //If overflow would have occurred, loop is over
      Break
  end;
end;

const
  //These come from Delphi's Source\RTL\GetMem.inc file
  cHeapOk           = 0;  // everything's fine
  cReleaseErr       = 1;  // operating system returned an error when we released
  cDecommitErr      = 2;  // operating system returned an error when we decommited
  cBadCommittedList = 3;  // list of committed blocks looks bad
  cBadFiller1       = 4;  // filler block is bad
  cBadFiller2       = 5;  // filler block is bad
  cBadFiller3       = 6;  // filler block is bad
  cBadCurAlloc      = 7;  // current allocation zone is bad
  cCantInit         = 8;  // couldn't initialize
  cBadUsedBlock     = 9;  // used block looks bad
  cBadPrevBlock     = 10; // prev block before a used block is bad
  cBadNextBlock     = 11; // next block after a used block is bad
  cBadFreeList      = 12; // free list is bad
  cBadFreeBlock     = 13; // free block is bad
  cBadBalance       = 14; // free list doesn't correspond to blocks marked free

function HeapErrorDesc(Error: Cardinal): String;
begin
  case Error of
    cHeapOk:           Result := 'Everything''s fine';
    cReleaseErr:       Result := 'OS returned an error when we released';
    cDecommitErr:      Result := 'OS returned an error when we decommited';
    cBadCommittedList: Result := 'List of committed blocks looks bad';
    cBadFiller1,
    cBadFiller2,
    cBadFiller3:       Result := 'Filler block is bad';
    cBadCurAlloc:      Result := 'Current allocation zone is bad';
    cCantInit:         Result := 'Couldn''t initialize';
    cBadUsedBlock:     Result := 'Used block looks bad';
    cBadPrevBlock:     Result := 'Prev block before a used block is bad';
    cBadNextBlock:     Result := 'Next block after a used block is bad';
    cBadFreeList:      Result := 'Free list is bad';
    cBadFreeBlock:     Result := 'Free block is bad';
    cBadBalance:       Result := 'Free list doesn''t correspond to blocks marked free';
  end
end;

function TfMemDiagnosis.GetActive: boolean;
begin
result:= tmrHeapMonitor.Enabled;
end;

procedure TfMemDiagnosis.setActive(const Value: boolean);
begin
  //Enable heap monitoring (via timer)
  if value =active then exit;
  tmrHeapMonitor.Enabled := value;
    //Make the timer tick straight away
  if value then
    tmrHeapMonitor.OnTimer(tmrHeapMonitor);
end;

function ShowMemDiag: String;
var
  HS: THeapStatus;
  d : DWOrd;
begin
  HS := GetHeapStatus;
  if HS.HeapErrorCode = cHeapOk then
  begin
    d:=CommittedMemorySize;
    result := 'Heap status=OK'
    + ' Allocated memory count='+ inttostr(AllocMemCount)
    + ' Allocated memory size='+ inttostr(AllocMemSize)
    + ' Committed memory size='+ inttostr(d);
  end
  else
    result:= Format('(Invalid heap code %d: %s)',
      [HS.HeapErrorCode, HeapErrorDesc(HS.HeapErrorCode)]);
end;

procedure TfMemDiagnosis.tmrHeapMonitorTimer(Sender: TObject);
var
  HS: THeapStatus;
  d : DWOrd;
begin
 label2.Caption :='';
 label3.Caption :='';
 label4.Caption :='';
  HS := GetHeapStatus;
  if HS.HeapErrorCode = cHeapOk then
  begin
    d:=CommittedMemorySize;
    label1.Caption := 'Heap status=OK';
    label2.Caption := 'Allocated memory count='+ inttostr(AllocMemCount);
    label3.Caption := 'Allocated memory size='+ inttostr(AllocMemSize);
    label4.Caption := 'Committed memory size='+ inttostr(d);
  end
  else
    {Application.MainForm}label1.Caption := Format('(Invalid heap code %d: %s)',
      [HS.HeapErrorCode, HeapErrorDesc(HS.HeapErrorCode)]);
end;


procedure ReduceMemoryOverhead;
begin
//Stop the RTL wanting to clear System Variants
{$ifndef DelphiLessThan4}
TVarData(EmptyParam).VType := varEmpty;
{$endif}
//These two seem to be considered constants, so we hack around
//this by de-referencing the "constant" item's address
TVarData((@Null)^).VType := varEmpty;
TVarData((@Unassigned)^).VType := varEmpty;
//Unload OLEAUT32.DLL, which will in turn unload OLE32.DLL
FreeLibrary(GetModuleHandle('OLEAUT32.DLL'));
end;

end.
