unit uabout;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    OKButton: TButton;
    ProductName: TLabel;
    Version: TLabel;
    Bevel1: TBevel;
    Description: TLabel;
    Authors: TLabel;
    authors1: TLabel;
    authors2: TLabel;
    authors3: TLabel;
    Authors4: TLabel;
    Label1: TLabel;
    Copyright: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//function showabout(nonmodal:boolean=false;const pDescription:string=''):boolean;
function showabout(nonmodal:boolean=false;const pDescription:string='';
 const ACompany:string=''; const cAuthors:string='';const cAuthors1:string='';
 const cAuthors2:string=''; const cAuthors3:string='';
 const cAuthors4:string=''; const ACopyright:string=''):boolean;
function Freeabout:boolean;
procedure GetBuildInfo(var v1, v2, v3, v4: word);
function GetBuildInfoAsString:string;

implementation

{$R *.DFM}

//uses SMMemDx;

var
 AboutBox: TAboutBox;

procedure GetBuildInfo(var V1, V2, V3, V4: Word);
 var
   VerInfoSize:  DWORD;
   VerInfo:      Pointer;
   VerValueSize: DWORD;
   VerValue:     PVSFixedFileInfo;
   Dummy:        DWORD;
begin
 VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
 GetMem(VerInfo, VerInfoSize);
 GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
 VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
 with VerValue^ do
   begin
   V1 := dwFileVersionMS shr 16;
   V2 := dwFileVersionMS and $FFFF;
   V3 := dwFileVersionLS shr 16;
   V4 := dwFileVersionLS and $FFFF;
   end;
 FreeMem(VerInfo, VerInfoSize);
end;

function GetBuildInfoAsString:string;
var
  w1,w2,w3,w4 : word;
begin
   result:='';
   GetBuildInfo(w1,w2,w3,w4);
//   Result:=format('EpiData Analysis Version %d.%d Release %d (Build %d)',[w1,w2,w3,w4]);
   Result:=format('EpiData Analysis V%d.%d.%d.%d ',[w1,w2,w3,w4]);
end;

function GetMemoryInfo:string;
var
ms :TMemoryStatus;
begin
ms.dwLength :=sizeof(TMemoryStatus);
GlobalMemoryStatus(ms);
result:=format('%d %% of memory is used'+#13#10+ 'Physical memory %d MB'+
#13#10+ 'Free physical memory %d MB'+#13#10+ 'Total Virtual memory %d MB'+
#13#10+ 'Free Virtual memory %d MB',
[ms.dwMemoryLoad,ms.dwTotalPhys div 1048576,ms.dwAvailPhys div 1048576,
ms.dwTotalPageFile div 1048576,ms.dwAvailPageFile div 1048576]);
{free physical memory bytes
bytes of paging file
free bytes of paging file
user bytes of address space
free user bytes
    dwLength;        //
    dwMemoryLoad;    //
    dwTotalPhys;     //
    dwAvailPhys;     //
    dwTotalPageFile; //
    dwAvailPageFile; //
    dwTotalVirtual;  //
    dwAvailVirtual;  //
}
end;

function Freeabout:boolean;
begin
 if assigned(AboutBox) then
    AboutBox.free;
end;

function showabout(nonmodal:boolean=false;const pDescription:string='';
 const ACompany:string=''; const cAuthors:string='';const cAuthors1:string='';
 const cAuthors2:string='';
 const cAuthors3:string='';const cAuthors4:string=''; const ACopyright:string=''):boolean;
//var
//  w1,w2,w3,w4 : word;
begin
   result:=true;
   AboutBox:= TAboutBox.Create(nil);
   try
   with AboutBox do
   begin
    ProductName.caption:=application.title;
    Authors.Caption := cAuthors;
   Authors1.Caption := cAuthors1;
   Authors2.Caption := cAuthors2;
   Authors3.Caption := cAuthors3;
   Authors4.Caption := cAuthors4;
   Label1.caption := 'For updates, funding and further credits, see: ';
    aboutbox.caption:=  GetBuildInfoAsstring;
//    GetBuildInfo(w1,w2,w3,w4);
//    aboutbox.caption:=format('Version %d.%d (Build %d)',[w1,w2,w3,w4]);
//    company.caption:=Acompany;
    Copyright.caption:=Acopyright;
    Description.Caption:=pDescription;
    if nonmodal then
    begin
       show;
       update;
       sleep(100)
    end
    else
      showmodal;
   end;
   finally
     if not nonmodal then
        AboutBox.Free;
   end;
end;

end.

