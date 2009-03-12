unit UBaseChart;

interface

uses
  Classes, Chart, UCommands, UChartArray;


type
  TStdChartFunction = function(): TChart of object;
  TChartArrayFunction = function(): TChartArray of object;

  TCustomChartClass = class of TCustomChart;

  TCustomChart = class(TObject)
  private
    fOnCreateStdChart: TStdChartFunction;
    fOnCreateChartArray: TChartArrayFunction;
    function GetCmd(): TCommand;
  protected
    function CreateStandardChart: TChart;
    function CreateChartArray: TChartArray;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Cmd: TCommand read GetCmd;
    property OnCreateStandardChart: TStdChartFunction read fOnCreateStdChart write fOnCreateStdChart;
    property OnCreateChartArray: TChartArrayFunction read fOnCreateChartArray write fOnCreateChartArray;
  end;


implementation

uses
  UDebug, UGraph;

const
  UnitName = 'UBaseChart';


constructor TCustomChart.Create();
const
  ProcName = 'Create';
  Version = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, ProcName, Version, 3);
  try
    inherited Create();
  finally
    ODebug.DecIndent();
  end;
end;

destructor TCustomChart.Destroy();
const
  ProcName = 'Destroy';
  Version = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, ProcName, Version, 3);
  try
    inherited Destroy();
  finally
    ODebug.DecIndent();
  end;
end;

function TCustomChart.GetCmd(): TCommand;
const
  ProcName = 'GetCmd';
  Version = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, ProcName, Version, 5);
  try
    result := OGraph.Cmd;
  finally
    ODebug.DecIndent();
  end;
end;

function TCustomChart.CreateStandardChart(): TChart;
const
  ProcName = 'GetCmd';
  Version = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, ProcName, Version, 5);
  try
    result := nil;
    if Assigned(fOnCreateStdChart) then
      result := fOnCreateStdChart();
  finally
    ODebug.DecIndent();
  end;
end;

function TCustomChart.CreateChartArray(): TChartArray;
const
  ProcName = 'CreateChartArray';
  Version = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, ProcName, Version, 5);
  try
    result := nil;
    if Assigned(fOnCreateChartArray) then
      result := fOnCreateChartArray();
  finally
    ODebug.DecIndent();
  end;
end;

end.
