unit UChartArray;

interface

uses
  Classes, Chart;

type
  TChartArray = class
  private
    fList: TStringList;
    function GetCount(): Integer; 
    function GetChart(const index: integer): TChart;
    procedure SetChart(const index: integer; Chart: TChart);
    function GetFormTitle(const index: integer): string;
    procedure SetFormTitle(const index: integer; const FormTitle: string);
  public
    Constructor Create();
    Destructor Destroy(); override;
    procedure Add(Chart: TChart; FormTitle: string = 'Chart');
    property Count: Integer read GetCount;
    property Chart[const Index: integer]: TChart read GetChart write SetChart; default;
    property FormTitle[const Index: integer]: string read GetFormTitle write SetFormTitle;
  end;

implementation

uses
  SysUtils;
  
// ============================================================================
// Public methodes.
// ============================================================================

constructor TChartArray.Create();
begin
  fList := TStringList.Create;
end;

destructor TChartArray.Destroy();
begin
  if Assigned(fList) then FreeAndNil(fList);
end;

procedure TChartArray.Add(Chart: TChart; FormTitle: string = 'Chart');
begin
  fList.AddObject(FormTitle, Chart);
end;

// ============================================================================
// Private/Protected methodes.
// ============================================================================

function TChartArray.GetCount(): Integer;
begin
  result := fList.Count;
end;

function TChartArray.GetChart(const index: integer): TChart;
begin
  result := TChart(fList.Objects[index])
end;

procedure TChartArray.SetChart(const index: integer; Chart: TChart);
begin
  fList.Objects[index] := Chart;
end;

function TChartArray.GetFormTitle(const index: integer): string;
begin
  result := fList.Strings[index];
end;

procedure TChartArray.SetFormTitle(const index: integer; const FormTitle: string);
begin
  fList.Strings[index] := FormTitle;
end;

end.
