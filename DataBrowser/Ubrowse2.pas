unit Ubrowse2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Uvectors, UEpiGrid,
  ExtCtrls, StdActns, ActnList, Grids, Menus, StdCtrls, Buttons, ImgList;

type
  TFBrowse2 = class(TForm)
    ActionList1: TActionList;
    AcBrowseNext: TAction;
    Panel1: TPanel;
    Gridpop: TPopupMenu;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    ImageList1: TImageList;
    AcBrowseLast: TAction;
    AcBrowsePrior: TAction;
    AcBrowsefirst: TAction;
    AcClose: TAction;
    btSwapValueLabels: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AcBrowsePriorExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AcCloseExecute(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure btSwapValueLabelsClick(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    Grid : TEpiGrid;
    procedure OnCellChange(msg: string);
  public
    { Public declarations }
  end;

  TBrowse = class(TObject)
  private
    fVarnames: TStrings;
    fBrowsing: boolean;
  protected
    //
  public
    destructor Destroy(); override;
    procedure CreateBrowse(df: TEpiDataFrame; Vectorlist :TEpiVectors; VectorID: string; Editing: boolean);
    function UpdateBrowseWindow(df: TEpiDataFrame): boolean;
    function BrowseHasFocus(): boolean;
    procedure CloseBrowse();
    function CurrentVarnames(): TStrings;
    function GetBrowserHandle(): TForm;
    property Browsing: boolean read fBrowsing write fBrowsing;
  end;

var
  OBrowse: TBrowse;

implementation

uses
  UEpiDatatypes, UMain, UCmdProcessor, UIniFile, AnsDatatypes, UTranslation;

{$R *.DFM}
var
  FBrowse2: TFBrowse2;

destructor TBrowse.Destroy();
begin
  if Assigned(fVarNames) then FreeAndNil(fVarNames);
end;

function TBrowse.CurrentVarnames(): TStrings;
begin
  result := fVarnames;
end;

function TBrowse.GetBrowserHandle(): TForm;
begin
  result := FBrowse2;
end;

function TBrowse.UpdateBrowseWindow(df: TEpiDataFrame): boolean;
begin
  result := false;
  if not assigned(FBrowse2) then
    exit;
  df.Vectors.GetVectorNames(fVarnames);
  FBrowse2.Grid.DataFrame := df;
  FBrowse2.Grid.Update;  //TODO: Solving abstract error ?
  result := true;
end;

procedure TBrowse.CreateBrowse(df: TEpiDataframe; Vectorlist :TEpiVectors; VectorID: string; Editing: boolean);
var
  i: integer;
begin
  CloseBrowse();
  if vectorlist = nil then
    vectorlist := df.Vectors;
  FBrowse2:= TFBrowse2.Create(application);
  Fbrowse2.Grid.Dataframe := df;
  Fbrowse2.Grid.ReadOnly := not Editing;
  FBrowse2.Grid.ID := VectorID;
  for i:= 0 to FBrowse2.Grid.DataFrame.vectorcount-1 do
  begin
    if Vectorlist.FindVector(fbrowse2.Grid.DataFrame.Vectors[i].Name)<>nil then
    else
      Fbrowse2.Grid.ColWidths[i+1]:=-1;
  end;       
  if editing then
  begin
    FBrowse2.Caption := 'Edit Data';
    FBrowse2.ShowModal;
    CloseBrowse();
  end else begin
    if Assigned(fVarnames) then FreeAndNil(fVarnames);
    fVarnames := TStringList.Create();
    Vectorlist.GetVectorNames(fVarnames);
    FBrowse2.Show;
  end;
end;

procedure TBrowse.CloseBrowse();
begin
  if assigned(FBrowse2) then
    FreeAndNil(FBrowse2);
end;

function TBrowse.BrowseHasFocus(): boolean;
begin
  result := browsing;
end;

procedure TFBrowse2.FormCreate(Sender: TObject);
const
  def: TFormDefaults = (Section: 'Browse';
                        Top: 60; Left: 580;
                        Width: 500; Height: 600;
                        Maximize: false);
var
  opt: TEpiOption;
begin
  Grid := TEpiGrid.create(self);
  grid.OnCellChange := OnCellChange;
  Grid.parent := self;
  grid.align := alClient;
  grid.Options := grid.Options + [goColsizing,goRowsizing,goEditing];
  grid.PopupMenu := Gridpop;
  grid.GridLineWidth:=1;
  grid.Fixedcolor:=clBtnface;
  grid.color:=clInfoBk;
  dm.GetOptionValue('WINDOW FONT SIZE', opt);
  grid.Canvas.Font.Size := StrToInt(opt.Value);
  grid.Font.Size := StrToInt(opt.Value);
  OIniFile.LoadForm(self, def);
  btSwapValueLabels.Caption := OTranslator.Translate(4002, '&Show Values')
end;

procedure TFBrowse2.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TFBrowse2.FormShow(Sender: TObject);
begin
  grid.SetFocus;
end;

procedure TFBrowse2.AcBrowsePriorExecute(Sender: TObject);
begin
  if sender=AcBrowsefirst then
     grid.Row :=1
  else if sender=AcBrowseLast then
     grid.Row :=(grid.rowcount-1)
  else if sender=AcBrowseNext then
  begin
    if grid.row <(grid.rowcount-1) then
      grid.Row := grid.row +1
  end else
  if sender=AcBrowsePrior then
  begin
    if grid.row > 1 then
      grid.Row := grid.row -1
  end
end;

procedure TFBrowse2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OInifile.SaveCurrentForm(Self, 'Browse');
  FBrowse2.OnDeactivate := nil;  //TODO: Solving abstract error ?
  Grid.Dataframe := nil;
  //AcCloseExecute(Self);
end;

procedure TFBrowse2.AcCloseExecute(Sender: TObject);
// var dat, dec : char;
begin
  //OInifile.SaveCurrentForm(Self, 'Browse');
  FBrowse2.OnDeactivate := nil;
  close();
end;

procedure TFBrowse2.OnCellChange(msg: string);
begin
  aMainForm.CmdEdit.History.Add(msg);
  aMainForm.CmdEdit.CurrentCmd := aMainForm.CmdEdit.History.Count -1;
  dm.PrintCommand(msg);
end;

procedure TFBrowse2.FormDeactivate(Sender: TObject);
var
  opt: TEpiOption;
begin
  OBrowse.Browsing := false;
  if (dm.GetOptionValue('DISPLAY DATABROWSER', opt) and (AnsiUpperCase(opt.Value) = 'OFF')) and
     (Grid.ReadOnly) then
    OBrowse.CloseBrowse();
end;



procedure TFBrowse2.btSwapValueLabelsClick(Sender: TObject);
begin
  Grid.UseLabels := not Grid.UseLabels;
  if (pos('Labels', btSwapValueLabels.Caption)>0) then
    btSwapValueLabels.Caption := OTranslator.Translate(4002, '&Show Values')
  else
    btSwapValueLabels.Caption := OTranslator.Translate(4003, '&Show Labels');
  GRid.Repaint;
end;

end.
