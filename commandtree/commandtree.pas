unit commandtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ast;

type

  TCommandClickEvent = procedure(Const CommandString: UTF8String) of object;

  { TCommandTree }

  TCommandTree = class(TVirtualStringTree)
  private
    FOnCommandDoubleClick: TCommandClickEvent;
    procedure GetCommandText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  protected
    procedure DoCommandDoubleClick(const CommandString: UTF8String);
  public
    constructor Create(AOwner: TComponent); override;
    property OnCommandDoubleClick: TCommandClickEvent read FOnCommandDoubleClick write FOnCommandDoubleClick;
  end;

implementation

type
  { TStringCommandComposite }

  TStringCommandComposite = class
  private
    FTitle: UTF8String;
    FClass: TCustomStatementClass;
  public
    constructor Create(Title: UTF8String; csClass: TCustomStatementClass);
  end;

  { TStringStringComposite }

  TStringStringComposite = class
  private
    FTitle: UTF8String;
    FCommandString: UTF8String;
  public
    constructor Create(Title: UTF8String; CommandString: UTF8String);
  end;

{ TStringCommandComposite }

constructor TStringCommandComposite.Create(Title: UTF8String;
  csClass: TCustomStatementClass);
begin
  FTitle := Title;
  FClass := csClass;
end;

{ TStringStringComposite }

constructor TStringStringComposite.Create(Title: UTF8String;
  CommandString: UTF8String);
begin
  FTitle := Title;
  FCommandString := CommandString;
end;

{ TCommandTree }

procedure TCommandTree.GetCommandText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText := TStringStringComposite(GetNodeData(Node)^).FTitle;
end;

procedure TCommandTree.NodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  if (Assigned(HitInfo.HitNode)) then
    DoCommandDoubleClick(TStringStringComposite(Sender.GetNodeData(HitInfo.HitNode)^).FCommandString);
end;

procedure TCommandTree.DoCommandDoubleClick(const CommandString: UTF8String);
begin
  if (Assigned(FOnCommandDoubleClick)) and
     (CommandString <> '')
  then
    FOnCommandDoubleClick(CommandString);
end;

constructor TCommandTree.Create(AOwner: TComponent);
var
  ParentNode: PVirtualNode;
begin
  inherited Create(AOwner);

  with TreeOptions do
  begin
    AnimationOptions := [];
    AutoOptions      := [toAutoDropExpand, toAutoScrollOnExpand, toAutoDeleteMovedNodes, toAutoTristateTracking];
    MiscOptions      := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning, toEditOnDblClick];
    PaintOptions     := [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages];
    SelectionOptions := [toFullRowSelect, toRightClickSelect];
    StringOptions    := [toAutoAcceptEditChange];
  end;

  OnGetText := @GetCommandText;
  OnNodeDblClick := @NodeDblClick;
  NodeDataSize := SizeOf(Pointer);

  // Data
  ParentNode := AddChild(nil, TStringStringComposite.Create('Data', ''));
  // - Read, Browse, List Data, List Var, List Dataset, Save
  AddChild(ParentNode, TStringStringComposite.Create('Read', 'read '));
  AddChild(ParentNode, TStringStringComposite.Create('Browse', 'browse '));
  AddChild(ParentNode, TStringStringComposite.Create('List Data', 'list data '));
  AddChild(ParentNode, TStringStringComposite.Create('List Variables', 'list variables '));
  AddChild(ParentNode, TStringStringComposite.Create('List Datasets', 'list datasets '));
  AddChild(ParentNode, TStringStringComposite.Create('Save', 'save '));

  // Statistics
  ParentNode := AddChild(nil, TStringStringComposite.Create('Statistics', ''));
  // - Read, Browse, List Data, List Var, List Dataset, Save
  AddChild(ParentNode, TStringStringComposite.Create('Describe', 'describe '));
  AddChild(ParentNode, TStringStringComposite.Create('Frequency', 'freq '));
  AddChild(ParentNode, TStringStringComposite.Create('Tables', 'tables '));
  AddChild(ParentNode, TStringStringComposite.Create('Compact Tables', 'ctable '));

  // Statistics
  ParentNode := AddChild(nil, TStringStringComposite.Create('Other', ''));
  // - Read, Browse, List Data, List Var, List Dataset, Save
  AddChild(ParentNode, TStringStringComposite.Create('Save output', 'save !output'));
  AddChild(ParentNode, TStringStringComposite.Create('Show Commands', ''));
end;

end.

