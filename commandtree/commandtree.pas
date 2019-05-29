unit commandtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ast;

type

  { TCommandTree }

  TCommandTree = class(TVirtualStringTree)
  private
    procedure GetCommandText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  public
    constructor Create(AOwner: TComponent); override;

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

{ TStringCommandComposite }

constructor TStringCommandComposite.Create(Title: UTF8String;
  csClass: TCustomStatementClass);
begin
  FTitle := Title;
  FClass := csClass;
end;

{ TCommandTree }

procedure TCommandTree.GetCommandText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText := TStringCommandComposite(GetNodeData(Node)^).FTitle;
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
  NodeDataSize := SizeOf(Pointer);


  // Data
  ParentNode := AddChild(nil, TStringCommandComposite.Create('Data', nil));
  // - Read, Browse, List Data, List Var, List Dataset, Save
  AddChild(ParentNode, TStringCommandComposite.Create('Read', TReadCommand));
  AddChild(ParentNode, TStringCommandComposite.Create('Browse', TBrowseCommand));
  AddChild(ParentNode, TStringCommandComposite.Create('List Data', TListCommand));
  AddChild(ParentNode, TStringCommandComposite.Create('List Variables', TListCommand));
  AddChild(ParentNode, TStringCommandComposite.Create('List Datasets', TListCommand));
  AddChild(ParentNode, TStringCommandComposite.Create('Save', TSaveCommand));

end;

end.

