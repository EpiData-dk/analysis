unit editor_page;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synedit;

type

  { TEditorPage }

  TEditorPage = class

  private
    FEditor: TSynEdit;
    FFileName: UTF8String;
    function GetCaption: UTF8String;
    procedure SetFileName(AValue: UTF8String);
  public
    constructor Create;
    destructor Destroy; override;
    property Editor: TSynEdit read FEditor;
    property FileName: UTF8String read FFileName write SetFileName;
    property Caption: UTF8String read GetCaption;
  end;

implementation

uses
  Controls, LazFileUtils;

{ TEditorPage }

procedure TEditorPage.SetFileName(AValue: UTF8String);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;

function TEditorPage.GetCaption: UTF8String;
begin
  if (FileName = '') then
    Result := 'Untitled'
  else
    Result := ExtractFileNameOnly(FFileName);

  if (Editor.Modified) then
    Result := Result + '*';
end;

constructor TEditorPage.Create;
begin
  FEditor := TSynEdit.Create(nil);
  FEditor.Align := alClient;

  FFileName := '';
end;

destructor TEditorPage.Destroy;
begin
  inherited Destroy;
end;

end.

