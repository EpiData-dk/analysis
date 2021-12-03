unit select_datafile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, contnrs, epicustombase;

type

  { TSelectedDataFile }

  TSelectedDataFile = class(TEpiDataFile)
  private
    FRealDatafile: TEpiDataFile;
    FSelectionVector: TEpiField;
    FLazyDestroyedObjects: TObjectList;
  public
    constructor Create(AOwner: TEpiCustomBase; ARealDataFile: TEpiDataFile);
    destructor Destroy; override;
  end;


implementation

{ TSelectedDataFile }

constructor TSelectedDataFile.Create(AOwner: TEpiCustomBase; ARealDataFile: TEpiDataFile);
begin
  inherited Create(AOwner, 0);
  FRealDatafile := ARealDataFile;
  FLazyDestroyedObjects := TObjectList.Create(true);
end;

destructor TSelectedDataFile.Destroy;
begin
  FLazyDestroyedObjects.Free;
  FRealDatafile.Free;
  inherited Destroy;
end;

end.

