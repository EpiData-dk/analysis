unit recode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, executor, outputcreator;

type

  { TRecode }

  TRecode = class(TObject)
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecRecode(ST: TRecodeCommand);
  end;

implementation

{ TRecode }

constructor TRecode.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator
  );
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TRecode.Destroy;
begin
  inherited Destroy;
end;

procedure TRecode.ExecRecode(ST: TRecodeCommand);
begin
  ST.ExecResult := csrSuccess;
end;

end.

