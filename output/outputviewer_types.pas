unit outputviewer_types;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, outputgenerator_base, executor;

type

  { IAnaOutputViewer }

  IAnaOutputViewer = interface ['IAnaOutputViewer']
    procedure Initialize;
    procedure LoadFromStream(ST: TStream);
    procedure UpdateFontAndSize(AExecutor: TExecutor);
    function GetOutputGeneratorClass: TOutputGeneratorClass;

  // Methods for help context system
    function GetLineAtCaret: String;
    function GetSelectedText: String;
    function GetCaretPos: TPoint;
    function IsFocused: Boolean;

  end;

implementation

end.

