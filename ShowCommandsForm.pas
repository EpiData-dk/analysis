unit ShowCommandsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls;

type
  TShowCommands = class(TForm)
    CmdTree: TTreeView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ShowCommands: TShowCommands;

implementation

{$R *.dfm}

end.
