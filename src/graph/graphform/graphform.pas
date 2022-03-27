unit graphform;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, chartfactory, graphcommandresult, Forms;

type
  IGraphForm = interface['{9E9A5CBB-717A-49DC-8D31-EB00018EA376}']
    procedure SetCommandResult(ACommandResult: IGraphCommandResult);
    function GetForm: TCustomForm;
  end;

implementation

end.

