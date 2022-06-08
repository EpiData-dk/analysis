unit graphformfactory;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, graphform, Forms;

type

  { IGraphFormFactory }

  IGraphFormFactory = interface['{60911789-540E-46EF-8D5E-E047A066BD27}']
    function NewGraphForm(): IGraphForm;
    procedure CloseAllOpenForms();
    procedure CascadeWindows(LeadForm: TCustomForm);
  end;

var
  TheGraphFormFactory: IGraphFormFactory;

implementation

uses
  graphformfactory.impl;

initialization
  TheGraphFormFactory := TGraphFormFactory.Create;

end.

