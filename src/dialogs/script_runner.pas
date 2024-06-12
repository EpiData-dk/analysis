unit script_runner;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}


interface

uses
  Classes, SysUtils;

type
  IScriptMediator = interface['{E4363977-E235-4737-87DC-D8270E7026BC}']
    procedure RunScript(Script: UTF8String);
    procedure PasteScript(Script: UTF8String);
  end;


implementation

end.

