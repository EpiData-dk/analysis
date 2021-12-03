unit ana_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages;

const
  LM_MAIN_OPENRECENT   = LM_USER + 1;   // WParam: 0 = use LParam; >0 = Pointer to a TAction which initiated the call

implementation

end.

