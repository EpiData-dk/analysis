unit varnames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TVarnamesForm }

  TVarnamesForm = class(TForm)
    ListBox1: TListBox;
  private

  public

  end;

var
  VarnamesForm: TVarnamesForm;

implementation

{$R *.lfm}

end.

