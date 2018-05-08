unit history_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { THistoryForm }

  THistoryForm = class(TForm)
    ListBox1: TListBox;
  private

  public

  end;

var
  HistoryForm: THistoryForm;

implementation

{$R *.lfm}

end.

