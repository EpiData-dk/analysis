unit projecttree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TProjectTreeForm }

  TProjectTreeForm = class(TForm)
    TreeView1: TTreeView;
  private

  public

  end;

var
  ProjectTreeForm: TProjectTreeForm;

implementation

{$R *.lfm}

end.

