unit outputviewer_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, HtmlView;

type

  { TOutputViewerForm }

  TOutputViewerForm = class(TForm)
    HtmlViewer1: THtmlViewer;
  private

  public

  end;

var
  OutputViewerForm: TOutputViewerForm;

implementation

{$R *.lfm}

end.

