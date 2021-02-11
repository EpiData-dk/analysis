unit introduction_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TIntroductionForm }

  TIntroductionForm = class(TForm)
    Memo1: TMemo;
  private

  public

  end;

var
  IntroductionForm: TIntroductionForm;

implementation

{$R *.lfm}

end.

