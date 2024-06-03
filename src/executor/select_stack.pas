unit select_stack;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, epidatafiles, ast;

type

  { TSelectStack }

  TSelectStack = class(TObjectStack)
  public
    Function Push(F: TEpiIntField): TEpiIntField;
    Function Pop: TEpiIntField;
    Function Peek: TEpiIntField;
  end;

const
  SELECTION_EXPR = 'SELECTION_EXPR';

implementation

{ TSelectStack }

function TSelectStack.Push(F: TEpiIntField): TEpiIntField;
begin
  result := TEpiIntField(inherited Push(F))
end;

function TSelectStack.Pop: TEpiIntField;
begin
  result := TEpiIntField(inherited Pop)
end;

function TSelectStack.Peek: TEpiIntField;
begin
  result := TEpiIntField(inherited Peek)
end;

end.

