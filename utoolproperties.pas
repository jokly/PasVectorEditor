unit UToolProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Forms, Controls, FPCanvas, TypInfo,
  UFigures;

type

TToolParam = Class(TObject)
  public
    FEditor: TWinControl;
    constructor Create(_Name: String; Form: TForm); virtual; abstract;
    procedure OnChangeEditor(Sender: TObject);
end;

TIntegerParam = Class(TToolParam)
  public
    constructor Create(_Name: String; Form: TForm); override;
end;

TPenStyleParam = Class(TToolParam)
  public
    constructor Create(_Name: String; Form: TForm); override;
end;

TBrushStyleParam = Class(TToolParam)
  public
    constructor Create(_Name: String; Form: TForm); override;
end;

TToolParams = Class(TObject)
  private
    FPropEditors: array of TToolParam; static;
  public
    constructor Create(_Figure: TFigure; Form: TForm);
    procedure Delete;
    class procedure ChangeFigure(_Figure: TFigure);
end;

implementation

const
  InitYPosEditor = 126;
  MaxValueLength = 2;

var
  Figure: TFigure;
  YPosEditor: Integer = InitYPosEditor;

procedure TToolParam.OnChangeEditor(Sender: TObject);
begin
  with Sender as TWinControl do
    SetPropValue(Figure, Name, Caption);
end;

function CreateComboBox(_Name: String; Form: TForm; Values: array of String; Param: TToolParam): TComboBox;
var
  i: Integer;
begin
  Result:= TComboBox.Create(Form);
  with (Result as TComboBox) do begin
    Visible:= False;
    Top:= YPosEditor + 10;
    YPosEditor:= Top + Height;
    Width:= 73;
    Left:= 8;
    Parent:= Form;
    Name:= _Name;
    for i:= 0 to High(Values) do
      AddItem(Values[i], Result);
    Caption:= Items[0];
    OnChange:= @Param.OnChangeEditor;
    Visible:= True;
  end;
end;

constructor TIntegerParam.Create(_Name: String; Form: TForm);
begin
  FEditor:= TLabeledEdit.Create(Form);
  with (FEditor as TLabeledEdit) do begin
    Visible:= False;
    Top:= YPosEditor + 10;
    YPosEditor:= Top + Height;
    Width:= 73;
    Left:= 8;
    Parent:= Form;
    Name:= _Name;
    Text:= '1';
    EditLabel.Caption:= _Name;
    EditLabel.Visible:= True;
    LabelPosition:= lpAbove;
    MaxLength:= MaxValueLength;
    NumbersOnly:= True;
    OnChange:= @Self.OnChangeEditor;
    Visible:= True;
  end;
end;

constructor TPenStyleParam.Create(_Name: String; Form: TForm);
var
  Styles: array[1..8] of String = ('psSolid', 'psDash', 'psDot', 'psDashDot',
  'psDashDotDot', 'psinsideFrame', 'psPattern', 'psClear');
begin
  FEditor:= CreateComboBox(_Name, Form, Styles, Self);
end;

constructor TBrushStyleParam.Create(_Name: String; Form: TForm);
var
  Styles: array[1..10] of String = ('bsSolid', 'bsClear', 'bsHorizontal',
  'bsVertical', 'bsFDiagonal', 'bsBDiagonal', 'bsCross', 'bsDiagCross',
  'bsImage', 'bsPattern');
begin
  FEditor:= CreateComboBox(_Name, Form, Styles, Self);
end;

constructor TToolParams.Create(_Figure: TFigure; Form: TForm);
var
  Num, i: Integer;
  PropList: PPropList;
  PropInfo: TPropInfo;
begin
  Figure:= _Figure;
  Num:= GetPropList(_Figure.ClassInfo, PropList);
  SetLength(FPropEditors, Num);
  for i:= 0 to Num - 1 do begin
    PropInfo:= PropList^[i]^;
    if PropInfo.PropType^.Kind = tkInteger then
      FPropEditors[i]:= TIntegerParam.Create(PropInfo.Name, Form)
    else if PropInfo.PropType^.Name = 'TFPPenStyle' then
      FPropEditors[i]:= TPenStyleParam.Create(PropInfo.Name, Form)
    else if PropInfo.PropType^.Name = 'TFPBrushStyle' then
      FPropEditors[i]:= TBrushStyleParam.Create(PropInfo.Name, Form);
  end;
end;

class procedure TToolParams.ChangeFigure(_Figure: TFigure);
var
  i: Integer;
begin
  Figure:= _Figure;
  for i:= 0 to High(FPropEditors) do
    FPropEditors[i].OnChangeEditor(FPropEditors[i].FEditor);
end;

procedure TToolParams.Delete();
var
  i: Integer;
begin
  if Self = Nil then Exit;
  for i:= 0 to High(FPropEditors) do
    FPropEditors[i].FEditor.Free;
  SetLength(FPropEditors, 0);
  YPosEditor:= InitYPosEditor;
end;

end.

