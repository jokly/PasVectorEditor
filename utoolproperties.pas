unit UToolProperties;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, ExtCtrls, Controls, FPCanvas, Graphics, TypInfo;

type

TToolProp = Class(TObject)
  protected
    procedure OnChangeEditor(Sender: TObject);
  public
    TypeName: String;
    FEditor: TWinControl;
    function CreateProp(AName: String; Panel: TWinControl): TToolProp; virtual; abstract;
end;

TIntegerProp = Class(TToolProp)
  public
    function CreateProp(AName: String; Panel: TWinControl): TToolProp; override;
end;

TPenStyleProp = Class(TToolProp)
  public
    function CreateProp(AName: String; Panel: TWinControl): TToolProp; override;
end;

TBrushStyleProp = Class(TToolProp)
  public
    function CreateProp(AName: String; Panel: TWinControl): TToolProp; override;
end;

TToolProps = Class(TObject)
  private
    FPropsList: array of TToolProp; static;
    FPropEditors: array of TToolProp; static;
  public
    constructor Create(AFigure: TObject; Panel: TWinControl);
    procedure Delete;
    class procedure ChangeProps(AFigure: TObject);
    class procedure AddProp(Prop: TToolProp; Name: String);
end;

implementation

const
  InitYPosEditor = 65;
  MaxValueLength = 2;

var
  Figure: TObject;
  YPosEditor: Integer = InitYPosEditor;

procedure TToolProp.OnChangeEditor(Sender: TObject);
begin
  with Sender as TWinControl do
    SetPropValue(Figure, Name, Caption);
end;

function CreateComboBox(AName: String; Panel: TWinControl; Values: array of String; Param: TToolProp): TComboBox;
var
  i: Integer;
begin
  Result:= TComboBox.Create(Panel);
  with (Result as TComboBox) do begin
    Visible:= False;
    Top:= YPosEditor + 10;
    YPosEditor:= Top + Height;
    Width:= 73;
    Left:= 8;
    Parent:= Panel;
    Name:= AName;
    ReadOnly:= True;
    for i:= 0 to High(Values) do
      AddItem(Values[i], Result);
    Caption:= Items[0];
    OnChange:= @Param.OnChangeEditor;
    Visible:= True;
  end;
end;

function TIntegerProp.CreateProp(AName: String; Panel: TWinControl): TToolProp;
var
  ToolParam: TToolProp;
begin
  ToolParam:= TToolProp.Create;
  ToolParam.FEditor:= TLabeledEdit.Create(Panel);
  with (ToolParam.FEditor as TLabeledEdit) do begin
    Visible:= False;
    Top:= YPosEditor + 20;
    YPosEditor:= Top + Height;
    Width:= 73;
    Left:= 8;
    Parent:= Panel;
    Name:= AName;
    Text:= '1';
    EditLabel.Caption:= AName;
    EditLabel.Visible:= True;
    LabelPosition:= lpAbove;
    MaxLength:= MaxValueLength;
    NumbersOnly:= True;
    OnChange:= @Self.OnChangeEditor;
    Visible:= True;
  end;
  Result:= ToolParam;
end;

function TPenStyleProp.CreateProp(AName: String; Panel: TWinControl): TToolProp;
var
  Styles: array[1..8] of String = ('psSolid', 'psDash', 'psDot', 'psDashDot',
  'psDashDotDot', 'psinsideFrame', 'psPattern', 'psClear');
  ToolParam: TToolProp;
begin
  ToolParam:= TToolProp.Create;
  ToolParam.FEditor:= CreateComboBox(AName, Panel, Styles, Self);
  Result:= ToolParam;
end;

function TBrushStyleProp.CreateProp(AName: String; Panel: TWinControl): TToolProp;
var
  Styles: array[1..10] of String = ('bsSolid', 'bsClear', 'bsHorizontal',
  'bsVertical', 'bsFDiagonal', 'bsBDiagonal', 'bsCross', 'bsDiagCross',
  'bsImage', 'bsPattern');
  ToolParam: TToolProp;
begin
  ToolParam:= TToolProp.Create;
  ToolParam.FEditor:= CreateComboBox(AName, Panel, Styles, Self);
  Result:= ToolParam;
end;

constructor TToolProps.Create(AFigure: TObject; Panel: TWinControl);
var
  Num, i, j: Integer;
  PropList: PPropList;
  PropInfo: TPropInfo;
begin
  if AFigure = Nil then Exit;
  Figure:= AFigure;
  Num:= GetPropList(AFigure.ClassInfo, PropList);
  SetLength(FPropEditors, Num);
  for i:= 0 to Num - 1 do begin
    PropInfo:= PropList^[i]^;
    for j:= 0 to High(FPropsList) do
      if PropInfo.PropType^.Name = FPropsList[j].TypeName then begin
        FPropEditors[i]:= FPropsList[j].CreateProp(PropInfo.Name, Panel);
        break;
      end;
  end;
end;

class procedure TToolProps.ChangeProps(AFigure: TObject);
var
  i: Integer;
begin
  Figure:= AFigure;
  for i:= 0 to High(FPropEditors) do
    FPropEditors[i].OnChangeEditor(FPropEditors[i].FEditor);
end;

procedure TToolProps.Delete();
var
  i: Integer;
begin
  if Self = Nil then Exit;
  for i:= 0 to High(FPropEditors) do
    FPropEditors[i].FEditor.Free;
  SetLength(FPropEditors, 0);
  YPosEditor:= InitYPosEditor;
end;

class procedure TToolProps.AddProp(Prop: TToolProp; Name: String);
begin
  SetLength(FPropsList, Length(FPropsList) + 1);
  Prop.TypeName:= Name;
  FPropsList[High(FPropsList)]:= Prop;
end;

initialization
TToolProps.AddProp(TIntegerProp.Create, 'LongInt');
TToolProps.AddProp(TPenStyleProp.Create, 'TFPPenStyle');
TToolProps.AddProp(TBrushStyleProp.Create, 'TFPBrushStyle');

end.

