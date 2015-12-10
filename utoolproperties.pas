unit UToolProperties;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, ExtCtrls, Controls, FPCanvas, Graphics, TypInfo;

type

TToolProp = Class(TObject)
  protected
    procedure OnChangeEditor(Sender: TObject); virtual;
  public
    TypeName: String;
    FEditor: TWinControl;
    function CreateProp(AName: String; Panel: TWinControl): TToolProp; virtual; abstract;
end;

{ TIntegerProp }

TIntegerProp = Class(TToolProp)
  protected
    procedure OnChangeEditor(Sender: TObject); override;
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

{ TToolProps }

TToolProps = Class(TObject)
  private
    FPropsList: array of TToolProp; static;
    FPropEditors: array of TToolProp; static;
  public
    constructor Create(AFigure: TObject; Panel: TWinControl);
    constructor Create(AFigures: array of TObject; Panel: TWinControl);
    procedure Delete;
    class procedure ApplyProps(AFigure: TObject);
    class procedure AddProp(Prop: TToolProp; Name: String);
end;

implementation

const
  InitYPosEditor = 65;
  MaxValueLength = 2;

var
  Figures: array of TObject;
  YPosEditor: Integer = InitYPosEditor;

procedure TToolProp.OnChangeEditor(Sender: TObject);
var
  i: Integer;
begin
  with Sender as TWinControl do begin
    for i:= 0 to High(Figures) do
      SetPropValue(Figures[i], Name, Caption);
  end;
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

procedure TIntegerProp.OnChangeEditor(Sender: TObject);
begin
  if (Sender as TWinControl).Caption = '' then
    (Sender as TWinControl).Caption:= '1';
  inherited OnChangeEditor(Sender);
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
  SetLength(Figures, 1);
  Figures[0]:= AFigure;
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

constructor TToolProps.Create(AFigures: array of TObject; Panel: TWinControl);
var
  Num1, Num2, i, j, g: Integer;
  PropList1, PropList2: PPropList;
  PropInfo1, PropInfo2: TPropInfo;
  IsConsider: Integer;
begin
  if Length(AFigures) = 0 then Exit;
  SetLength(Figures, Length(AFigures));
  for i:= 0 to High(Figures) do
    Figures[i]:= AFigures[i];
  Num1:= GetPropList(AFigures[0].ClassInfo, PropList1);
  for i:= 1 to High(AFigures) do begin
    Num2:= GetPropList(Figures[i].ClassInfo, PropList2);
    for j:= 0 to Num1 - 1 do begin
      PropInfo1:= PropList1^[j]^;
      IsConsider:= j;
      for g:= 0 to Num2 - 1 do begin
        PropInfo2:= PropList2^[g]^;
        if PropInfo1.Name = PropInfo2.Name then begin
          IsConsider:= -1;
          break;
        end;
      end;
      if IsConsider <> -1 then begin
        for g:=IsConsider to Num1 - 2 do
          PropList1^[g]:= PropList1^[g + 1];
        dec(Num1);
      end;
    end;
  end;
  SetLength(FPropEditors, Num1);
  for i:= 0 to Num1 - 1 do begin
    PropInfo1:= PropList1^[i]^;
    for j:= 0 to High(FPropsList) do
      if PropInfo1.PropType^.Name = FPropsList[j].TypeName then begin
        FPropEditors[i]:= FPropsList[j].CreateProp(PropInfo1.Name, Panel);
        break;
      end;
  end;
end;

class procedure TToolProps.ApplyProps(AFigure: TObject);
var
  i: Integer;
begin
  SetLength(Figures, 1);
  Figures[0]:= AFigure;
  for i:= 0 to High(FPropEditors) do
    FPropEditors[i].OnChangeEditor(FPropEditors[i].FEditor);
end;

procedure TToolProps.Delete();
var
  i,s: Integer;
begin
  if Self = Nil then
    Exit;
  s:= High(FPropEditors);
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

