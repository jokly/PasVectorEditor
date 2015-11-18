unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, StdCtrls, LCLtype, Grids, Windows, Math, FPCanvas,
  UAbout, UTools, UFigures, UCoordinateSystem, UToolProperties;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonAllCanvas: TButton;
    ColorDialog: TColorDialog;
    DrawGridColor: TDrawGrid;
    EditZoom: TEdit;
    ColorPanel: TPanel;
    LeftColor: TShape;
    RightColor: TShape;
    ValueOfZoom: TLabel;
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MExit: TMenuItem;
    PaintBox: TPaintBox;
    ScrollBarVertical: TScrollBar;
    ScrollBarHorizontal: TScrollBar;
    ToolsPanel: TPanel;
    PropertiesPanel: TPanel;
    procedure ButtonAllCanvasClick(Sender: TObject);
    procedure DrawGridColorDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure LeftColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MAboutClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure RightColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBarHorizontalScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ScrollBarVerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ToolClick(Sender: TObject);
    procedure UpdateScrollBarsAndZoom();
    procedure SetColors(Button: TMouseButton);
  end;

var
  MainForm: TMainForm;

implementation

var
  IndexOfBtn: Integer;
  ArrayOfColor: array[1..14] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal,
    clGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua);
  ToolParams: TToolParams;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ToolsPanel.Height:= SizeOfButton + 2 * SpaceBetweenButtons;
  for i:= 0 to High(TTool.Tools) do begin
    TTool.Tools[i].ButtonOnForm:= TBitBtn.Create(Self);
    with TTool.Tools[i].ButtonOnForm do begin
      Name:= TTool.Tools[i].ToString + IntToStr(i);
      Caption:= '';
      Parent:= Self;
      Width:= SizeOfButton;
      Height:= SizeOfButton;
      Glyph:= TTool.Tools[i].ImageOfButton;
      BorderStyle:= bsNone;
      Spacing:=0;
      Left:= (i+1) * SpaceBetweenButtons + i * SizeOfButton;
      Top:= SpaceBetweenButtons;
      OnClick:= @MainForm.ToolClick;
      Tag:= i;
    end;
  end;
  TTool.Tools[0].ButtonOnForm.Click;
end;

procedure TMainForm.UpdateScrollBarsAndZoom();
begin
  ScrollBarHorizontal.SetParams(
    Round(Delta.X),
    Round(Min(MinBounds.X * Zoom, Delta.X)),
    Round(Max(MaxBounds.X * Zoom, Delta.X)));
  ScrollBarVertical.SetParams(
    Round(Delta.Y),
    Round(Min(MinBounds.Y * Zoom, Delta.Y)),
    Round(Max(MaxBounds.Y * Zoom, Delta.Y)));
  EditZoom.Text:= IntToStr(Round(Zoom * 100));
  ValueOfZoom.Caption:= EditZoom.Text + '%';
end;

procedure TMainForm.SetColors(Button: TMouseButton);
begin
  if Button = mbLeft then TTPaint.Figure.SetPenColor(LeftColor.Brush.Color)
  else if Button = mbRight then TTPaint.Figure.SetPenColor(RightColor.Brush.Color);
  if TTPaint.Figure.ClassParent = TFillFigure then begin
    if Button = mbLeft then
      (TTPaint.Figure as TFillFigure).SetBrushColor(RightColor.Brush.Color)
    else if Button = mbRight then
      (TTPaint.Figure as TFillFigure).SetBrushColor(LeftColor.Brush.Color);
  end;
end;

procedure TMainForm.ButtonAllCanvasClick(Sender: TObject);
var
  RectLoupe: TTRectangleLoupe;
  Shift: set of TShiftStateEnum;
begin
  RectLoupe:= (TTRectangleLoupe.newinstance as TTRectangleLoupe);
  RectLoupe.OnMouseDown(mbLeft, MinCoordinate);
  RectLoupe.OnMouseUp(mbLeft, MaxCoordinate);
  UpdateScrollBarsAndZoom();
  Invalidate;
end;

procedure TMainForm.DrawGridColorDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with DrawGridColor.Canvas do begin
    Brush.Color:= ArrayOfColor[aRow * 7 + aCol + 1];
    FillRect(aRect);
  end;
end;

procedure TMainForm.DrawGridColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aCol, aRow: longint;
begin
  DrawGridColor.MouseToCell(X, Y, aCol, aRow);
  if Button = mbLeft then
    LeftColor.Brush.Color:= ArrayOfColor[aRow * 7 + aCol + 1]
  else if Button = mbRight then
    RightColor.Brush.Color:= ArrayOfColor[aRow * 7 + aCol + 1];
end;

procedure TMainForm.EditZoomChange(Sender: TObject);
begin
  if EditZoom.Text = '' then EditZoom.Text:= IntToStr(Round(MinZoom * 100))
  else if StrToInt(EditZoom.Text) < MinZoom * 100 then
     EditZoom.Text:= IntToStr(Round(MinZoom * 100))
  else if StrToInt(EditZoom.Text) > MaxZoom * 100 then
     EditZoom.Text:= IntToStr(Round(MaxZoom * 100));
  Zoom:= StrToInt(EditZoom.Text) / 100;
  UpdateScrollBarsAndZoom();
  Invalidate;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if GetKeyState(VK_LBUTTON) < 0 then Exit;
  if (Key = VK_Z) and (Shift = [ssCtrl]) then
     TFigure.DeleteLastFigure();
  if (Key = VK_C) and (Shift = [ssCtrl]) then begin
    while TFigure.GetLastFigure() <> Nil do
      TFigure.DeleteLastFigure();
    MinCoordinate:= WorldPoint(0, 0);
    MaxCoordinate:= WorldPoint(0, 0);
    MinBounds.X:= 0; MinBounds.Y:= 0;
    MaxBounds.X:= PaintBox.Width; MaxBounds.Y:= PaintBox.Height;
    UpdateScrollBarsAndZoom();
  end;
  Invalidate;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  SizeOfWindow.X:= PaintBox.Width;
  SizeOfWindow.Y:= PaintBox.Height;
  MinBounds.X:= Min(0, MinBounds.X);
  MinBounds.Y:= Min(0, MinBounds.Y);
  MaxBounds.X:= Max(SizeOfWindow.X, MaxBounds.X);
  MaxBounds.Y:= Max(SizeOfWindow.Y, MaxBounds.Y);
  UpdateScrollBarsAndZoom();
end;

procedure TMainForm.LeftColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
     LeftColor.Brush.Color:= ColorDialog.Color;
end;

procedure TMainForm.MAboutClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TMainForm.MExitClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown:= True;
  TTool.Tools[IndexOfBtn].OnMouseDown(Button, ToWorldPoint(X, Y));
  SetColors(Button);
  Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsMouseDown then begin
    TTool.Tools[IndexOfBtn].OnMouseMove(ToWorldPoint(X, Y));
    CalculateBounds(ToWorldPoint(X, Y));
    UpdateScrollBarsAndZoom();
  end;
  Invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown:= False;
  TTool.Tools[IndexOfBtn].OnMouseUp(Button, ToWorldPoint(X, Y));
  Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  Figure: TFigure;
begin
  for Figure in FFigures do
    Figure.Draw(PaintBox.Canvas);
  TTPaint.Figure.Draw(PaintBox.Canvas);
end;

procedure TMainForm.RightColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
     RightColor.Brush.Color:= ColorDialog.Color;
end;

procedure TMainForm.ScrollBarHorizontalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Delta.X:= ScrollPos;
  Invalidate;
end;

procedure TMainForm.ScrollBarVerticalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Delta.Y:= ScrollPos;
  Invalidate;
end;

procedure TMainForm.ToolClick(Sender: TObject);
var
  PaintTool: TTPaint;
begin
  IndexOfBtn:= (Sender as TBitBtn).Tag;
  ToolParams.Delete;
  if TTool.Tools[IndexOfBtn].ClassParent = TTPaint then begin
    PaintTool:= TTool.Tools[IndexOfBtn] as TTPaint;
    PaintTool.CreateFigure();
    ToolParams:= TToolParams.Create(PaintTool.Figure, MainForm);
  end;
end;

end.

