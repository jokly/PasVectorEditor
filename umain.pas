unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, ColorBox, StdCtrls, LCLtype, ComCtrls, Windows, Math, UAbout,
  UTools, UFigures, UCoordinateSystem;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonAllCanvas: TButton;
    EditZoom: TEdit;
    ValueOfZoom: TLabel;
    PenWidthBox: TComboBox;
    PenColorBox: TColorBox;
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
    procedure EditZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenColorBoxChange(Sender: TObject);
    procedure PenWidthBoxChange(Sender: TObject);
    procedure ScrollBarHorizontalScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ScrollBarVerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ToolClick(Sender: TObject);
    procedure UpdateScrollBarsAndZoom();
  end;

var
  MainForm: TMainForm;

implementation

var
  IndexOfBtn: Integer;

{$R *.lfm}

{ TMainForm }

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

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ToolsPanel.Height:= SizeOfButton + 2 * SpaceBetweenButtons;
  for i:=0 to High(TTool.Tools) do begin
    TTool.Tools[i].ButtonOnForm:= TBitBtn.Create(Self);
    with TTool.Tools[i].ButtonOnForm do begin
      Name:= TTool.Tools[i].ToString + IntToStr(i);
      Caption:= '';
      Parent:= Self;
      Width:= SizeOfButton;
      Height:= SizeOfButton;
      Glyph:= TTool.Tools[i].ImageOfButton;
      BorderStyle:= bsNone;
      Left:= (i+1) * SpaceBetweenButtons + i * SizeOfButton;
      Top:= SpaceBetweenButtons;
      OnClick:= @MainForm.ToolClick;
      Tag:= i;
    end;
  end;
  TTool.Tools[0].ButtonOnForm.Click;
end;

procedure TMainForm.ButtonAllCanvasClick(Sender: TObject);
var
  RectLoupe: TTRectangleLoupe;
  Shift: set of TShiftStateEnum;
begin
  RectLoupe:= (TTRectangleLoupe.newinstance as TTRectangleLoupe);
  RectLoupe.OnMouseDown(Sender, mbLeft, Shift, MinCoordinate);
  RectLoupe.OnMouseUp(Sender, mbLeft, Shift, MaxCoordinate);
  UpdateScrollBarsAndZoom();
  Invalidate;
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
  TTool.Tools[IndexOfBtn].OnMouseDown(Sender, Button, Shift, ToWorldPoint(X, Y));
  UpdateScrollBarsAndZoom();
  Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsMouseDown then begin
    TTool.Tools[IndexOfBtn].OnMouseMove(Sender, Shift, ToWorldPoint(X, Y));
    CalculateBounds(ToWorldPoint(X, Y));
    UpdateScrollBarsAndZoom();
  end;
  Invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown:= False;
  TTool.Tools[IndexOfBtn].OnMouseUp(Sender, Button, Shift, ToWorldPoint(X, Y));
  UpdateScrollBarsAndZoom();
  Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  Figure: TFigure;
begin
  for Figure in FFigures do
      Figure.Draw(PaintBox.Canvas);
end;

procedure TMainForm.PenColorBoxChange(Sender: TObject);
begin
  TTPaint.SetPenColor(PenColorBox.Selected);
end;

procedure TMainForm.PenWidthBoxChange(Sender: TObject);
begin
  TTPaint.SetPenWidth(StrToInt(PenWidthBox.Text));
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
begin
  IndexOfBtn:= (Sender as TBitBtn).Tag;
end;

end.

