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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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
    TrackBarZoom: TTrackBar;
    procedure ButtonAllCanvasClick(Sender: TObject);
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
    procedure TrackBarZoomChange(Sender: TObject);
    procedure UpdateScrollBarsAndZoom();
  end;

var
  MainForm: TMainForm;

implementation

const
  Bound = 60;
  Addition = 3;

var
  IndexOfBtn: Integer;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.UpdateScrollBarsAndZoom();
begin
  ScrollBarHorizontal.SetParams(
    Round(WindowPos.X),
    Round(MinBounds.X * Zoom),
    Round(MaxBounds.X * Zoom));
  ScrollBarVertical.SetParams(
    Round(WindowPos.Y),
    Round(MinBounds.Y * Zoom),
    Round(MaxBounds.Y * Zoom));
  TrackBarZoom.Position:= Round(Zoom);
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
  TrackBarZoom.Min:= MinZoom;
  TrackBarZoom.Max:= MaxZoom;
  WindowPos:= WorldPoint(0, 0);
end;

procedure TMainForm.ButtonAllCanvasClick(Sender: TObject);
var
  RectLoupe: TTRectangleLoupe;
  Shift: set of TShiftStateEnum;
begin
  RectLoupe:= (TTRectangleLoupe.newinstance as TTRectangleLoupe);
  RectLoupe.OnMouseDown(Sender, mbLeft, Shift, WorldPoint(MinBounds.X, MinBounds.Y));
  RectLoupe.OnMouseUp(Sender, mbLeft, Shift, WorldPoint(MaxBounds.X, MaxBounds.Y));
  Invalidate;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if GetKeyState(VK_LBUTTON) < 0 then Exit;
  if (Key = VK_Z) and (Shift = [ssCtrl]) then
     TFigure.DeleteLastFigure();
  if (Key = VK_C) and (Shift = [ssCtrl]) then begin
    while TFigure.GetLastFigure() <> nil do
      TFigure.DeleteLastFigure();
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
  TTool.Tools[IndexOfBtn].OnMouseDown(Sender, Button, Shift, WorldPoint(X, Y));
  UpdateScrollBarsAndZoom();
  Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsMouseDown then begin
    TTool.Tools[IndexOfBtn].OnMouseMove(Sender, Shift, WorldPoint(X, Y));
    ScrollBarHorizontal.Position:= Round(WindowPos.X);
    ScrollBarVertical.Position:= Round(WindowPos.Y);
    TrackBarZoom.Position:= Round(Zoom);
    if X > (PaintBox.Width - Bound) then begin
      Delta.X+= Addition;
      MaxBounds.X+= Addition;
      WindowPos.X+= Addition;
    end
    else if X < Bound then begin
      Delta.X-= Addition;
      MinBounds.X-= Addition;
      WindowPos.X-= Addition;
    end;
    if Y > (PaintBox.Height - Bound) then begin
      Delta.Y+= Addition;
      MaxBounds.Y+= Addition;
      WindowPos.Y+= Addition;
    end
    else if Y < Bound then begin
      Delta.Y-= Addition;
      MinBounds.Y-= Addition;
      WindowPos.Y-= Addition;
    end;
    UpdateScrollBarsAndZoom();
  end;
  //Label1.Caption:= 'SHo ' + IntToStr(ScrollBarHorizontal.Min) + ' ' + IntToStr(ScrollBarHorizontal.Max);
  //Label4.Caption:= 'SVe ' + IntToStr(ScrollBarVertical.Min) + ' ' + IntToStr(ScrollBarVertical.Max);
  //Label2.Caption:= 'WoP ' + IntToStr(Round(TWorldPoint.WorldPoint(X, Y).X)) + ' ' + IntToStr(Round(TWorldPoint.WorldPoint(X, Y).Y));
  //Label3.Caption:= 'ScP ' + IntToStr(X) + ' ' + IntToStr(Y);
  //Label5.Caption:= 'WinP ' + IntToStr(Round(WindowPos.X)) + ' ' + IntToStr(Round(WindowPos.Y));
  Invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown:= False;
  TTool.Tools[IndexOfBtn].OnMouseUp(Sender, Button, Shift, WorldPoint(X, Y));
  UpdateScrollBarsAndZoom();
  Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  Figure: TFigure;
begin
  ValueOfZoom.Caption:= FloatToStr(Round(Zoom)) + '%';
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

procedure TMainForm.TrackBarZoomChange(Sender: TObject);
begin
  UpdateScrollBarsAndZoom();
  Invalidate;
end;

end.

