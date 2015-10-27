unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, ColorBox, StdCtrls, LCLtype, ComCtrls, Windows, UAbout,
  UTools, UFigures, UCoordinateSystem;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
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
  PaintBox.Canvas.Brush.Color:= clGreen;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  TTool.Tools[0].ButtonOnForm.Click;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if GetKeyState(VK_LBUTTON) < 0 then Exit;
  if (Key = VK_Z) and (Shift = [ssCtrl]) then
     TFigure.DeleteLastFigure();
  PaintBox.Invalidate;
  if (Key = VK_C) and (Shift = [ssCtrl]) then begin
    while TFigure.GetLastFigure() <> nil do
      TFigure.DeleteLastFigure();
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if PaintBox.Width < MainForm.Width then PaintBox.Width:= MainForm.Width;
  if PaintBox.Height < MainForm.Height then PaintBox.Height:= MainForm.Height;
  Invalidate;
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
  TTool.Tools[IndexOfBtn].OnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsMouseDown then begin
    if X > (PaintBox.Width - Bound) then begin
      PaintBox.Width:= PaintBox.Width + Addition;
      PaintBox.Left:= PaintBox.Left - Addition;
      ScrollBarHorizontal.SetParams(-PaintBox.Left, 0, -PaintBox.Left);
    end;
    if Y > (PaintBox.Height - Bound) then begin
      PaintBox.Height:= PaintBox.Height + Addition;
      PaintBox.Top:= PaintBox.Top - Addition;
      ScrollBarVertical.SetParams(-PaintBox.Top, 0, -PaintBox.Top);
    end;
    TTool.Tools[IndexOfBtn].OnMouseMove(Sender, Shift, X, Y);
    Label1.Caption:= IntToStr(PaintBox.Width) + ' : ' + IntToStr(PaintBox.Height) + ' Y:' + IntToStr(Y);
  end;

  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown:= False;
  TTool.Tools[IndexOfBtn].OnMouseUp(Sender, Button, Shift, X, Y);
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
  TTool.SetPenColor(PenColorBox.Selected);
end;

procedure TMainForm.PenWidthBoxChange(Sender: TObject);
begin
  TTool.SetPenWidth(StrToInt(PenWidthBox.Text));
end;

procedure TMainForm.ScrollBarHorizontalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  PaintBox.Left:= -ScrollPos;
end;

procedure TMainForm.ScrollBarVerticalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  PaintBox.Top:= -ScrollPos;
end;

procedure TMainForm.ToolClick(Sender: TObject);
begin
  IndexOfBtn:= (Sender as TBitBtn).Tag;
end;

procedure TMainForm.TrackBarZoomChange(Sender: TObject);
begin
  Zoom:= TrackBarZoom.Position;
  Invalidate;
end;

end.

