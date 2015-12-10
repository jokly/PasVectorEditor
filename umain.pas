unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, StdCtrls, Grids, Windows, Math,
  UAbout, UTools, UFigures, UCoordinateSystem;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonAllCanvas: TButton;
    ColorDialog: TColorDialog;
    DrawGridColor: TDrawGrid;
    EditZoom: TEdit;
    ColorPanel: TPanel;
    LeftColor: TShape;
    MActions: TMenuItem;
    MClear: TMenuItem;
    MDelete: TMenuItem;
    MNew: TMenuItem;
    MExit: TMenuItem;
    MOpen: TMenuItem;
    MSave: TMenuItem;
    MSaveAs: TMenuItem;
    MUndo: TMenuItem;
    MUp: TMenuItem;
    MDown: TMenuItem;
    OpenDialog: TOpenDialog;
    RightColor: TShape;
    SaveDialog: TSaveDialog;
    ValueOfZoom: TLabel;
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MAbout: TMenuItem;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure LeftColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MAboutClick(Sender: TObject);
    procedure MNewClick(Sender: TObject);
    procedure MOpenClick(Sender: TObject);
    procedure MSaveClick(Sender: TObject);
    procedure MSaveAsClick(Sender: TObject);
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

const
  AppName =  'pasVectorEditor';
  Untitled = 'untitled';

var
  IndexOfBtn: Integer;
  ArrayOfColor: array[1..14] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal,
    clGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua);
  ShiftState: TShiftState;
  IsSaved: Boolean = True;
  FileName: String = Untitled;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  MainForm.Caption:= FileName + ' - ' + AppName;
  DefaultFormatSettings.DecimalSeparator:= '.';

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
  PropPanel:= PropertiesPanel;
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
  if TTool.Figure = Nil then Exit;
  if Button = mbLeft then TTool.Figure.SetPenColor(LeftColor.Brush.Color)
  else if Button = mbRight then TTool.Figure.SetPenColor(RightColor.Brush.Color);
  if TTool.Figure.ClassParent = TFillFigure then begin
    if Button = mbLeft then
      (TTool.Figure as TFillFigure).SetBrushColor(RightColor.Brush.Color)
    else if Button = mbRight then
      (TTool.Figure as TFillFigure).SetBrushColor(LeftColor.Brush.Color);
  end;
end;

procedure TMainForm.ButtonAllCanvasClick(Sender: TObject);
var
  RectLoupe: TTRectangleLoupe;
begin
  RectLoupe:= (TTRectangleLoupe.newinstance as TTRectangleLoupe);
  RectLoupe.OnMouseDown(mbLeft, ShiftState, MinCoordinate);
  RectLoupe.OnMouseUp(mbLeft, ShiftState, MaxCoordinate);
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
  aCol, aRow, i: Integer;
begin
  DrawGridColor.MouseToCell(X, Y, aCol, aRow);
  if Button = mbLeft then begin
    LeftColor.Brush.Color:= ArrayOfColor[aRow * 7 + aCol + 1];
    for i:= 0 to High(Figures) do
    if Figures[i].IsSelected then
      Figures[i].SetPenColor(LeftColor.Brush.Color);
  end
  else if Button = mbRight then begin
    RightColor.Brush.Color:= ArrayOfColor[aRow * 7 + aCol + 1];
    for i:= 0 to High(Figures) do
    if (Figures[i].ClassParent = TFillFigure) and (Figures[i].IsSelected) then
       (Figures[i] as TFillFigure).SetBrushColor(RightColor.Brush.Color);
  end;
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Ans: TModalResult;
begin
  if IsSaved then
     Exit;
  Ans:= MessageDlg('Save changes?', 'File has been modified, save changes?', mtConfirmation,
                   [mbYes, mbNo, mbIgnore], 0);
  if Ans = mrYes then
     MSave.Click
  else if Ans = mrNo then
     CanClose:= True
  else
    CanClose:= False;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ShiftState:= Shift;
  if GetKeyState(VK_LBUTTON) < 0 then
    Exit;
  if (Key = VK_N) and (Shift = [ssCtrl]) then
     MNew.Click
  else if (Key = VK_S) and (Shift = [ssCtrl]) then
     MSave.Click
  else if (Key = VK_S) and (Shift = [ssCtrl, ssShift]) then
     MSaveAs.Click
  else if (Key = VK_O) and (Shift = [ssCtrl]) then
     MOpen.Click
  else if (Key = VK_Z) and (Shift = [ssCtrl]) then
     TFigure.DeleteLastFigure()
  else if (Key = VK_D) and (Shift = [ssCtrl]) then
     TSelectedFiguresMethods.Delete()
  else if (Key = VK_UP) and (Shift = [ssCtrl]) then
     TSelectedFiguresMethods.ToTopFigures()
  else if (Key = VK_DOWN) and (Shift = [ssCtrl]) then
     TSelectedFiguresMethods.ToBottomFigures()
  else if (Key = VK_C) and (Shift = [ssCtrl]) then begin
    while TFigure.GetLastFigure() <> Nil do
      TFigure.DeleteLastFigure();
    MinCoordinate:= WorldPoint(0, 0);
    MaxCoordinate:= WorldPoint(0, 0);
    MinBounds.X:= 0; MinBounds.Y:= 0;
    MaxBounds.X:= PaintBox.Width;
    MaxBounds.Y:= PaintBox.Height;
    UpdateScrollBarsAndZoom();
  end;
  Invalidate;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ShiftState:= Shift;
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
var
  i: Integer;
begin
  if ColorDialog.Execute then
     LeftColor.Brush.Color:= ColorDialog.Color;
  for i:= 0 to High(Figures) do
    if Figures[i].IsSelected then
      Figures[i].SetPenColor(ColorDialog.Color);
  Invalidate;
end;

procedure TMainForm.RightColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if ColorDialog.Execute then
     RightColor.Brush.Color:= ColorDialog.Color;
  for i:= 0 to High(Figures) do
    if (Figures[i].ClassParent = TFillFigure) and (Figures[i].IsSelected) then
       (Figures[i] as TFillFigure).SetBrushColor(ColorDialog.Color);
  Invalidate;
end;

procedure TMainForm.MAboutClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TMainForm.MNewClick(Sender: TObject);
var
  Ans: Integer;
begin
  if not IsSaved then begin
    Ans:= MessageDlg('Save changes?', 'File has been modified, save changes?', mtConfirmation,
                   [mbYes, mbNo, mbIgnore], 0);
    if Ans = mrYes then
       MSave.Click
    else if Ans = mrIgnore then
       Exit;
  end;
  FileName:= Untitled;
  MainForm.Caption:= FileName + ' - ' + AppName;
  IsSaved:= True;
  SetLength(Figures, 0);
  Invalidate;
end;

procedure TMainForm.MOpenClick(Sender: TObject);
var
  Ans: Integer;
begin
  if not IsSaved then begin
    Ans:= MessageDlg('Save changes?', 'File has been modified, save changes?', mtConfirmation,
                   [mbYes, mbNo, mbIgnore], 0);
    if Ans = mrYes then
       MSave.Click
    else if Ans = mrIgnore then
      Exit;
  end;
  if (OpenDialog.Execute) and (TFigure.LoadFile(OpenDialog.FileName)) then begin
     MainForm.Caption:= OpenDialog.FileName + ' - ' + AppName;
     FileName:= OpenDialog.FileName;
     IsSaved:= True;
  end;
  Invalidate;
end;

procedure TMainForm.MSaveClick(Sender: TObject);
begin
  if FileName = Untitled then
     MSaveAs.Click
  else begin
    TFigure.SaveFile(FileName);
    MainForm.Caption:= FileName + ' - ' + AppName;
    IsSaved:= True;
  end;
end;

procedure TMainForm.MSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    TFigure.SaveFile(SaveDialog.FileName);
    MainForm.Caption:= SaveDialog.FileName + ' - ' + AppName;
    FileName:= SaveDialog.FileName;
    IsSaved:= True;
  end;
end;

procedure TMainForm.MExitClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown:= True;
  TTool.Tools[IndexOfBtn].OnMouseDown(Button, ShiftState, ToWorldPoint(X, Y));
  SetColors(Button);
  IsSaved:= False;
  MainForm.Caption:= FileName + '* - ' + AppName;
  Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsMouseDown then begin
    TTool.Tools[IndexOfBtn].OnMouseMove(ShiftState, ToWorldPoint(X, Y));
    CalculateBounds(ToWorldPoint(X, Y));
    UpdateScrollBarsAndZoom();
    Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown:= False;
  TTool.Tools[IndexOfBtn].OnMouseUp(Button, ShiftState, ToWorldPoint(X, Y));
  Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  Figure: TFigure;
begin
  for Figure in Figures do
    Figure.Draw(PaintBox.Canvas);
  if TTool.Figure <> Nil then
    TTool.Figure.Draw(PaintBox.Canvas);
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
  TTool.ShowProperties(TTool.Tools[IndexOfBtn], PropertiesPanel);
end;

end.

