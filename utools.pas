unit UTools;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, Controls, Graphics,
  Buttons, Math, Typinfo, UFigures, UCoordinateSystem, UToolProperties;

type

  { TTool }

  { TSelectedFiguresMethods }

  TSelectedFiguresMethods = class(TObject)
    public
      class procedure Delete();
      class procedure ToTopFigures();
      class procedure ToBottomFigures();
  end;

  TTool = Class(TObject)
    public
      Tools: array of TTool; static;
      ButtonOnForm: TBitBtn;
      ImageOfButton: TBitmap;
      Figure: TFigure; static;
      class procedure AddTool(Tool: TTool);
      constructor Create(PathToFile: String);
      class procedure FindMinMaxCoordinate(WPoint: TWorldPoint);
      class procedure ShowProperties(ATool: TTool; Panel: TWinControl);
      procedure CreateFigure(); virtual;
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); virtual; abstract;
  end;

  TTPen = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTLine = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTPolyline = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRectangle = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRoundRectangle = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTEllipse = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTLoupe = Class(TTool)
    public
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTHand = Class(TTool)
    private
      StartPos: TWorldPoint; static;
    public
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRectangleLoupe = Class(TTool)
    public
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  { TTSelect }

  TTSelect = Class(TTool)
    private
      StartPos: TWorldPoint;
      FigureOffset: TWorldPoint;
      IsShiftWasDown: Boolean;
    public
      procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  const
    SpaceBetweenButtons = 7;
    SizeOfButton = 36;
    MinZoom = 0.01;
    MaxZoom = 90;

  var
    IsMouseDown: Boolean;
    MinCoordinate, MaxCoordinate: TWorldPoint;
    PropPanel: TWinControl;

implementation

const
  ZoomOfLoupe = 0.3;

var
  ToolParams: TToolProps;

{ TSelectedFiguresMethods }

class procedure TTool.FindMinMaxCoordinate(WPoint: TWorldPoint);
begin
  MinCoordinate:= WorldPoint(Min(WPoint.X, MinCoordinate.X), Min(WPoint.Y, MinCoordinate.Y));
  MaxCoordinate:= WorldPoint(Max(WPoint.X, MaxCoordinate.X), Max(WPoint.Y, MaxCoordinate.Y));
end;

class procedure TSelectedFiguresMethods.Delete;
var
  i: Integer;
  TempFigures: array of TFigure;
begin
  SetLength(TempFigures, 0);
  for i:= 0 to High(Figures) do
    if not Figures[i].IsSelected then begin
      SetLength(TempFigures, Length(TempFigures) + 1);
      TempFigures[High(TempFigures)]:= Figures[i];
    end;
  Figures:= TempFigures;
end;

class procedure TSelectedFiguresMethods.ToTopFigures;
var
  i, j: Integer;
  TempFigures: array of TFigure;
begin
  SetLength(TempFigures, Length(Figures));
  j:= High(Figures);
  for i:= 0 to High(Figures) do
    if Figures[i].IsSelected then begin
      TempFigures[j]:= Figures[i];
      dec(j);
    end;
  for i:= High(Figures) downto 0 do
    if not Figures[i].IsSelected then begin
      TempFigures[j]:= Figures[i];
      Dec(j);
    end;
  Figures:= TempFigures;
end;

class procedure TSelectedFiguresMethods.ToBottomFigures;
var
  i, j: Integer;
  TempFigures: array of TFigure;
begin
  SetLength(TempFigures, Length(Figures));
  j:= 0;
  for i:= 0 to High(Figures) do
    if Figures[i].IsSelected then  begin
      TempFigures[j]:= Figures[i];
      Inc(j);
    end;
  for i:= 0 to High(Figures) do
    if not Figures[i].IsSelected then begin
      TempFigures[j]:= Figures[i];
      Inc(j);
    end;
  Figures:= TempFigures;
end;

class procedure TTool.ShowProperties(ATool: TTool; Panel: TWinControl);
begin
  ToolParams.Delete;
  ATool.CreateFigure();
  ToolParams:= TToolProps.Create(Figure, Panel);
end;

constructor TTool.Create(PathToFile: String);
begin
  ImageOfButton:= TBitmap.Create;
  ImageOfButton.Height:= SizeOfButton;
  ImageOfButton.Width:= SizeOfButton;
  ImageOfButton.LoadFromFile(PathToFile);
end;

class procedure TTool.AddTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)]:= Tool;
end;

procedure TTool.CreateFigure;
begin
  Figure:= Nil;
end;

procedure TTPen.CreateFigure();
begin
  Figure:= TPen.Create;
  TToolProps.ApplyProps(Figure);
end;

procedure TTPen.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TPen).AddPoint(WPoint);
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTPen.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TPen).AddPoint(WPoint);
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTPen.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TPen).AddPoint(WPoint);
  TFigure.AddFigure(Figure);
  CreateFigure();
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.CreateFigure();
begin
  Figure:= TLine.Create;
  TToolProps.ApplyProps(Figure);
end;

procedure TTLine.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TLine).StartP:= WPoint;
  (Figure as TLine).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);

begin
  (Figure as TLine).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
var
  ALine: TLine;
begin
  ALine:= (Figure as TLine);
  ALine.EndP:= WPoint;
  ALine.MinP:= WorldPoint(Min(ALine.StartP.X, ALine.EndP.X), Min(ALine.StartP.Y, ALine.EndP.Y));
  ALine.MaxP:= WorldPoint(Max(ALine.StartP.X, ALine.EndP.X), Max(ALine.StartP.Y, ALine.EndP.Y));
  TFigure.AddFigure(Figure);
  CreateFigure();
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.CreateFigure();
begin
  Figure:= TPolyline.Create;
  TToolProps.ApplyProps(Figure);
end;

procedure TTPolyline.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
var
  ALine: TLine;
begin
  ALine:= TLine.Create;
  ALine.StartP:= WPoint;
  ALine.EndP:= WPoint;
  if (Button = mbRight) and ((Figure as TPolyline).GetLastLine <> Nil) then begin
    (Figure as TPolyline).GetLastLine().EndP:= WPoint;
    (Figure as TPolyline).AddLine(ALine);
    TFigure.AddFigure(Figure);
    CreateFigure();
    IsMouseDown:= False;
    Exit;
  end
  else if Button = mbRight then Exit;
  (Figure as TPolyline).AddLine(ALine);
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
begin
  if (Figure as TPolyline).GetLastLine <> Nil then
    (Figure as TPolyline).GetLastLine().EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  IsMouseDown:= True;
  if Button = mbRight then IsMouseDown:= False;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.CreateFigure();
begin
  Figure:= TRectangle.Create;
  TToolProps.ApplyProps(Figure);
end;

procedure TTRectangle.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).StartP:= WPoint;
  (Figure as TRectangle).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.CreateFigure();
begin
  Figure:= TRoundRectangle.Create;
  TToolProps.ApplyProps(Figure);
end;

procedure TTRoundRectangle.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).StartP:= WPoint;
  (Figure as TRoundRectangle).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.CreateFigure();
begin
  Figure:= TEllipse.Create;
  TToolProps.ApplyProps(Figure);
end;

procedure TTEllipse.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TEllipse).StartP:= WPoint;
  (Figure as TEllipse).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.OnMouseMove(Shift: TShiftState;WPoint: TWorldPoint);
begin
  (Figure as TEllipse).EndP:= WPoint;
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TEllipse).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  TTool.FindMinMaxCoordinate(WPoint);
end;

procedure TTLoupe.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
var
  startP: TPoint;
begin
  startP:= ToScreenPoint(WPoint);
  if Button = mbLeft then
    Zoom+= ZoomOfLoupe
  else if Button = mbRight then
    Zoom-= ZoomOfLoupe;
  Delta.X+= ToScreenPoint(WPoint).X - startP.X;
  Delta.Y+= ToScreenPoint(WPoint).Y - startP.Y;
end;
procedure TTLoupe.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
begin

end;

procedure TTLoupe.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin

end;

procedure TTHand.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  TTHand.StartPos:= WPoint;
end;
procedure TTHand.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
begin
  Delta.X+= (StartPos.X - WPoint.X) * Zoom;
  Delta.Y+= (StartPos.Y - WPoint.Y) * Zoom;
end;


procedure TTHand.OnMouseUp(Button: TMouseButton; Shift: TShiftState;  WPoint: TWorldPoint);
begin

end;

procedure TTRectangleLoupe.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
begin
  TFigure.AddFigure(TRectangle.Create());
  (TFigure.GetLastFigure() as TFillFigure).BrushStyle:= bsClear;
  (TFigure.GetLastFigure() as TRectangle).StartP:= WPoint;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;
procedure TTRectangleLoupe.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
begin
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTRectangleLoupe.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
var
  Rect: TRectangle;
begin
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
  Rect:=(TFigure.GetLastFigure() as TRectangle);
  if (Rect.StartP.X = Rect.EndP.X) or (Rect.StartP.Y = Rect.EndP.Y) then Exit;
  if Min(SizeOfWindow.X / Abs(Rect.StartP.X - Rect.EndP.X),
         SizeOfWindow.Y / Abs(Rect.StartP.Y - Rect.EndP.Y)) < MaxZoom then
    Zoom:= Min(SizeOfWindow.X / Abs(Rect.StartP.X - Rect.EndP.X),
               SizeOfWindow.Y / Abs(Rect.StartP.Y - Rect.EndP.Y));
  Delta.X:= Min(Rect.StartP.X, Rect.EndP.X) * Zoom;
  Delta.Y:= Min(Rect.StartP.Y, Rect.EndP.Y) * Zoom;
  TFigure.DeleteLastFigure();
end;

procedure TTSelect.OnMouseDown(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
var
  i: Integer;
begin
  StartPos:= WPoint;
  if Shift = [ssShift] then begin
    FigureOffset:= WorldPoint(0, 0);
    Exit;
  end
  else if not(Shift = [ssCtrl]) then begin
    for i:=0 to High(Figures) do
      Figures[i].IsSelected:= False;
  end
  else if Shift = [ssShift, ssCtrl] then Exit;
  if not(ssShift in Shift) and (IsShiftWasDown) then
    IsShiftWasDown:= False;
  TFigure.AddFigure(TRectangle.Create());
  (TFigure.GetLastFigure() as TFillFigure).BrushStyle:= bsClear;
  (TFigure.GetLastFigure() as TFillFigure).PenStyle:= psDash;
  (TFigure.GetLastFigure() as TFillFigure).PenWidth:= 3;
  (TFigure.GetLastFigure() as TFillFigure).SetPenColor(clBlue);
  (TFigure.GetLastFigure() as TRectangle).StartP:= WPoint;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTSelect.OnMouseMove(Shift: TShiftState; WPoint: TWorldPoint);
var
  StartP, EndP: TPoint;
  SelectRect: TRect;
  i: Integer;
begin
  if Shift = [ssShift] then begin
    FigureOffset.X:= WPoint.X - StartPos.X;
    FigureOffset.Y:= WPoint.Y - StartPos.Y;
    StartPos:= WPoint;
    for i:= 0 to High(Figures) do
      if Figures[i].IsSelected then
        Figures[i].Depose(FigureOffset);
    IsShiftWasDown:= True;
    Exit;
  end
  else if Shift = [ssShift, ssCtrl] then Exit;
  if IsShiftWasDown then Exit;

  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
  StartP:= ToScreenPoint((TFigure.GetLastFigure() as TRectangle).StartP);
  EndP:= ToScreenPoint((TFigure.GetLastFigure() as TRectangle).EndP);

  SelectRect:= Rect(StartP.x, StartP.y, EndP.x, EndP.y);
  for i:=High(Figures) - 1 downto 0  do begin
      Figures[i].IsSelected:= Figures[i].IsInside(SelectRect);
      if (Figures[i].IsInside(SelectRect)) and (StartPos.X = WPoint.X)
        and (StartPos.Y = StartPos.Y) then
          Break;
  end;
end;

procedure TTSelect.OnMouseUp(Button: TMouseButton; Shift: TShiftState; WPoint: TWorldPoint);
var
  SelectedFigures: array of TObject;
  i: Integer;
begin
  if (Shift = [ssShift, ssCtrl]) or (Shift = [ssShift]) then
    Exit;
  if not(ssShift in Shift) and (IsShiftWasDown) then begin
    IsShiftWasDown:= False;
    Exit;
  end;

  OnMouseMove(Shift, WPoint);

  TFigure.DeleteLastFigure();
  SetLength(SelectedFigures, 0);
  for i:= 0 to High(Figures) do
    if Figures[i].IsSelected then begin
      SetLength(SelectedFigures, Length(SelectedFigures) + 1);
      SelectedFigures[High(SelectedFigures)]:= Figures[i];
    end;

  ToolParams.Delete;
  TToolProps.Create(SelectedFigures, PropPanel);
end;

initialization
TTool.AddTool(TTPen.Create('img\pen.bmp'));
TTool.AddTool(TTLine.Create('img\line.bmp'));
TTool.AddTool(TTPolyline.Create('img\polyline.bmp'));
TTool.AddTool(TTRectangle.Create('img\rectangle.bmp'));
TTool.AddTool(TTRoundRectangle.Create('img\roundRect.bmp'));
TTool.AddTool(TTEllipse.Create('img\ellipse.bmp'));
TTool.AddTool(TTLoupe.Create('img\loupe.bmp'));
TTool.AddTool(TTHand.Create('img\hand.bmp'));
TTool.AddTool(TTRectangleLoupe.Create('img\rectangleLoupe.bmp'));
TTool.AddTool(TTSelect.Create('img\cursor.bmp'));

end.

