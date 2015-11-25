unit UTools;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, Controls, Graphics,
  Buttons, Math, Typinfo, UFigures, UCoordinateSystem, UToolProperties,
  Dialogs; //To Delete

type

  { TTool }

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
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseMove(WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); virtual; abstract;
  end;

  TTPen = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTLine = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTPolyline = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTRectangle = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTRoundRectangle = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTEllipse = Class(TTool)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTLoupe = Class(TTool)
    public
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTHand = Class(TTool)
    private
      StartPos: TWorldPoint; static;
    public
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  TTRectangleLoupe = Class(TTool)
    public
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  { TTCursorSelect }

  TTCursorSelect = Class(TTool)
    public
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  { TTRectSelect }

  TTRectSelect = Class(TTool)
    public
      procedure OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint); override;
      procedure OnMouseMove(WPoint: TWorldPoint); override;
      procedure OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint); override;
  end;

  const
    SpaceBetweenButtons = 7;
    SizeOfButton = 36;
    MinZoom = 0.01;
    MaxZoom = 90;

  var
    IsMouseDown: Boolean;
    MinCoordinate, MaxCoordinate: TWorldPoint;
    CtrlState: Boolean;
    SelectedFigures: array of TFigure;

implementation

const
  ZoomOfLoupe = 0.3;

var
  ToolParams: TToolProps;

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

class procedure TTool.FindMinMaxCoordinate(WPoint: TWorldPoint);
begin
  MinCoordinate:= WorldPoint(Min(WPoint.X, MinCoordinate.X), Min(WPoint.Y, MinCoordinate.Y));
  MaxCoordinate:= WorldPoint(Max(WPoint.X, MaxCoordinate.X), Max(WPoint.Y, MaxCoordinate.Y));
end;

procedure TTool.CreateFigure;
begin
  Figure:= Nil;
end;

procedure TTPen.CreateFigure();
begin
  Figure:= TPen.Create;
  TToolProps.ChangeProps(Figure);
end;

procedure TTPen.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
var
  i: Integer;
begin
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPen.OnMouseMove(WPoint: TWorldPoint);
begin
  (Figure as TPen).AddPoint(WPoint);
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPen.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
begin
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.CreateFigure();
begin
  Figure:= TLine.Create;
  TToolProps.ChangeProps(Figure);
end;

procedure TTLine.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
begin
  (Figure as TLine).StartP:= WPoint;
  (Figure as TLine).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.OnMouseMove(WPoint: TWorldPoint);

begin
  (Figure as TLine).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
var
  ALine: TLine;
begin
  ALine:= (Figure as TLine);
  ALine.EndP:= WPoint;
  ALine.MinP:= WorldPoint(Min(ALine.StartP.X, ALine.EndP.X), Min(ALine.StartP.Y, ALine.EndP.Y));
  ALine.MaxP:= WorldPoint(Max(ALine.StartP.X, ALine.EndP.X), Max(ALine.StartP.Y, ALine.EndP.Y));
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.CreateFigure();
begin
  Figure:= TPolyline.Create;
  TToolProps.ChangeProps(Figure);
end;

procedure TTPolyline.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
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
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.OnMouseMove(WPoint: TWorldPoint);
begin
  if (Figure as TPolyline).GetLastLine <> Nil then
    (Figure as TPolyline).GetLastLine().EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
begin
  IsMouseDown:= True;
  if Button = mbRight then IsMouseDown:= False;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.CreateFigure();
begin
  Figure:= TRectangle.Create;
  TToolProps.ChangeProps(Figure);
end;

procedure TTRectangle.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).StartP:= WPoint;
  (Figure as TRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.OnMouseMove(WPoint: TWorldPoint);
begin
  (Figure as TRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.CreateFigure();
begin
  Figure:= TRoundRectangle.Create;
  TToolProps.ChangeProps(Figure);
end;

procedure TTRoundRectangle.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).StartP:= WPoint;
  (Figure as TRoundRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.OnMouseMove(WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.CreateFigure();
begin
  Figure:= TEllipse.Create;
  TToolProps.ChangeProps(Figure);
end;

procedure TTEllipse.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
begin
  (Figure as TEllipse).StartP:= WPoint;
  (Figure as TEllipse).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.OnMouseMove(WPoint: TWorldPoint);
begin
  (Figure as TEllipse).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
begin
  (Figure as TEllipse).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLoupe.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
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
procedure TTLoupe.OnMouseMove(WPoint: TWorldPoint);
begin

end;

procedure TTLoupe.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
begin

end;

procedure TTHand.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
begin
  TTHand.StartPos:= WPoint;
end;
procedure TTHand.OnMouseMove(WPoint: TWorldPoint);
begin
  Delta.X+= (StartPos.X - WPoint.X) * Zoom;
  Delta.Y+= (StartPos.Y - WPoint.Y) * Zoom;
end;


procedure TTHand.OnMouseUp(Button: TMouseButton;  WPoint: TWorldPoint);
begin

end;

procedure TTRectangleLoupe.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
begin
  TFigure.AddFigure(TRectangle.Create());
  (TFigure.GetLastFigure() as TFillFigure).BrushStyle:= bsClear;
  (TFigure.GetLastFigure() as TRectangle).StartP:= WPoint;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;
procedure TTRectangleLoupe.OnMouseMove(WPoint: TWorldPoint);
begin
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTRectangleLoupe.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
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

procedure TTCursorSelect.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
var
  i: Integer;
  MousePoint: TPoint;
begin
  MousePoint:= ToScreenPoint(WPoint);
  if not(CtrlState) then begin
    SetLength(SelectedFigures, 0);
    for i:=0 to High(Figures) do
      Figures[i].IsSelected:= False;
  end;
  for i:=0 to High(Figures) do begin
    if Figures[i].IsInside(Rect(MousePoint.x - 1, MousePoint.y - 1, MousePoint.x + 1, MousePoint.y + 1)) then begin
      if Figures[i].IsSelected = False then
        Figures[i].IsSelected:= True
      else
        Figures[i].IsSelected:= False;
    end;
  end;
end;

procedure TTCursorSelect.OnMouseMove(WPoint: TWorldPoint);
begin

end;

procedure TTCursorSelect.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
begin

end;

procedure TTRectSelect.OnMouseDown(Button: TMouseButton; WPoint: TWorldPoint);
begin
  TFigure.AddFigure(TRectangle.Create());
  (TFigure.GetLastFigure() as TFillFigure).BrushStyle:= bsClear;
  (TFigure.GetLastFigure() as TRectangle).StartP:= WPoint;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTRectSelect.OnMouseMove(WPoint: TWorldPoint);
begin
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTRectSelect.OnMouseUp(Button: TMouseButton; WPoint: TWorldPoint);
var
  StartP, EndP: TPoint;
  SelectRect: TRect;
  i: Integer;
begin
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
  StartP:= ToScreenPoint((TFigure.GetLastFigure() as TRectangle).StartP);
  EndP:= ToScreenPoint((TFigure.GetLastFigure() as TRectangle).EndP);
  SelectRect:= Rect(StartP.x, StartP.y, EndP.x, EndP.y);
  SetLength(SelectedFigures, 0);
  for i:=0 to High(Figures) do
    Figures[i].IsSelected:= False;
  for i:=0 to High(Figures) do begin
    if Figures[i].IsInside(SelectRect) then begin
      Figures[i].IsSelected:= True;
      SetLength(SelectedFigures, Length(SelectedFigures) + 1);
      SelectedFigures[High(SelectedFigures)]:= Figures[i];
    end;
  end;
  TFigure.DeleteLastFigure();
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
TTool.AddTool(TTCursorSelect.Create('img\cursor.bmp'));
TTool.AddTool(TTRectSelect.Create('img\rectangle.bmp'));

end.

