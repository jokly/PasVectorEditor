unit UTools;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, Math, Typinfo, UFigures, UCoordinateSystem;

type

  TTool = Class(TObject)
    public
      Tools: array of TTool; static;
      ButtonOnForm: TBitBtn;
      ImageOfButton: TBitmap;
      class procedure AddTool(Tool: TTool);
      constructor Create(PathToFile: String);
      class function ExecMethod(Instance: TObject; _Name: String): Boolean;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); virtual; abstract;
  end;

  TTPaint = Class(TTool)
    protected
      class procedure FindMinMaxCoordinate(WPoint: TWorldPoint);
    public
      Figure: TFigure; static;
      procedure CreateFigure(); virtual; abstract;
      class function GetProperties(_Figure: TFigure): Integer;
    published

  end;

  TTPen = Class(TTPaint)
    public
    procedure CreateFigure(); override;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTLine = Class(TTPaint)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTPolyline = Class(TTPaint)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRectangle = Class(TTPaint)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRoundRectangle = Class(TTPaint)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTEllipse = Class(TTPaint)
    public
      procedure CreateFigure(); override;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTLoupe = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTHand = Class(TTool)
    private
      StartPos: TWorldPoint; static;
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRectangleLoupe = Class(TTool)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  const
    SpaceBetweenButtons = 7;
    SizeOfButton = 36;
    MinZoom = 0.01;
    MaxZoom = 90;

  var
    IsMouseDown: Boolean;
    MinCoordinate, MaxCoordinate: TWorldPoint;
    ToolProperties: PPropList;

implementation

const
  ZoomOfLoupe = 0.3;

var
  ButtonWasDown: TMouseButton = mbMiddle;

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

class function TTPaint.GetProperties(_Figure: TFigure): Integer;
begin
  Result:= GetPropList(_Figure.ClassInfo, ToolProperties);
end;

class function TTool.ExecMethod(Instance: TObject; _Name: String): Boolean;
type
  TProc = procedure of object;
var
  Method: TMethod;
  Exec: TProc;
begin
  Method.Data:= Pointer(Instance);
  Method.Code:= Instance.MethodAddress(_Name);
  Exec:= TProc(Method);
  Result:= Assigned(Method.Code);
  if Result then Exec;
end;

class procedure TTPaint.FindMinMaxCoordinate(WPoint: TWorldPoint);
begin
  MinCoordinate:= WorldPoint(Min(WPoint.X, MinCoordinate.X), Min(WPoint.Y, MinCoordinate.Y));
  MaxCoordinate:= WorldPoint(Max(WPoint.X, MaxCoordinate.X), Max(WPoint.Y, MaxCoordinate.Y));
end;

procedure TTPen.CreateFigure();
begin
  Figure:= TPen.Create;
end;

procedure TTPen.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPen.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TPen).AddPoint(WPoint);
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPen.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.CreateFigure();
begin
  Figure:= TLine.Create;
end;

procedure TTLine.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TLine).StartP:= WPoint;
  (Figure as TLine).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TLine).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLine.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TLine).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.CreateFigure();
begin
  Figure:= TPolyline.Create;
end;

procedure TTPolyline.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if (Button = mbRight) and ((Figure as TPolyline).GetLastLine <> Nil) then begin
    (Figure as TPolyline).GetLastLine().EndP:= WPoint;
    TFigure.AddFigure(Figure);
    CreateFigure();
    IsMouseDown:= False;
    Exit;
  end
  else if Button = mbRight then Exit;
  (Figure as TPolyline).AddLine();
  (Figure as TPolyline).GetLastLine().StartP:= WPoint;
  (Figure as TPolyline).GetLastLine().EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  if (Figure as TPolyline).GetLastLine <> Nil then
    (Figure as TPolyline).GetLastLine().EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTPolyline.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  IsMouseDown:= True;
  if Button = mbRight then IsMouseDown:= False;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.CreateFigure();
begin
  Figure:= TRectangle.Create;
end;

procedure TTRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).StartP:= WPoint;
  (Figure as TRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRectangle).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.CreateFigure();
begin
  Figure:= TRoundRectangle.Create;
end;

procedure TTRoundRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).StartP:= WPoint;
  (Figure as TRoundRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTRoundRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TRoundRectangle).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.CreateFigure();
begin
  Figure:= TEllipse.Create;
end;

procedure TTEllipse.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TEllipse).StartP:= WPoint;
  (Figure as TEllipse).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TEllipse).EndP:= WPoint;
  FindMinMaxCoordinate(WPoint);
end;

procedure TTEllipse.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  (Figure as TEllipse).EndP:= WPoint;
  TFigure.AddFigure(Figure);
  CreateFigure();
  FindMinMaxCoordinate(WPoint);
end;

procedure TTLoupe.OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint);
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
procedure TTLoupe.OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint);
begin

end;

procedure TTLoupe.OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint);
begin

end;

procedure TTHand.OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint);
begin
  TTHand.StartPos:= WPoint;
end;
procedure TTHand.OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint);
begin
  Delta.X+= (StartPos.X - WPoint.X) * Zoom;
  Delta.Y+= (StartPos.Y - WPoint.Y) * Zoom;
end;


procedure TTHand.OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint);
begin

end;

procedure TTRectangleLoupe.OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TRectangle.Create());
  (TFigure.GetLastFigure() as TFillFigure).BrushStyle:= bsClear;
  (TFigure.GetLastFigure() as TRectangle).StartP:= WPoint;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;
procedure TTRectangleLoupe.OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTRectangleLoupe.OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint);
var
  Rect: TRectangle;
begin
  if ButtonWasDown = mbRight then Exit;
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

end.

