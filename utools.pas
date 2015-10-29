unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, Math, UFigures, UCoordinateSystem;

type

  TTool = Class(TObject)
    public
      Tools: array of TTool; static;
      ButtonOnForm: TBitBtn;
      ImageOfButton: TBitmap;
      class procedure AddTool(Tool: TTool); static;
      constructor Create(PathToFile: String); overload;
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); virtual; abstract;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); virtual; abstract;
  end;

  TTPaint = Class(TTool)
    protected
      FPenColor: TColor; static;
      FPenWidth: Integer; static;
    public
      class procedure SetPenColor(Color: TColor); static;
      class procedure SetPenWidth(Width: Integer); static;
  end;

  TTPen = Class(TTPaint)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTLine = Class(TTPaint)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTPolyline = Class(TTPaint)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRectangle = Class(TTPaint)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTRoundRectangle = Class(TTPaint)
    public
      procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
      procedure OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint); override;
      procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint); override;
  end;

  TTEllipse = Class(TTPaint)
    public
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
    SizeOfButton = 35;
    MinZoom = 1;
    MaxZoom = 20;

  var
    IsMouseDown: Boolean;

implementation

const
  ZoomOfLoupe = 0.2;

var
  IsMouseWasDown: Boolean;
  ButtonWasDown: TMouseButton;

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

class procedure TTPaint.SetPenColor(Color: TColor);
begin
  FPenColor:= Color;
end;

class procedure TTPaint.SetPenWidth(Width: Integer);
begin
  FPenWidth:= Width;
end;

procedure TTPen.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TPen.Create(FPenColor, FPenWidth));
end;

procedure TTPen.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TPen).AddPoint(WPoint);
end;

procedure TTPen.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin

end;

procedure TTLine.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TLine.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TLine).StartP:= WPoint;
end;

procedure TTLine.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TLine).EndP:= WPoint;
end;

procedure TTLine.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TLine).EndP:= WPoint;
end;

procedure TTPolyline.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbRight then begin
    (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= WPoint;
    IsMouseWasDown:= False;
    Exit;
  end;
  if IsMouseWasDown then
    (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= WPoint
  else
     IsMouseWasDown:= True;
  TFigure.AddFigure(TPolyline.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TPolyline).AddLine;
  (TFigure.GetLastFigure() as TPolyline).GetLastLine().StartP:= WPoint;
end;

procedure TTPolyline.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  (TFigure.GetLastFigure() as TPolyline).GetLastLine().EndP:= WPoint;
end;

procedure TTPolyline.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if IsMouseWasDown then IsMouseDown:= True;
end;

procedure TTRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TRectangle.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TRectangle).StartP:= WPoint;
end;

procedure TTRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRectangle).EndP:= WPoint;
end;

procedure TTRoundRectangle.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TRoundRectangle.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TRoundRectangle).StartP:= WPoint;
end;

procedure TTRoundRectangle.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRoundRectangle).EndP:= WPoint;
end;

procedure TTRoundRectangle.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TRoundRectangle).EndP:= WPoint;
end;

procedure TTEllipse.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbRight then begin
    ButtonWasDown:= mbRight;
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TEllipse.Create(FPenColor, FPenWidth));
  (TFigure.GetLastFigure() as TEllipse).StartP:= WPoint;
end;

procedure TTEllipse.OnMouseMove(Sender: TObject; Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TEllipse).EndP:= WPoint;
end;

procedure TTEllipse.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; WPoint: TWorldPoint);
begin
  if ButtonWasDown = mbRight then Exit;
  (TFigure.GetLastFigure() as TEllipse).EndP:= WPoint;
end;

procedure TTLoupe.OnMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; WPoint: TWorldPoint);
begin
  if Button = mbLeft then
    Zoom+= ZoomOfLoupe
  else if Button = mbRight then begin
    if Zoom - ZoomOfLoupe > 0 then
      Zoom-= ZoomOfLoupe;
  end;
  //Dx:= (WPoint.X - WidthOfWindow / 2) * Zoom / 100;
  //Dy:= (WPoint.Y - HeightOfWindow / 2) * Zoom / 100;
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
  StartPos:= WPoint;
end;
procedure TTHand.OnMouseMove(Sender: TObject; Shift: TShiftState;
        WPoint: TWorldPoint);
begin
  Delta.X+= StartPos.X - WPoint.X;
  Delta.Y+= StartPos.Y - WPoint.Y;
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
    TFigure.DeleteLastFigure();
    Exit;
  end
  else ButtonWasDown:= mbLeft;
  TFigure.AddFigure(TRectangle.Create(clBlack, 1));
  (TFigure.GetLastFigure() as TRectangle).StartP:= WPoint;
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
  if Min(SizeOfWindow.X / Abs(Rect.StartP.X - Rect.EndP.X),
         SizeOfWindow.Y / Abs(Rect.StartP.Y - Rect.EndP.Y)) * 100 < MaxZoom then
    Zoom:= Min(SizeOfWindow.X / Abs(Rect.StartP.X - Rect.EndP.X),
               SizeOfWindow.Y / Abs(Rect.StartP.Y - Rect.EndP.Y)) * 100;
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
TTool.AddTool(TTRectangleLoupe.Create('img\rectangle.bmp'));

end.

