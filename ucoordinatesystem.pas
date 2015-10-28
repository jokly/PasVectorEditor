unit UCoordinateSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWorldPoint = Class(TObject)
    public
      X, Y: Extended;
      class function Create(_X, _Y: Extended): TWorldPoint; overload;
      class function WorldPoint(_X, _Y: Extended): TWorldPoint; static;
      class function ToScreenPoint(_WordPoint: TWorldPoint): TPoint; static;
      class function ToWorldPoint(_Point: TPoint): TWorldPoint; static;
  end;

  var
    Zoom, Dx, Dy: Extended;
    LeftOfCanvas, RightOfCanvas, TopOfCanvas, BottomOfCanvas: Extended;
    WindowPos: TWorldPoint;
    WidthOfWindow, HeightOfWindow: Integer;

implementation

class function TWorldPoint.Create(_X, _Y: Extended): TWorldPoint;
var
  WPoint: TWorldPoint;
begin
  WPoint:= (newinstance as TWorldPoint);
  WPoint.X:= _X;
  WPoint.Y:= _Y;
  Result:= WPoint;
end;

class function TWorldPoint.WorldPoint(_X, _Y: Extended): TWorldPoint;
var
  WPoint: TWorldPoint;
begin
  WPoint:= (newinstance as TWorldPoint);
  WPoint.X:= (_X + Dx) * 100 / Zoom;
  WPoint.Y:= (_Y + Dy) * 100 / Zoom;
  Result:= WPoint;
end;

class function TWorldPoint.ToScreenPoint(_WordPoint: TWorldPoint): TPoint;
begin
  Result:= Point(Round(_WordPoint.X * Zoom / 100 - Dx), Round(_WordPoint.Y * Zoom / 100 - Dy));
end;

class function TWorldPoint.ToWorldPoint(_Point: TPoint): TWorldPoint;
var
  WPoint: TWorldPoint;
begin
  WPoint:= (newinstance as TWorldPoint);
  WPoint.X:= (_Point.X + Dx) * 100 / Zoom;
  WPoint.Y:= (_Point.Y + Dy) * 100 / Zoom;
  Result:= WPoint;
end;

initialization
Zoom:= 100;
Dx:= 0;
Dy:= 0;

end.
