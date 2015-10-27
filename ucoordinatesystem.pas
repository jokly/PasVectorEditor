unit UCoordinateSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWorldPoint = Class(TObject)
    public
      X, Y: Extended;
      class function WorldPoint(_X, _Y: Extended): TWorldPoint; static;
      class function ToScreenPoint(_WordPoint: TWorldPoint): TPoint; static;
      class function ToWorldPoint(_Point: TPoint): TWorldPoint; static;
  end;

  var
    Zoom, Dx, Dy: Extended;

implementation

class function TWorldPoint.WorldPoint(_X, _Y: Extended): TWorldPoint;
var
  WPoint: TWorldPoint;
begin
  WPoint:= (newinstance as TWorldPoint);
  WPoint.X:= _X * 100 / Zoom;
  WPoint.Y:= _Y * 100 / Zoom;
  Result:= WPoint;
end;

class function TWorldPoint.ToScreenPoint(_WordPoint: TWorldPoint): TPoint;
begin
  Result:= Point(Round(_WordPoint.X * Zoom / 100), Round(_WordPoint.Y * Zoom / 100));
end;

class function TWorldPoint.ToWorldPoint(_Point: TPoint): TWorldPoint;
var
  WPoint: TWorldPoint;
begin
  WPoint:= (newinstance as TWorldPoint);
  WPoint.X:= _Point.X * 100 / Zoom;
  WPoint.Y:= _Point.Y * 100 / Zoom;
  Result:= WPoint;
end;

initialization
Zoom:= 100;
Dx:= 0;
Dy:= 0;

end.
