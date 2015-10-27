unit UCoordinateSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWordPoint = Class(TObject)
    public
      X, Y: Extended;
      class function WordPoint(_X, _Y: Extended): TWordPoint; static;
      class function ToTPoint(_WordPoint: TWordPoint): TPoint; static;
      class function ToWordPoint(_Point: TPoint): TWordPoint; static;
  end;

  var
    Zoom, Dx, Dy: Extended;

implementation

class function TWordPoint.WordPoint(_X, _Y: Extended): TWordPoint;
var
  WPoint: TWordPoint;
begin
  WPoint:= (newinstance as TWordPoint);
  WPoint.X:= _X;
  WPoint.Y:= _Y;
  Result:= WPoint;
end;

class function TWordPoint.ToTPoint(_WordPoint: TWordPoint): TPoint;
begin
  Result:= Point(Round(_WordPoint.X), Round(_WordPoint.Y));
end;

class function TWordPoint.ToWordPoint(_Point: TPoint): TWordPoint;
var
  WPoint: TWordPoint;
begin
  WPoint:= (newinstance as TWordPoint);
  WPoint.X:= _Point.X + Dx;
  WPoint.Y:= _Point.Y + Dy;
  Result:= WPoint;
end;

initialization
Zoom:= 1;
Dx:= 0;
Dy:= 0;

end.
