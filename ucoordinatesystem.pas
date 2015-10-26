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
  end;

implementation

class function TWordPoint.WordPoint(_X, _Y: Extended): TWordPoint;
var
  WPoint: TWordPoint;
begin
  WPoint.X:= _X;
  WPoint.Y:= _Y;
  Result:= WPoint;
end;

end.
