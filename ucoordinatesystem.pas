unit UCoordinateSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWorldPoint = record
      X, Y: Double;
  end;

function CreateWorldPoint(X, Y: Extended): TWorldPoint;
function WorldPoint(X, Y: Extended): TWorldPoint;
function WorldPoint(Point: TPoint): TWorldPoint;
function ToScreenPoint(WordPoint: TWorldPoint): TPoint;

var
  Zoom: Double;
  Delta: TWorldPoint;
  MinBounds, MaxBounds: TWorldPoint;
  SizeOfWindow: TPoint;

implementation

function CreateWorldPoint(X, Y: Extended): TWorldPoint;
begin
  Result.X:= X;
  Result.Y:= Y;
end;

function WorldPoint(X, Y: Extended): TWorldPoint;
begin
  Result.X:= (X + Delta.X) * 1 / Zoom;
  Result.Y:= (Y + Delta.Y) * 1 / Zoom;
end;

function WorldPoint(Point: TPoint): TWorldPoint;
begin
  Result.X:= (Point.X + Delta.X) * 1 / Zoom;
  Result.Y:= (Point.Y + Delta.Y) * 1 / Zoom;
end;

function ToScreenPoint(WordPoint: TWorldPoint): TPoint;
begin
  Result:= Point(Round(WordPoint.X * Zoom - Delta.X), Round(WordPoint.Y * Zoom - Delta.Y));
end;

initialization
Zoom:= 1;
Delta.X:= 0;
Delta.Y:= 0;

end.
