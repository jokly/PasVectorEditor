unit UCoordinateSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWorldPoint = record
      X, Y: Double;
  end;

function WorldPoint(X, Y: Double): TWorldPoint;
function ToWorldPoint(X, Y: Double): TWorldPoint;
function WorldPoint(Point: TPoint): TWorldPoint;
function ToScreenPoint(WordPoint: TWorldPoint): TPoint;
procedure CalculateBounds(WPoint: TWorldPoint);

var
  Zoom: Double;
  Delta: TWorldPoint;
  MinBounds, MaxBounds: TWorldPoint;
  SizeOfWindow: TPoint;

implementation

const
  Bound = 60;
  Addition = 200;

function WorldPoint(X, Y: Double): TWorldPoint;
begin
  Result.X:= X;
  Result.Y:= Y;
end;

function ToWorldPoint(X, Y: Double): TWorldPoint;
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

procedure CalculateBounds(WPoint: TWorldPoint);
begin
  if WPoint.X > (MaxBounds.X - Bound) then
    MaxBounds.X+= Addition
  else if WPoint.X < (MinBounds.X + Bound) then
    MinBounds.X-= Addition;
  if WPoint.Y > (MaxBounds.Y - Bound) then
    MaxBounds.Y+= Addition
  else if WPoint.Y < (MinBounds.y + Bound) then
    MinBounds.Y-= Addition;
end;

initialization
Zoom:= 1;
Delta.X:= 0;
Delta.Y:= 0;

end.
