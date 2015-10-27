unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, UCoordinateSystem;

type

  TFigure = Class(TObject)
    private
      FPenColor: TColor;
      FPenWidth: Integer;
    public
      constructor Create(PenColor: TColor; PenWidth: Integer);
      class procedure AddFigure(Figure: TFigure); static;
      class function GetLastFigure(): TFigure; static;
      class procedure DeleteLastFigure(); static;
      procedure Draw(Canvas: TCanvas); virtual; abstract;
  end;

  TPen = Class(TFigure)
    private
      FPoints: array of TWorldPoint;
    public
      procedure AddPoint(Point: TWorldPoint);
      procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TPolyline = Class(TFigure)
    private
      FLines: array of TLine;
    public
      procedure Draw(Canvas: TCanvas); override;
      procedure AddLine();
      function GetLastLine(): TLine;
  end;

  TRectangle = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRectangle = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  var
    FFigures: array of TFigure;

implementation
const
  RoundingOfRoundRect = 20;

constructor TFigure.Create(PenColor: TColor; PenWidth: Integer);
begin
  FPenColor:= PenColor;
  FPenWidth:= PenWidth;
end;

class procedure TFigure.AddFigure(Figure: TFigure);
begin
  SetLength(FFigures, Length(FFigures) + 1);
  FFigures[High(FFigures)]:= Figure;
end;

class function TFigure.GetLastFigure(): TFigure;
begin
  if Length(FFigures) > 0 then
    Result:= FFigures[High(FFigures)]
  else
    Result:= Nil;
end;

class procedure TFigure.DeleteLastFigure();
begin
  if Length(FFigures) > 0 then
    SetLength(FFigures, Length(FFigures) - 1);
end;

procedure TPen.AddPoint(Point: TWorldPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= Point;
end;

procedure TPen.Draw(Canvas: TCanvas);
var
  Point: TWorldPoint;
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    if Length(FPoints) > 0 then
      MoveTo(TWorldPoint.ToScreenPoint(FPoints[0]));
    for Point in FPoints do
      LineTo(TWorldPoint.ToScreenPoint(Point));
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  if (StartP = Nil) or (EndP = Nil) then Exit;
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    MoveTo(TWorldPoint.ToScreenPoint(StartP));
    LineTo(TWorldPoint.ToScreenPoint(EndP));
  end;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  _line: TLine;
begin
  Canvas.Pen.Color:= FPenColor;
  Canvas.Pen.Width:= FPenWidth;
  for _line in FLines do
    _line.Draw(Canvas);
end;

procedure TPolyline.AddLine();
begin
  SetLength(FLines, Length(FLines) + 1);
  FLines[High(FLines)]:= TLine.Create(FPenColor, FPenWidth);
end;

function TPolyline.GetLastLine(): TLine;
begin
  if FLines = Nil then Exit;
  if (Length(FLines) > 0) then
    Result:= FLines[High(FLines)]
  else
    Result:= Nil;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  if (StartP = Nil) or (EndP = Nil) then Exit;
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    Rectangle(TWorldPoint.ToScreenPoint(StartP).x, TWorldPoint.ToScreenPoint(StartP).y,
              TWorldPoint.ToScreenPoint(EndP).x, TWorldPoint.ToScreenPoint(EndP).y);
  end;
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  if (StartP = Nil) or (EndP = Nil) then Exit;
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    RoundRect(TWorldPoint.ToScreenPoint(StartP).x, TWorldPoint.ToScreenPoint(StartP).y,
              TWorldPoint.ToScreenPoint(EndP).x, TWorldPoint.ToScreenPoint(EndP).y,
              RoundingOfRoundRect, RoundingOfRoundRect);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  if (StartP = Nil) or (EndP = Nil) then Exit;
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    Ellipse(TWorldPoint.ToScreenPoint(StartP).x, TWorldPoint.ToScreenPoint(StartP).y,
              TWorldPoint.ToScreenPoint(EndP).x, TWorldPoint.ToScreenPoint(EndP).y);
  end;
end;

end.

