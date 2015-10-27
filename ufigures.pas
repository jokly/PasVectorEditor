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
      FPoints: array of TWordPoint;
    public
      procedure AddPoint(Point: TPoint);
      procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = Class(TFigure)
    public
      StartP, EndP: TWordPoint;
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
      StartP, EndP: TWordPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRectangle = Class(TFigure)
    public
      StartP, EndP: TWordPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = Class(TFigure)
    public
      StartP, EndP: TWordPoint;
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

procedure TPen.AddPoint(Point: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= TWordPoint.ToWordPoint(Point);
end;

procedure TPen.Draw(Canvas: TCanvas);
var
  Point: TWordPoint;
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    if Length(FPoints) > 0 then
      //MoveTo(TWordPoint.ToTPoint(FPoints[0]));
      MoveTo(TWordPoint.ToTPoint(FPoints[0]).x + Round(TWordPoint.ToTPoint(FPoints[0]).x / 100) * Round(Zoom),
      TWordPoint.ToTPoint(FPoints[0]).y + Round(TWordPoint.ToTPoint(FPoints[0]).y / 100) * Round(Zoom));
    for Point in FPoints do
      //LineTo(TWordPoint.ToTPoint(Point));
      LineTo(TWordPoint.ToTPoint(Point).x + Round(TWordPoint.ToTPoint(Point).x / 100) * Round(Zoom),
      TWordPoint.ToTPoint(Point).y + Round(TWordPoint.ToTPoint(Point).y / 100) * Round(Zoom));
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    MoveTo(TWordPoint.ToTPoint(StartP));
    LineTo(TWordPoint.ToTPoint(EndP));
  end;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  _line: TLine;
begin
  Canvas.Pen.Color:= FPenColor;
  Canvas.Pen.Width:= FPenWidth;
  for _line in FLines do
    with Canvas do begin
      MoveTo(TWordPoint.ToTPoint(_line.StartP));
      LineTo(TWordPoint.ToTPoint(_line.EndP));
    end;
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
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    Rectangle(Round(StartP.x), Round(StartP.y), Round(EndP.x), Round(EndP.y));
  end;
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    RoundRect(Round(StartP.x), Round(StartP.y), Round(EndP.x), Round(EndP.y),
              RoundingOfRoundRect, RoundingOfRoundRect);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    Ellipse(Round(StartP.x), Round(StartP.y), Round(EndP.x), Round(EndP.y));
  end;
end;

end.

