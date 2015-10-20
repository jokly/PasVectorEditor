unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons;

type
  TFigure = Class(TObject)
    private
      FFigures: array of TFigure; static;
      FPenColor: TColor;
      FPenWidth: Integer;
    public
      constructor Create(PenColor: TColor; PenWidth: Integer);
      class procedure addFigure(figure: TFigure); static;
      class function getLastFigure(): TFigure; static;
      class procedure deleteLastFigure(); static;
      procedure Draw(Canvas: TCanvas); virtual; abstract;
  end;

  TPen = Class(TFigure)
    private
      FPoints: array of TPoint;
    public
      procedure addPoint(point: TPoint);
      procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TPolyline = Class(TFigure)
    private
      FLines: array of TLine;
    public
      procedure Draw(Canvas: TCanvas); override;
      procedure addLine();
      function getLastLine(): TLine;
  end;

  TRectangle = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRectangle = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

implementation

constructor TFigure.Create(PenColor: TColor; PenWidth: Integer);
begin
  FPenColor:= PenColor;
  FPenWidth:= PenWidth;
end;

class procedure TFigure.addFigure(figure: TFigure);
begin
  SetLength(TFigure.FFigures, Length(TFigure.FFigures) + 1);
  TFigure.FFigures[High(TFigure.FFigures)]:= figure;
end;

class function TFigure.getLastFigure(): TFigure;
begin
  if Length(FFigures) > 0 then
    Result:= FFigures[High(FFigures)]
  else
    Result:= nil;
end;

class procedure TFigure.deleteLastFigure();
begin
  if Length(FFigures) > 0 then
    SetLength(FFigures, Length(FFigures) - 1);
end;

procedure TPen.addPoint(point: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= point;
end;

procedure TPen.Draw(Canvas: TCanvas);
var
  point: TPoint;
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    MoveTo(FPoints[0]);
    for point in FPoints do
      LineTo(point);
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    MoveTo(startP);
    LineTo(endP);
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
      MoveTo(_line.startP);
      LineTo(_line.endP);
    end;
end;

procedure TPolyline.addLine();
begin
  SetLength(FLines, Length(FLines) + 1);
  FLines[High(FLines)]:= TLine.Create(FPenColor, FPenWidth);
end;

function TPolyline.getLastLine(): TLine;
begin
  if FLines = nil then Exit;
  if (Length(FLines) > 0) then
    Result:= FLines[High(FLines)]
  else
    Result:= nil;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    Rectangle(startP.x, startP.y, endP.x, endP.y);
  end;
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    RoundRect(startP.x, startP.y, endP.x, endP.y, 10, 10);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:= FPenColor;
    Pen.Width:= FPenWidth;
    Brush.Style:= bsClear;
    Ellipse(startP.x, startP.y, endP.x, endP.y);
  end;
end;

end.

