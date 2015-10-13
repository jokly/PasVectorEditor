unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TFigure = Class(TObject)
    private
      figures: array of TFigure; static;
    public
      class procedure addFigure(figure: TFigure); static;
      class function getLastFigure(): TFigure; static;
      procedure Draw(PaintBox: TPaintBox); virtual; abstract;
  end;

  TPen = Class(TFigure)
    private
      points: array of TPoint;
    public
      procedure addPoint(point: TPoint);
      procedure Draw(PaintBox: TPaintBox); override;
  end;

  TLine = Class(TFigure)
    public
      startP, endP: TPoint;
      procedure Draw(PaintBox: TPaintBox); override;
  end;

  TPolyline = Class(TLine)

  end;

  TRectangle = Class(TFigure)

  end;

  TRoundRectangle = Class(TFigure)

  end;

  TEllipse = Class(TFigure)

  end;

implementation

class procedure TFigure.addFigure(figure: TFigure);
begin
  SetLength(TFigure.figures, Length(TFigure.figures) + 1);
  TFigure.figures[High(TFigure.figures)]:= figure;
end;

class function TFigure.getLastFigure(): TFigure;
begin
  result:= figures[High(figures)];
end;

procedure TPen.addPoint(point: TPoint);
begin
  SetLength(points, Length(points) + 1);
  points[High(points)]:= point;
end;

procedure TPen.Draw(PaintBox: TPaintBox);
var
  point: TPoint;
begin
  PaintBox.Canvas.MoveTo(points[0]);
  for point in points do
      PaintBox.Canvas.LineTo(point);
end;

procedure TLine.Draw(PaintBox: TPaintBox);
begin
  with PaintBox.Canvas do begin
    MoveTo(startP);
    LineTo(endP);
  end;
end;

end.

