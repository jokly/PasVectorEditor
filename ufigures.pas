unit UFigures;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, Graphics, LCLIntf, LCLType, Math, UCoordinateSystem;

type

  { TFigure }

  TFigure = Class(TObject)
    private
      FPenColor: TColor;
      FPenWidth: Integer;
      FPenStyle: TPenStyle;
    public
      IsSelected: Boolean;
      class procedure AddFigure(Figure: TFigure);
      class function GetLastFigure(): TFigure;
      class procedure DeleteLastFigure();
      procedure SetPenColor(Color: TColor);
      procedure Draw(Canvas: TCanvas); virtual;
      procedure DrawSelection(Canvas: TCanvas); virtual;
      function IsInside(ARect: TRect): Boolean; virtual; abstract;
    published
      property PenWidth: Integer read FPenWidth write FPenWidth default 1;
      property PenStyle: TPenStyle read FPenStyle write FPenStyle default psSolid;
  end;

  { TPen }

  TPen = Class(TFigure)
    private
      FPoints: array of TWorldPoint;
    protected
      MinP, MaxP: TWorldPoint;
    public
      procedure AddPoint(Point: TWorldPoint);
      function IsInside(ARect: TRect): Boolean; override;
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
  end;

  { TLine }

  TLine = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      MinP, MaxP: TWorldPoint;
      function IsInside(ARect: TRect): Boolean; override;
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
  end;

  { TPolyline }

  TPolyline = Class(TFigure)
    private
      FLines: array of TLine;
    protected
      MinP, MaxP: TWorldPoint;
    public
      procedure AddLine(ALine: TLine);
      function GetLastLine(): TLine;
      function IsInside(ARect: TRect): Boolean; override;
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
  end;

  { TFillFigure }

  TFillFigure = Class(TFigure)
    private
      FBrushColor: TColor;
      FBrushStyle: TBrushStyle;
    public
      StartP, EndP: TWorldPoint;
      procedure SetBrushColor(Color: TColor);
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
    published
      property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle default bsClear;
  end;

  { TRectangle }

  TRectangle = Class(TFillFigure)
    public
      procedure Draw(Canvas: TCanvas); override;
      function IsInside(ARect: TRect): Boolean; override;
  end;

  { TRoundRectangle }

  TRoundRectangle = Class(TFillFigure)
    private
      FRoundingX: Integer;
      FRoundingY: Integer;
    public
      procedure Draw(Canvas: TCanvas); override;
      function IsInside(ARect: TRect): Boolean; override;
    published
      property RoundingX: Integer
        read FRoundingX
        write FRoundingX
        default 20;
      property RoundingY: Integer
        read FRoundingY
        write FRoundingY
        default 20;
  end;

  { TEllipse }

  TEllipse = Class(TFillFigure)
    public
      procedure Draw(Canvas: TCanvas); override;
      function IsInside(ARect: TRect): Boolean; override;
  end;

  var
    Figures: array of TFigure;

implementation

class procedure TFigure.AddFigure(Figure: TFigure);
begin
  if Figure = Nil then Exit;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)]:= Figure;
end;

class function TFigure.GetLastFigure(): TFigure;
begin
  if Length(Figures) > 0 then
    Result:= Figures[High(Figures)]
  else
    Result:= Nil;
end;

class procedure TFigure.DeleteLastFigure();
begin
  if Length(Figures) > 0 then
    SetLength(Figures, Length(Figures) - 1);
end;

procedure TFigure.SetPenColor(Color: TColor);
begin
  FPenColor:= Color;
end;

procedure TFigure.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Width:= PenWidth;
    Pen.Style:= PenStyle;
    Pen.Color:= FPenColor;
  end;
end;

procedure TFigure.DrawSelection(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Width:= 2;
    Pen.Color:= clBlue;
    Pen.Style:= psDash;
    Brush.Style:= bsClear;
  end;
end;

procedure TPen.AddPoint(Point: TWorldPoint);
begin
  if Length(FPoints) > 0 then begin
    MinP:= WorldPoint(Min(Point.X, MinP.X), Min(Point.Y, MinP.Y));
    MaxP:= WorldPoint(Max(Point.X, MaxP.X), Max(Point.Y, MaxP.Y));
  end
  else begin
    MinP:= Point;
    MaxP:= Point;
  end;
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= Point;
end;

function TPen.IsInside(ARect: TRect): Boolean;
var
  FigurePos: array of TPoint;
  Region: HRGN;
  i: Integer;
begin
  Result:= False;
  SetLength(FigurePos, Length(FPoints));
  for i:= 0 to High(FPoints) do
    FigurePos[i]:= ToScreenPoint(FPoints[i]);
  Region:= CreatePolygonRgn(@FigurePos[0], Length(FigurePos), WINDING);
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

procedure TPen.Draw(Canvas: TCanvas);
var
  ScPoints: array of TPoint;
  i: Integer;
begin
  if IsSelected then
    DrawSelection(Canvas);
  Inherited;
  with Canvas do begin
    SetLength(ScPoints, Length(FPoints));
    for i:= 0 to High(FPoints) do
      ScPoints[i]:= ToScreenPoint(FPoints[i]);
    Polyline(ScPoints);
  end;
end;

procedure TPen.DrawSelection(Canvas: TCanvas);
begin
  Inherited;
  Canvas.Rectangle(ToScreenPoint(MinP).x - 5, ToScreenPoint(MinP).y - 5,
                   ToScreenPoint(MaxP).x + 5, ToScreenPoint(MaxP).y + 5);
end;

function TLine.IsInside(ARect: TRect): Boolean;
var
  FigurePos: array of TPoint;
  Region: HRGN;
begin
  Result:= False;
  ARect:= Rect(ARect.Left - 5, ARect.Top - 5, ARect.Right + 5, ARect.Bottom + 5);
  SetLength(FigurePos, 4);
  FigurePos[0]:= Point(round(StartP.X - 1), round(StartP.Y + 1));
  FigurePos[1]:= Point(round(StartP.X + 1), round(StartP.Y - 1));
  FigurePos[2]:= Point(round(EndP.x - 1), round(EndP.y + 1));
  FigurePos[3]:= Point(round(EndP.x + 1), round(EndP.y - 1));
  Region:= CreatePolygonRgn(@FigurePos[0], Length(FigurePos), WINDING);
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  if IsSelected then
    DrawSelection(Canvas);
  Inherited;
  with Canvas do begin
    MoveTo(ToScreenPoint(StartP));
    LineTo(ToScreenPoint(EndP));
  end;
end;

procedure TLine.DrawSelection(Canvas: TCanvas);
begin
  Inherited;
  Canvas.Rectangle(ToScreenPoint(MinP).x - 5, ToScreenPoint(MinP).y - 5,
                   ToScreenPoint(MaxP).x + 5, ToScreenPoint(MaxP).y + 5);
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  _line: TLine;
begin
  if IsSelected then
    DrawSelection(Canvas);
  Inherited;
  for _line in FLines do begin
    with Canvas do begin
      MoveTo(ToScreenPoint(_line.StartP));
      LineTo(ToScreenPoint(_line.EndP));
    end;
  end;
end;

procedure TPolyline.DrawSelection(Canvas: TCanvas);
begin
  Inherited;
  Canvas.Rectangle(ToScreenPoint(MinP).x - 5, ToScreenPoint(MinP).y - 5,
                   ToScreenPoint(MaxP).x + 5, ToScreenPoint(MaxP).y + 5);
end;

procedure TPolyline.AddLine(ALine: TLine);
begin
  if Length(FLines) > 0 then begin
    MinP:= WorldPoint(Min(ALine.StartP.X, MinP.X), Min(ALine.StartP.Y, MinP.Y));
    MaxP:= WorldPoint(Max(ALine.StartP.X, MaxP.X), Max(ALine.StartP.Y, MaxP.Y));
  end
  else begin
    MinP:= WorldPoint(Min(ALine.StartP.X, ALine.EndP.X), Min(ALine.StartP.Y, ALine.EndP.Y));
    MaxP:= WorldPoint(Max(ALine.StartP.X, ALine.EndP.X), Max(ALine.StartP.Y, ALine.EndP.Y));
  end;
  SetLength(FLines, Length(FLines) + 1);
  FLines[High(FLines)]:= ALine;
end;

function TPolyline.GetLastLine(): TLine;
begin
  if FLines = Nil then Exit(nil);
  if (Length(FLines) > 0) then
    Result:= FLines[High(FLines)]
  else
    Result:= Nil;
end;

function TPolyline.IsInside(ARect: TRect): Boolean;
var
  i: Integer;
begin
  Result:= False;
  for i:= 0 to High(FLines) do
    if FLines[i].IsInside(ARect) then begin
      Result:= True;
      Break;
    end;
end;

procedure TFillFigure.SetBrushColor(Color: TColor);
begin
  FBrushColor:= Color;
end;

procedure TFillFigure.Draw(Canvas: TCanvas);
begin
  if IsSelected then
    DrawSelection(Canvas);
  Inherited;
end;

procedure TFillFigure.DrawSelection(Canvas: TCanvas);
var
  MinP, MaxP: TWorldPoint;
begin
  Inherited;
  MinP:= WorldPoint(Min(StartP.X, EndP.X), Min(StartP.Y, EndP.Y));
  MaxP:= WorldPoint(Max(StartP.X, EndP.X), Max(StartP.Y, EndP.Y));
  Canvas.Rectangle(ToScreenPoint(MinP).x - 5, ToScreenPoint(MinP).y - 5,
                   ToScreenPoint(MaxP).x + 5, ToScreenPoint(MaxP).y + 5);
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Inherited;
  with Canvas do begin
    Brush.Color:= FBrushColor;
    Brush.Style:= BrushStyle;
    Rectangle(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
              ToScreenPoint(EndP).x, ToScreenPoint(EndP).y);
  end;
end;

function TRectangle.IsInside(ARect: TRect): Boolean;
var
  Region: HRGN;
begin
  Result:= False;
  Region:= CreateRectRgnIndirect(Rect(round(StartP.X), round(StartP.Y), round(EndP.X), round(EndP.Y)));
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  Inherited;
  with Canvas do begin
    Brush.Color:= FBrushColor;
    Brush.Style:= BrushStyle;
    RoundRect(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
              ToScreenPoint(EndP).x, ToScreenPoint(EndP).y,
              RoundingX, RoundingY);
  end;
end;

function TRoundRectangle.IsInside(ARect: TRect): Boolean;
var
  Region: HRGN;
begin
  Result:= False;
  Region:= CreateRoundRectRgn(
           round(StartP.X), round(StartP.Y), round(EndP.X), round(EndP.Y),
           RoundingX, RoundingY);
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Inherited;
  with Canvas do begin
    Brush.Color:= FBrushColor;
    Brush.Style:= BrushStyle;
    Ellipse(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
            ToScreenPoint(EndP).x, ToScreenPoint(EndP).y);
  end;
end;

function TEllipse.IsInside(ARect: TRect): Boolean;
var
  Region: HRGN;
begin
  Result:= False;
  Region:= CreateEllipticRgnIndirect(
           Rect(round(StartP.X), round(StartP.Y), round(EndP.X), round(EndP.Y)));
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

end.

