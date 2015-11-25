unit UFigures;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, Graphics, LCLIntf, LCLType, UCoordinateSystem;

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
      function IsInside(ARect: TRect): Boolean; virtual; abstract;
    published
      property PenWidth: Integer read FPenWidth write FPenWidth default 1;
      property PenStyle: TPenStyle read FPenStyle write FPenStyle default psSolid;
  end;

  { TPen }

  TPen = Class(TFigure)
    private
      FPoints: array of TWorldPoint;
    public
      procedure AddPoint(Point: TWorldPoint);
      function IsInside(ARect: TRect): Boolean; override;
      procedure Draw(Canvas: TCanvas); override;
  end;

  { TLine }

  TLine = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      function IsInside(ARect: TRect): Boolean; override;
      procedure Draw(Canvas: TCanvas); override;
  end;

  { TPolyline }

  TPolyline = Class(TFigure)
    private
      FLines: array of TLine;
    public
      procedure AddLine();
      function GetLastLine(): TLine;
      function IsInside(ARect: TRect): Boolean; override;
      procedure Draw(Canvas: TCanvas); override;
  end;

  { TFillFigure }

  TFillFigure = Class(TFigure)
    private
      FBrushColor: TColor;
      FBrushStyle: TBrushStyle;
    public
      StartP, EndP: TWorldPoint;
      procedure SetBrushColor(Color: TColor);
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
    if IsSelected then
      Pen.Color:= clRed
    else
      Pen.Color:= FPenColor;
    Pen.Width:= PenWidth;
    Pen.Style:= PenStyle;
  end;
end;

procedure TPen.AddPoint(Point: TWorldPoint);
begin
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
  Inherited;
  with Canvas do begin
    SetLength(ScPoints, Length(FPoints));
    for i:= 0 to High(FPoints) do
      ScPoints[i]:= ToScreenPoint(FPoints[i]);
    Polyline(ScPoints);
  end;
end;

function TLine.IsInside(ARect: TRect): Boolean;
var
  FigurePos: array of TPoint;
  Region: HRGN;
  i: Integer;
begin
  Result:= False;
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
var
  SPoint, EPoint: TPoint;
begin
  SPoint:= ToScreenPoint(StartP);
  EPoint:= ToScreenPoint(EndP);
  if (SPoint.X = EPoint.X) and (SPoint.Y = EPoint.Y) then Exit;
  Inherited;
  with Canvas do begin
    MoveTo(SPoint);
    LineTo(EPoint);
  end;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  _line: TLine;
begin
  Inherited;
  for _line in FLines do begin
    with Canvas do begin
      MoveTo(ToScreenPoint(_line.StartP));
      LineTo(ToScreenPoint(_line.EndP));
    end;
  end;
end;

procedure TPolyline.AddLine();
begin
  SetLength(FLines, Length(FLines) + 1);
  FLines[High(FLines)]:= TLine.Create();
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
  MouseRect: TRect;
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
  MouseRect: TRect;
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
  MouseRect: TRect;
  Region: HRGN;
begin
  Result:= False;
  Region:= CreateEllipticRgnIndirect(
           Rect(round(StartP.X), round(StartP.Y), round(EndP.X), round(EndP.Y)));
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

end.

