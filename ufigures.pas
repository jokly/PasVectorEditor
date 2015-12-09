unit UFigures;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, Graphics, LCLIntf, LCLType, Math, UCoordinateSystem,
  Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, TypInfo, sysutils, Dialogs;

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
      procedure Depose(Offset: TWorldPoint); virtual; abstract;
      class function LoadFile(FileName: String): Boolean;
      class procedure LoadFigure(ANode: TDOMNode); virtual; abstract;
      procedure InitFigure(ANode: TDOMNode); virtual; // Устанавливает значения при загрузке
      class procedure SaveFile(FileName: String);
      procedure SetValuesOfFigures(ANode: TDOMNode); virtual; // Устанавливает значения при сохранении
      function SaveFigure(ADoc: TXMLDocument): TDOMNode; virtual; abstract;
    published
      property PenWidth: Integer read FPenWidth write FPenWidth;
      property PenStyle: TPenStyle read FPenStyle write FPenStyle;
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
      procedure Depose(Offset: TWorldPoint); override;
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
      class procedure LoadFigure(ANode: TDOMNode); override;
      function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
  end;

  { TLine }

  TLine = Class(TFigure)
    public
      StartP, EndP: TWorldPoint;
      MinP, MaxP: TWorldPoint;
      function IsInside(ARect: TRect): Boolean; override;
      procedure Depose(Offset: TWorldPoint); override;
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
      class procedure LoadFigure(ANode: TDOMNode); override;
      function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
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
      procedure Depose(Offset: TWorldPoint); override;
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
      class procedure LoadFigure(ANode: TDOMNode); override;
      function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
  end;

  { TFillFigure }

  TFillFigure = Class(TFigure)
    private
      FBrushColor: TColor;
      FBrushStyle: TBrushStyle;
    public
      StartP, EndP: TWorldPoint;
      procedure SetBrushColor(Color: TColor);
      procedure Depose(Offset: TWorldPoint); override;
      procedure Draw(Canvas: TCanvas); override;
      procedure DrawSelection(Canvas: TCanvas); override;
      procedure InitFigure(ANode: TDOMNode); override;
      procedure SetValuesOfFigures(ANode: TDOMNode); override;
    published
      property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
  end;

  { TRectangle }

  TRectangle = Class(TFillFigure)
    public
      procedure Draw(Canvas: TCanvas); override;
      function IsInside(ARect: TRect): Boolean; override;
      class procedure LoadFigure(ANode: TDOMNode); override;
      function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
  end;

  { TRoundRectangle }

  TRoundRectangle = Class(TFillFigure)
    private
      FRoundingX: Integer;
      FRoundingY: Integer;
    public
      procedure Draw(Canvas: TCanvas); override;
      function IsInside(ARect: TRect): Boolean; override;
      class procedure LoadFigure(ANode: TDOMNode); override;
      function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    published
      property RoundingX: Integer read FRoundingX write FRoundingX;
      property RoundingY: Integer read FRoundingY write FRoundingY;
  end;

  { TEllipse }

  TEllipse = Class(TFillFigure)
    public
      procedure Draw(Canvas: TCanvas); override;
      function IsInside(ARect: TRect): Boolean; override;
      class procedure LoadFigure(ANode: TDOMNode); override;
      function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
  end;

  var
    Figures: array of TFigure;

implementation

var
  ClassesFigures: array of TFigure;

procedure AddFigure(AFigure: TFigure);
begin
  SetLength(ClassesFigures, Length(ClassesFigures) + 1);
  ClassesFigures[High(ClassesFigures)]:= AFigure;
end;

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

class function TFigure.LoadFile(FileName: String): Boolean;
var
  Doc: TXMLDocument;
  FigNode: TDOMNode;
  i: Integer;
begin
  Result:= True;
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit(False);
  try
    ReadXMLFile(Doc, FileName);
    if Doc.DocumentElement.NodeName <> 'Figures' then
      Exit(False);
    SetLength(Figures, 0);
    FigNode:= Doc.DocumentElement.FirstChild;
    while FigNode <> Nil do begin
      for i:=0 to High(ClassesFigures) do
        if FigNode.NodeName = ClassesFigures[i].ClassName then
          ClassesFigures[i].LoadFigure(FigNode);
      FigNode:= FigNode.GetNextNodeSkipChildren;
    end;
  finally
    Doc.Free;
  end;
end;

class procedure TFigure.SaveFile(FileName: String);
var
  Doc: TXMLDocument;
  FiguresNode: TDOMNode;
  i: Integer;
begin
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit;
  try
    Doc:= TXMLDocument.Create;
    FiguresNode:= Doc.CreateElement('Figures');
    Doc.AppendChild(FiguresNode);
    FiguresNode:= Doc.DocumentElement;
    for i:= 0 to High(Figures) do
      FiguresNode.AppendChild(Figures[i].SaveFigure(Doc));
    WriteXML(Doc, FileName);
  finally
    Doc.Free;
  end;
end;

procedure TFigure.SetValuesOfFigures(ANode: TDOMNode);
begin
  TDOMElement(ANode).SetAttribute('PenWidth', IntToStr(PenWidth));
  TDOMElement(ANode).SetAttribute('PenColor', IntToStr(FPenColor));
  TDOMElement(ANode).SetAttribute('PenStyle',  GetEnumName(TypeInfo(TPenStyle), Integer(PenStyle)));
end;

procedure TFigure.InitFigure(ANode: TDOMNode);
begin
  SetPropValue(Self, 'PenWidth', ANode.Attributes.Item[0].NodeValue);
  SetPenColor(StrToInt(ANode.Attributes.Item[1].NodeValue));
  SetPropValue(Self, 'PenStyle', ANode.Attributes.Item[2].NodeValue);
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
  ARect:= Rect(ARect.Left - 5, ARect.Top - 5, ARect.Right + 5, ARect.Bottom + 5);
  SetLength(FigurePos, 2 * Length(FPoints) - 1);
  for i:= 0 to High(FPoints) do
    FigurePos[i]:= ToScreenPoint(FPoints[i]);
  for i:=High(FPoints) downto 0 do
    FigurePos[2 * Length(FPoints) - 1 - i]:= Point(ToScreenPoint(FPoints[i]).x + 1,
                         ToScreenPoint(FPoints[i]).y - 1);
  Region:= CreatePolygonRgn(@FigurePos[0], Length(FigurePos), ALTERNATE);
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

procedure TPen.Depose(Offset: TWorldPoint);
var
  i: Integer;
begin
  for i:= 0 to High(FPoints) do  begin
    FPoints[i].X+= Offset.X;
    FPoints[i].Y+= Offset.Y;
  end;
  MinP.X+= Offset.X; MinP.Y+= Offset.Y;
  MaxP.X+= Offset.X; MaxP.Y+= Offset.Y;
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
var
  Append: Integer;
begin
  Inherited;
  Append:= PenWidth div 2 + 5;
  Canvas.Rectangle(ToScreenPoint(MinP).x - Append, ToScreenPoint(MinP).y - Append,
                   ToScreenPoint(MaxP).x + Append, ToScreenPoint(MaxP).y + Append);
end;

class procedure TPen.LoadFigure(ANode: TDOMNode);
var
  Pen: TPen;
  i: Integer;
  PNode: TDOMNode;
begin
  SetLength(Figures, Length(Figures) + 1);
  Pen:= TPen.Create;
  Pen.InitFigure(ANode);
  PNode:= ANode;
  for i:= 1 to ANode.GetChildCount do begin
    PNode:= PNode.GetNextNode;
    Pen.AddPoint(WorldPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
                            StrToFloat(PNode.Attributes.Item[1].NodeValue)));
  end;
  Figures[High(Figures)]:= Pen;
end;

function TPen.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: Integer;
begin
  Result:= ADoc.CreateElement('TPen');
  Self.SetValuesOfFigures(Result);
  for i:= 0 to High(FPoints) do begin
    PNode:= ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(FPoints[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(FPoints[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TLine.IsInside(ARect: TRect): Boolean;
var
  FigurePos: array of TPoint;
  Region: HRGN;
begin
  Result:= False;
  ARect:= Rect(ARect.Left - 5, ARect.Top - 5, ARect.Right + 5, ARect.Bottom + 5);
  SetLength(FigurePos, 4);
  FigurePos[0]:= Point(ToScreenPoint(StartP).x + 1, ToScreenPoint(StartP).y + 1);
  FigurePos[1]:= Point(ToScreenPoint(StartP).x + 1, ToScreenPoint(StartP).y - 1);
  FigurePos[2]:= Point(ToScreenPoint(EndP).x - 1, ToScreenPoint(EndP).y + 1);
  FigurePos[3]:= Point(ToScreenPoint(EndP).x + 1, ToScreenPoint(EndP).y - 1);
  Region:= CreatePolygonRgn(@FigurePos[0], Length(FigurePos), WINDING);
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

procedure TLine.Depose(Offset: TWorldPoint);
begin
  StartP.X+= Offset.X; StartP.Y+= Offset.Y;
  EndP.X+= Offset.X; EndP.Y+= Offset.Y;
  MinP.X+= Offset.X; MinP.Y+= Offset.Y;
  MaxP.X+= Offset.X; MaxP.Y+= Offset.Y;
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
var
  Append: Integer;
begin
  Inherited;
  Append:= PenWidth div 2 + 5;
  Canvas.Rectangle(ToScreenPoint(MinP).x - Append, ToScreenPoint(MinP).y - Append,
                   ToScreenPoint(MaxP).x + Append, ToScreenPoint(MaxP).y + Append);
end;

class procedure TLine.LoadFigure(ANode: TDOMNode);
var
  Line: TLine;
begin
  SetLength(Figures, Length(Figures) + 1);
  Line:= TLine.Create;
  Line.InitFigure(ANode);
  ANode:= ANode.GetNextNode;
  Line.StartP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                           StrToFloat(ANode.Attributes.Item[1].NodeValue));
  ANode:= ANode.GetNextNode;
  Line.EndP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                         StrToFloat(ANode.Attributes.Item[1].NodeValue));
  Figures[High(Figures)]:= Line;
end;

function TLine.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
begin
  Result:= ADoc.CreateElement('TLine');
  Self.SetValuesOfFigures(Result);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(StartP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(StartP.Y));
  Result.AppendChild(PNode);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(EndP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(EndP.Y));
  Result.AppendChild(PNode);
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
var
  Append: Integer;
begin
  Inherited;
  Append:= PenWidth div 2 + 5;
  Canvas.Rectangle(ToScreenPoint(MinP).x - Append, ToScreenPoint(MinP).y - Append,
                   ToScreenPoint(MaxP).x + Append, ToScreenPoint(MaxP).y + Append);
end;

class procedure TPolyline.LoadFigure(ANode: TDOMNode);
var
  SPNode, EPNode: TDOMNode;
  Polyline: TPolyline;
  Line: TLine;
  i: Integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Polyline:= TPolyline.Create;
  Polyline.InitFigure(ANode);
  SPNode:= ANode.GetNextNode;
  EPNode:= SPNode.GetNextNode;
  for i:= 1 to ANode.GetChildCount do begin
    Line:= TLine.Create;
    Line.StartP:=WorldPoint(StrToFloat(SPNode.Attributes.Item[0].NodeValue),
                            StrToFloat(SPNode.Attributes.Item[1].NodeValue));
    Line.EndP:=WorldPoint(StrToFloat(EPNode.Attributes.Item[0].NodeValue),
                          StrToFloat(EPNode.Attributes.Item[1].NodeValue));
    EPNode:= SPNode.CloneNode(True);
    SPNode:= SPNode.GetNextNode;
    Polyline.AddLine(Line);
  end;
  Figures[High(Figures)]:= Polyline;
end;

function TPolyline.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: Integer;
begin
  Result:= ADoc.CreateElement('TPolyline');
  Self.SetValuesOfFigures(Result);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(FLines[0].StartP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(FLines[0].StartP.Y));
  Result.AppendChild(PNode);
  for i:= 0 to High(FLines) do begin
    PNode:= ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(FLines[i].EndP.X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(FLines[i].EndP.Y));
    Result.AppendChild(PNode);
  end;
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

procedure TPolyline.Depose(Offset: TWorldPoint);
var
  i: Integer;
begin
  for i:= 0 to High(FLines) do
    FLines[i].Depose(Offset);
  MinP.X+= Offset.X; MinP.Y+= Offset.Y;
  MaxP.X+= Offset.X; MaxP.Y+= Offset.Y;
end;

procedure TFillFigure.SetBrushColor(Color: TColor);
begin
  FBrushColor:= Color;
end;

procedure TFillFigure.Depose(Offset: TWorldPoint);
begin
  StartP.X+= Offset.X;
  StartP.Y+= Offset.Y;
  EndP.X+= Offset.X;
  EndP.Y+= Offset.Y;
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
  Append: Integer;
begin
  Inherited;
  MinP:= WorldPoint(Min(StartP.X, EndP.X), Min(StartP.Y, EndP.Y));
  MaxP:= WorldPoint(Max(StartP.X, EndP.X), Max(StartP.Y, EndP.Y));
  Append:= PenWidth div 2 + 5;
  Canvas.Rectangle(ToScreenPoint(MinP).x - Append, ToScreenPoint(MinP).y - Append,
                   ToScreenPoint(MaxP).x + Append, ToScreenPoint(MaxP).y + Append);
end;

procedure TFillFigure.InitFigure(ANode: TDOMNode);
begin
  inherited InitFigure(ANode);
  SetBrushColor(StrToInt(ANode.Attributes.Item[3].NodeValue));
  SetPropValue(Self, 'BrushStyle', ANode.Attributes.Item[4].NodeValue);
end;

procedure TFillFigure.SetValuesOfFigures(ANode: TDOMNode);
begin
  inherited SetValuesOfFigures(ANode);
  TDOMElement(ANode).SetAttribute('BrushColor', IntToStr(FBrushColor));
  TDOMElement(ANode).SetAttribute('BrushStyle',  GetEnumName(TypeInfo(TBrushStyle), Integer(BrushStyle)));
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
  Region:= CreateRectRgnIndirect(Rect(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
                                      ToScreenPoint(EndP).x, ToScreenPoint(EndP).y));
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

class procedure TRectangle.LoadFigure(ANode: TDOMNode);
var
  Rect: TRectangle;
begin
  SetLength(Figures, Length(Figures) + 1);
  Rect:= TRectangle.Create;
  Rect.InitFigure(ANode);
  ANode:= ANode.GetNextNode;
  Rect.StartP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                           StrToFloat(ANode.Attributes.Item[1].NodeValue));
  ANode:= ANode.GetNextNode;
  Rect.EndP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                         StrToFloat(ANode.Attributes.Item[1].NodeValue));
  Figures[High(Figures)]:= Rect;
end;

function TRectangle.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
begin
  Result:= ADoc.CreateElement('TRectangle');
  Self.SetValuesOfFigures(Result);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(StartP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(StartP.Y));
  Result.AppendChild(PNode);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(EndP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(EndP.Y));
  Result.AppendChild(PNode);
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
           ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
           ToScreenPoint(EndP).x, ToScreenPoint(EndP).y,
           RoundingX, RoundingY);
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

class procedure TRoundRectangle.LoadFigure(ANode: TDOMNode);
var
  RoundRect: TRoundRectangle;
begin
  SetLength(Figures, Length(Figures) + 1);
  RoundRect:= TRoundRectangle.Create;
  RoundRect.InitFigure(ANode);
  SetPropValue(RoundRect, 'RoundingX', ANode.Attributes.Item[5].NodeValue);
  SetPropValue(RoundRect, 'RoundingY', ANode.Attributes.Item[6].NodeValue);
  ANode:= ANode.GetNextNode;
  RoundRect.StartP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                                StrToFloat(ANode.Attributes.Item[1].NodeValue));
  ANode:= ANode.GetNextNode;
  RoundRect.EndP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                              StrToFloat(ANode.Attributes.Item[1].NodeValue));
  Figures[High(Figures)]:= RoundRect;
end;

function TRoundRectangle.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
begin
  Result:= ADoc.CreateElement('TRoundRectangle');
  Self.SetValuesOfFigures(Result);
  TDOMElement(Result).SetAttribute('RoundingX', IntToStr(RoundingX));
  TDOMElement(Result).SetAttribute('RoundingY', IntToStr(RoundingY));
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(StartP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(StartP.Y));
  Result.AppendChild(PNode);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(EndP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(EndP.Y));
  Result.AppendChild(PNode);
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
           Rect(ToScreenPoint(StartP).x, ToScreenPoint(StartP).y,
                ToScreenPoint(EndP).x, ToScreenPoint(EndP).y));
  if RectInRegion(Region, ARect) then Result:= True;
  DeleteObject(Region);
end;

class procedure TEllipse.LoadFigure(ANode: TDOMNode);
var
  Ellipse: TEllipse;
begin
  SetLength(Figures, Length(Figures) + 1);
  Ellipse:= TEllipse.Create;
  Ellipse.InitFigure(ANode);
  ANode:= ANode.GetNextNode;
  Ellipse.StartP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                              StrToFloat(ANode.Attributes.Item[1].NodeValue));
  ANode:= ANode.GetNextNode;
  Ellipse.EndP:= WorldPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                            StrToFloat(ANode.Attributes.Item[1].NodeValue));
  Figures[High(Figures)]:= Ellipse;
end;

function TEllipse.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
begin
  Result:= ADoc.CreateElement('TEllipse');
  Self.SetValuesOfFigures(Result);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(StartP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(StartP.Y));
  Result.AppendChild(PNode);
  PNode:= ADoc.CreateElement('point');
  TDOMElement(PNode).SetAttribute('x', FloatToStr(EndP.X));
  TDOMElement(PNode).SetAttribute('y', FloatToStr(EndP.Y));
  Result.AppendChild(PNode);
end;

initialization
AddFigure(TPen.Create);
AddFigure(TLine.Create);
AddFigure(TPolyline.Create);
AddFigure(TRectangle.Create);
AddFigure(TRoundRectangle.Create);
AddFigure(TEllipse.Create);

end.

