unit TriangleColorWheelMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  System.Types,
  System.UITypes, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm27 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    Button1: TButton;
    pnl1: TPanel;
    TrackBar1: TTrackBar;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form27: TForm27;

implementation

uses
  CodeSiteLogging,
  SkiaHelpers,
  System.Math,
  System.UIConsts;

{$R *.dfm}

const Names30DegreeColors: array [0..11] of string = ('Red','Orange','Yellow','Chartreuse','Green','Spring','Cyan','Azure','Blue','Violet','Magenta','Rose');
const AllHueNames: array of string =
  ['Red','Grapefruit','Vampire Red','Shocking Crimson','Mushroom Red','Carolina Reaper','Bittersweet','Emergency Reds','Redrum','Scarlet Glow','Blazing Orange','Red Riot','Sunset By The Nile','Coquelicot','Salmon',
  'Phoenix Fire','Coral','Flame','Pale Copper','Evening Ember','Mystic red','Bright Orange','Persimmon','Orange Ish','Orangish','Burnt Orange','Moroccan Sunset','Philippine orange','Cheese Ball Dust','Merin''s Fire',
  'Orange','Neon Carrot','Light Orange','Dark Orange','Yellow Orange','Pastel Orange','Vivid gamboge','Orange Peel','Oranger Ice','Cheese','Sun Ray','Apricot Orange','Chinese yellow','Sunflower Mango','Nacho Cheese',
  'Amber','Golden Yellow','Golden Sunrise','Tangerine yellow','Fake cheese','Sunhouse','Soviet Gold','Sundried','Gold Shine','Straw','Irish Daffodil','Pure Sunshine','Yellow Mellow','Lemon','Berry Bee Benson','Yellow',
  'Electric Banana','Stinging Neon','Scruffy Yellow','Zeus''s Bolt','Battery Acid','Pear','Luminescent Banana','Lime Zest','Unripe','Split Pea','Neon Yellow','Electric lime','Liquid Neon','Chartreusey','Lime',
  'Neon Lemon Grass','Yelleen','Greener Alien ','Sour Apple Crush','Spring Bud','Spilt Acid','Radioactive Green','bright yellow green','Greenday','Neon Lime','Sprite Lemon Lime','The Burning Green','Non-Toxic Green',
  'Alien Fungus','Chartreuse','Lawn Green','Lawn','Toxic Aliens','Green Glow','Laurel Green','Bright green','Electric Cucumber','Toxic Green','Frog Candy','Green Apple','Gecko','Rainy Green','Chlorophyll ','Limeom',
  'Harlequin','Leprechaun','Dreams Fav Colour','Highlighter Green','Battery-Powered','Harlequeen','Nuclear Power','Electrical Green','Lucky Clover Shine','Bad Guy','Zint','Synthetic Green','Nickelodeon Slime','Limey',
  'Ultragreen','Green','Electric Pickle','Eye Strain Lime','Lime Neon Green','Green Screen','Creepy Green','Brightest Forest','Sprite','Toxicitea','Cyber Neon Green','Plant Cell','Villain''s Green','Lucent Lime',
  'Toxic Poisoning','Billie','Erin','Dasyphyllous Green','Danger Green','Area 51','Seaweed Sandcastle','Secluded Moss','Emerald Splash','Sprung Green','Fictional Grass','Ecstatic Green','Seafoam Airz','Minty Sails',
  'Mint Highlights','Neon Rosemary','Sundew Green','Spring','Mountain Jewel','Seaweed','Bright Acid Green','Mint Romance','GovernMint','Tropical Elements','Sugar Mint','Eye Asassination','Bright Seagreen','Viriliam',
  'Echomint','Subtle Mint','Sea-grass','Cold Mint','Technobotanica','Bright Seaglass','Ocean  Man','Turquoise Sprite','True Mint','Illuminated Rain','Pool Of Diamonds','Balmy Weather','Fabulous Mint','Vibrant Mint',
  'Caribbean','Turquoise Blue','Mystic Blue','Toothepaste Teal','Toothpaste','Cyan','Agressive Aqua','Bright Cyan','Neon Cyan','Off Cyan','Azulity','Tropical Aqua Blue','Manager','Ocean Of Angels','Light Sea',
  'Enlightened Blue','Bike Ride Blue','Vivid sky blue','Suvaros','Cyan Sky','Deep Sky Blue','Hawaii Morning','Ocean''s Horizon','Derp Sky Blue','Groundspeed Blue','Heavens Hue','Skylighter Blue','Cloudless',
  'Light Sky Blue','Spectre Blue','Dark Sky Blue','Dodged Blue','Steel Blue','Beta Blue','Azure Blue World','Azure','Bootstrap Blue','Blue Sparkle','Denim','Brandeis Blue','Cobalt','Bright Blue','Sapphire',
  'Relaxed Blue','Ice Cold','Giko Blue','Lightish Blue','Sonic Skin','False Periwinkle','Electronic Ocean','Cerulean','Integrity Sapphire','Electric Feel','Ultranavy Blue','Persian Blue','Neon Blue','Blue Bird',
  'Deadly Blue','Wozniak Blue','Sonic','Blue Electricity','Saint Patrick Blue','Luna Blue','Ocean Rider','Sans','Blue','Lapis Lipstick','Azure Deep Ocean','IKEA','Deep Sleepin','Ultramarine Blue','Obnoxiously Blue',
  'Strictly 3-D Blue','Under The Sea','Bluebolt','Abandon Waters','Kaito Blue','UV Bomb','Dragonblue','Da Ba Dee Blue','Blueberry Pie','Bluple','Ceruliwinkle','Purplish Blue','Error Blue','Depression Feels',
  'Peacock Blue','Eclipse Purple','Dark Pastel Purple','Electric Indigo','Mr Purple','Indigo Neon','Original Barney','Lalitha','Purple Neon','Violet','Blue Purple','Royal Eggplant','Hot Lavender','Electric Violet',
  'Star Platinum','Perplexed Purple','Purple Turtle','Pixie Iris','Vernon','Dark Orchid','Wisteria','Cosmic Purple','Neon Lavender','Rich Lilac','Electric purple','Glowing Violet','Chewy Plum','Vivid orchid',
  'Network Purple','Electrogogo','Vaporwave','Psychedelic Purple','Uncut Amethyst','Mwave','Pastel Purple','Broadway Pink','Energetic Orchid','Purple Pink','A Unicorn Sneeze','Magenta','Light Magenta','Magitink',
  'Warm Purple','Vice City','Dolly','Violence Violet','Pinkest Pink','Candy Pink','Ow My Eyes','Neopink','Molly Rose','Hot Magenta','Electric Pink','Pinky Pi','Pink','Purple Pizzazz','Bubblegum Bright','Barbz',
  'Flirty Sunday','Umbridge','Spicy Pink','Shocking Pink','Fanatic Fuchsia','Pink Pearl','Magenta Pink','Magenta pigment','Hot Pink','Mulberry','Love World','Rose','In The Feedback','Infinity Pink','Retro Pink Pop',
  'Respark','Shocking Rose','Newborn Cherry','Raspberry','Razzmatazz','New York Sunset','Plump Rose','Hawaiian Raspberry','Summer Raspberry','Starburst Red','Warm Pink','Carmine','Lipstick Red','Princess Castle',
  'Rasberry Acai','Rabid Raspberry','Phoenix Red','Vivid Cranberry','Love Offering','Theater Red','Watermelon','Blood','Fury','bright red','Marinara Red','Coral Pink'];

function CalculateTriangleVertices(const Center: TPointF; const Radius, Angle: Single): TArray<TPointF>;
begin
  var SideLength := Radius / Sqrt(3);

  var AdjustedAngle := (Angle + Pi / 2);

  // The top vertex of the triangle (touching the circle)
  var Triangle0 := TPointF.Create(
    Center.X + Radius * Cos(AdjustedAngle),
    Center.Y + Radius * Sin(AdjustedAngle)
  );

  // The other two vertices based on the side length and adjusted angle
  var Triangle1 := TPointF.Create(
    Triangle0.X - SideLength * Cos(AdjustedAngle - Pi / 6),
    Triangle0.Y - SideLength * Sin(AdjustedAngle - Pi / 6)
  );

  var Triangle2 := TPointF.Create(
    Triangle0.X - SideLength * Cos(AdjustedAngle + Pi / 6),
    Triangle0.Y - SideLength * Sin(AdjustedAngle + Pi / 6)
  );

  Result := TArray<TPointF>.Create(Triangle0, Triangle1, Triangle2);
end;

function RadiansToDegrees(const Radians: Single): Single;
begin
  Result := Radians * (180 / Pi);
end;

function ModFloat(X, Y: Double): Double;
begin
  Result := X - Y * Floor(X / Y);
end;

procedure DrawTriangle(const ACanvas: ISkCanvas; const Vertices: TArray<TPointF>;
  const Color: TAlphaColor);
begin
  var PathBuilder := TSkPathBuilder.Interfaced;
  PathBuilder.MoveTo(Vertices[0]);
  PathBuilder.LineTo(Vertices[1]);
  PathBuilder.LineTo(Vertices[2]);
  PathBuilder.Close;

  var path := PathBuilder.Detach;

  var Paint := TSKPaint.Interfaced;
  Paint.Color := Color;
  ACanvas.DrawPath(path, Paint);

end;

procedure RenderTriangle(const AIndex, ANumTriangles: Integer;
  const ACenter: TPointF; const ARadius: Single; const ACanvas: ISkCanvas);
begin
  var Angle := AIndex * 2 * Pi / ANumTriangles;
  var Vertices := CalculateTriangleVertices(ACenter, ARadius, Angle);
  var Color := HSLToRGB((Angle + pi) / (2 * Pi), 1, 0.5);

  DrawTriangle(ACanvas, Vertices, Color);
end;

function GetTextHeight(AText: String; AFont: ISkFont): Single;
begin
  var bounds := AFont.GetBounds(AFont.GetGlyphs(AText));
  Result := 0;
  for var bound in bounds do
  begin
    if bound.Height > Result then
      Result := bound.Height;
  end;
end;

procedure RenderColorWheel(const ANumTrianges: Integer; const ACanvas: ISkCanvas;
  const AOpacity: Single; const ADest: TRectF);
begin
  var Center :=
    TPointF.Create(
      (ADest.Left + ADest.Right) / 2,
      (ADest.Top + ADest.Bottom) / 2);
  var Radius := Min(ADest.Width, ADest.Height) / 2;
  var TriangleHeight := 0.85 * Radius;

  var Paint := TSKPaint.Interfaced;
  Paint.AlphaF := AOpacity;

  // First pass: Draw every other triangle starting at 30 degrees
  for var i := 1 to ANumTrianges - 1 do
    if i mod 2 = 1 then
      RenderTriangle(i, ANumTrianges, Center, TriangleHeight, ACanvas);

  // Second pass: Draw the remaining triangles
  for var i := 0 to ANumTrianges do
    if i mod 2 = 0 then
      RenderTriangle(i, ANumTrianges, Center, TriangleHeight, ACanvas);

  var DegreeFont := TSKFont.Interfaced(
    TSkTypeface.MakeFromName('Ubuntu', TSkFontStyle.Normal), Radius * 0.04);
  var NameFont := TSKFont.Interfaced(
    TSkTypeface.MakeFromName('Ubuntu', TSkFontStyle.Normal), Radius * 0.07);
  var HexFont := TSKFont.Interfaced(
    TSkTypeface.MakeFromName('Ubuntu Mono', TSkFontStyle.Bold), Radius * 0.05);

  var HexPosition := Min(ADest.Width, ADest.Height) / 4;
  var DegreePosition := DegreeFont.Size+DegreeFont.Size;
  var NamePosition := DegreePosition + NameFont.Size;

  var degreeStop := min(1, ANumTrianges mod 8);
  var hexStop := ANumTrianges div min(12, ANumTrianges);

  for var i := 0 to ANumTrianges-1 do
  begin
    paint.Color := TAlphaColors.Black;

    var offsetAngle := (i / ANumTrianges) * 360;

//    if i mod degreeStop = 0 then
//    begin
//      var flip := (offsetAngle > 90) and (offsetAngle < 270);
//      // Degrees
//      var X := Center.X - DegreeFont.MeasureText(offsetAngle.ToString) / 2;
//      var TextHeight := GetTextHeight(offsetAngle.ToString + #$0B0, DegreeFont);
//      //if offsetAngle mod 10 = 0 then
//      begin
//        if flip then ACanvas.Rotate(180, Center.X, DegreePosition - TextHeight/2);
//        ACanvas.DrawSimpleText(offsetAngle.ToString + #$0B0, X, DegreePosition, DegreeFont, paint);
//        if flip then ACanvas.Rotate(180, Center.X, DegreePosition - TextHeight/2);
//      end;
//    end;

    var flip := (offsetAngle > 90) and (offsetAngle < 270);

    // Name
    if ANumTrianges = Length(Names30DegreeColors) then
    begin
      var TextHeight := GetTextHeight(Names30DegreeColors[i], NameFont);
      var X := Center.X - NameFont.MeasureText(Names30DegreeColors[i]) / 2;
      if flip then ACanvas.Rotate(180, Center.X, NamePosition - TextHeight/2);
      ACanvas.DrawSimpleText(Names30DegreeColors[i], X, NamePosition, NameFont, paint);
      if flip then ACanvas.Rotate(180, Center.X, NamePosition - TextHeight/2);
    end;

    if (i mod hexStop = 0) and (i div hexStop < 12) then
    begin
      // Hex
      var color := HSLtoRGB(offsetAngle/360, 1.0, 0.5);
      var hex := '#'+IntToHex(color).Remove(0,2);
      var TextHeight := GetTextHeight(hex, HexFont);

      var x := Center.X - HexFont.MeasureText(hex) / 2;
      paint.Color := TAlphaColors.Black;
      paint.Style := TSkPaintStyle.Stroke;
      paint.StrokeWidth := 2;

      if flip then ACanvas.Rotate(180, Center.X, hexPosition + TextHeight/3);
      ACanvas.DrawSimpleText(hex, x, hexPosition+10, HexFont, paint);
      paint.Style := TSkPaintStyle.Fill;
      paint.Color := TAlphaColors.White;
      ACanvas.DrawSimpleText(hex, x, hexPosition+10, HexFont, paint);
      if flip then ACanvas.Rotate(180, Center.X, hexPosition + TextHeight/3);
    end;

    ACanvas.Rotate(360/ANumTrianges, Center.X, Center.Y);
  end;

end;

procedure CreateSVG(const AOutputFileName: string; const ADest: TRectF);
begin
  var LStream := TFileStream.Create(AOutputFileName, fmCreate);
  try
    var LCanvas := TSkSVGCanvas.Make(ADest, LStream, [TSkSVGCanvasFlag.ConvertTextToPaths]);
    RenderColorWheel(72, LCanvas, 1, ADest);
    LCanvas := nil;
  finally
    LStream.Free;
  end;
end;

procedure TForm27.Button1Click(Sender: TObject);
begin
  CreateSVG('TriangeColorWheel(RGB-HSL).svg', TRectF.Create(0, 0, 1000, 1000));
end;

procedure TForm27.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  RenderColorWheel(TrackBar1.Position, ACanvas, AOpacity, ADest);
end;

procedure TForm27.TrackBar1Change(Sender: TObject);
begin
  SkPaintBox1.Redraw;
end;

end.
