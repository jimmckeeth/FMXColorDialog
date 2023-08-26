unit X11ColorData;

interface

uses System.UITypes;

type
  X11Colors = record
  const
    ColorCount = 145;
    ColorValues: Array [0 .. 144] of TAlphaColor = ($FFF0F8FF, $FFFAEBD7,
      $FF00FFFF, $FF7FFFD4, $FFF0FFFF, $FFF5F5DC, $FFFFE4C4, $FF000000,
      $FFFFEBCD, $FF0000FF, $FF8A2BE2, $FFA52A2A, $FFDEB887, $FF5F9EA0,
      $FF7FFF00, $FFD2691E, $FFFF7F50, $FF6495ED, $FFFFF8DC, $FFDC143C,
      $FF00FFFF, $FF00008B, $FF008B8B, $FFB8860B, $FFA9A9A9, $FF006400,
      $FFBDB76B, $FF8B008B, $FF556B2F, $FFFF8C00, $FF9932CC, $FF8B0000,
      $FFE9967A, $FF8FBC8F, $FF483D8B, $FF2F4F4F, $FF00CED1, $FF9400D3,
      $FFFF1493, $FF00BFFF, $FF696969, $FF1E90FF, $FFB22222, $FFFFFAF0,
      $FF228B22, $FFFF00FF, $FFDCDCDC, $FFF8F8FF, $FFFFD700, $FFDAA520,
      $FFBEBEBE, $FF808080, $FF00FF00, $FF008000, $FFADFF2F, $FFF0FFF0,
      $FFFF69B4, $FFCD5C5C, $FF4B0082, $FFFFFFF0, $FFF0E68C, $FFE6E6FA,
      $FFFFF0F5, $FF7CFC00, $FFFFFACD, $FFADD8E6, $FFF08080, $FFE0FFFF,
      $FFFAFAD2, $FFD3D3D3, $FF90EE90, $FFFFB6C1, $FFFFA07A, $FF20B2AA,
      $FF87CEFA, $FF778899, $FFB0C4DE, $FFFFFFE0, $FF00FF00, $FF32CD32,
      $FFFAF0E6, $FFFF00FF, $FFB03060, $FF800000, $FF66CDAA, $FF0000CD,
      $FFBA55D3, $FF9370DB, $FF3CB371, $FF7B68EE, $FF00FA9A, $FF48D1CC,
      $FFC71585, $FF191970, $FFF5FFFA, $FFFFE4E1, $FFFFE4B5, $FFFFDEAD,
      $FF000080, $FFFDF5E6, $FF808000, $FF6B8E23, $FFFFA500, $FFFF4500,
      $FFDA70D6, $FFEEE8AA, $FF98FB98, $FFAFEEEE, $FFDB7093, $FFFFEFD5,
      $FFFFDAB9, $FFCD853F, $FFFFC0CB, $FFDDA0DD, $FFB0E0E6, $FFA020F0,
      $FF800080, $FF663399, $FFFF0000, $FFBC8F8F, $FF4169E1, $FF8B4513,
      $FFFA8072, $FFF4A460, $FF2E8B57, $FFFFF5EE, $FFA0522D, $FFC0C0C0,
      $FF87CEEB, $FF6A5ACD, $FF708090, $FFFFFAFA, $FF00FF7F, $FF4682B4,
      $FFD2B48C, $FF008080, $FFD8BFD8, $FFFF6347, $FF40E0D0, $FFEE82EE,
      $FFF5DEB3, $FFFFFFFF, $FFF5F5F5, $FFFFFF00, $FF9ACD32);
    ColorNames: Array [0 .. 144] of String = ('Alice Blue', 'Antique White',
      'Aqua', 'Aquamarine', 'Azure', 'Beige', 'Bisque', 'Black',
      'Blanched Almond', 'Blue', 'Blue Violet', 'Brown', 'Burlywood',
      'Cadet Blue', 'Chartreuse', 'Chocolate', 'Coral', 'Cornflower Blue',
      'Cornsilk', 'Crimson', 'Cyan', 'Dark Blue', 'Dark Cyan', 'Dark Goldenrod',
      'Dark Gray', 'Dark Green', 'Dark Khaki', 'Dark Magenta',
      'Dark Olive Green', 'Dark Orange', 'Dark Orchid', 'Dark Red',
      'Dark Salmon', 'Dark Sea Green', 'Dark Slate Blue', 'Dark Slate Gray',
      'Dark Turquoise', 'Dark Violet', 'Deep Pink', 'Deep Sky Blue', 'Dim Gray',
      'Dodger Blue', 'Firebrick', 'Floral White', 'Forest Green', 'Fuchsia',
      'Gainsboro*', 'Ghost White', 'Gold', 'Goldenrod', 'Gray', 'Web Gray',
      'Green', 'Web Green', 'Green Yellow', 'Honeydew', 'Hot Pink',
      'Indian Red', 'Indigo', 'Ivory', 'Khaki', 'Lavender', 'Lavender Blush',
      'Lawn Green', 'Lemon Chiffon', 'Light Blue', 'Light Coral', 'Light Cyan',
      'Light Goldenrod', 'Light Gray', 'Light Green', 'Light Pink',
      'Light Salmon', 'Light Sea Green', 'Light Sky Blue', 'Light Slate Gray',
      'Light Steel Blue', 'Light Yellow', 'Lime', 'Lime Green', 'Linen',
      'Magenta', 'Maroon', 'Web Maroon', 'Medium Aquamarine', 'Medium Blue',
      'Medium Orchid', 'Medium Purple', 'Medium Sea Green', 'Medium Slate Blue',
      'Medium Spring Green', 'Medium Turquoise', 'Medium Violet Red',
      'Midnight Blue', 'Mint Cream', 'Misty Rose', 'Moccasin', 'Navajo White',
      'Navy Blue', 'Old Lace', 'Olive', 'Olive Drab', 'Orange', 'Orange Red',
      'Orchid', 'Pale Goldenrod', 'Pale Green', 'Pale Turquoise',
      'Pale Violet Red', 'Papaya Whip', 'Peach Puff', 'Peru', 'Pink', 'Plum',
      'Powder Blue', 'Purple', 'Web Purple', 'Rebecca Purple', 'Red',
      'Rosy Brown', 'Royal Blue', 'Saddle Brown', 'Salmon', 'Sandy Brown',
      'Sea Green', 'Seashell', 'Sienna', 'Silver', 'Sky Blue', 'Slate Blue',
      'Slate Gray', 'Snow', 'Spring Green', 'Steel Blue', 'Tan', 'Teal',
      'Thistle', 'Tomato', 'Turquoise', 'Violet', 'Wheat', 'White',
      'White Smoke', 'Yellow', 'Yellow Green');
  end;

implementation

end.
