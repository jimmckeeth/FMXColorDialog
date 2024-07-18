DROP TABLE IF EXISTS colors;
CREATE TABLE colors(
    idx INTEGER PRIMARY KEY AUTOINCREMENT,
    hex NVARCHAR,
    name NVARCHAR, 
    red SMALLINT,
    green SMALLINT,
    blue SMALLINT,
    family NVARCHAR,
    link NVARCHAR,
    source NVARCHAR,
    ID NVARCHAR,
    hue SINGLE,
    saturation SINGLE,
    luminance SINGLE,
    cyan SINGLE,
    magenta SINGLE,
    yellow SINGLE,
    key SINGLE);

insert into colors (null, hex, name, red, green, blue, family, link, source) select * from colors_old;