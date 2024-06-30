DROP TABLE IF EXISTS COLORNERD;
CREATE TABLE colornerd(
    "index" INTEGER primary key,
    "Color Name" NVARCHAR, 
    "ID" NVARCHAR, 
    "Hex" NVARCHAR,
    "Red" SMALLINT,
    "Green" SMALLINT,
    "Blue" SMALLINT,
    "Hue" SINGLE,
    "Saturation" SINGLE,
    "Luminance" SINGLE,
    "Cyan" SINGLE,
    "Magenta" SINGLE,
    "Yellow" SINGLE, 
    "Key" SINGLE,
    "Source" NVARCHAR,
    "System" NVARCHAR);
.import all_colornerd_paints.csv colornerd --skip 1    

create index idx_colornerd_name on colornerd("name");
create index idx_colornerd_hex on colornerd("hex");
create index idx_colornerd_system on colornerd("System");
create index idx_colornerd_hue on colornerd("hue");
create index idx_colornerd_Saturation on colornerd("Saturation");
create index idx_colornerd_Luminance on colornerd("Luminance");
