
with Ada.Text_IO;         use  Ada.Text_IO;
with Ada.Integer_Text_IO; use  Ada.Integer_Text_IO;

procedure helloadaexceptions is

    x   :integer range 1..20;
    y   :integer;

begin

    put("enter a number "); get(y);
    x:=y;
    put("thank you");

end helloadaexceptions;

