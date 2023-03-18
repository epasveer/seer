with Text_IO; use Text_IO;
with System;
with System.Address_Image;
with System.Address_To_Access_Conversions;

procedure HelloAda is

    -- Provided by Henrik Glass (https://github.com/henrikglass)

    -- a type describing a float array of length 256.
    type Float_Array_T is array(0..255) of Float;

    -- an "access type" for the above type.
    type Float_Array_Access_T is access all Float_Array_T;

    -- idek...
    package Conversions is new System.Address_To_Access_Conversions(Float_Array_T);
    use Conversions;

    -- some variables.
    My_Float_Array         : aliased Float_Array_T;
    My_Float_Array_Address : System.address;
    My_Float_Array_Access  : Float_Array_Access_T;
    My_Float_Array_Obj_Ptr : Conversions.Object_Pointer;

begin
    -- Fill My_Float_Array with data
    for i in 0 .. 64 loop
        My_Float_Array(i) := 1.3 * Float(i);
    end loop;


    My_Float_Array_Address := My_Float_Array'Address;
    My_Float_Array_Access  := My_Float_Array'Access;
    My_Float_Array_Obj_Ptr := Conversions.To_Pointer(My_Float_Array_Address);

    Put_Line(System.Address_Image(My_Float_Array_Address));
    Put_Line(Float'Image(My_Float_Array(3)));
end HelloAda;

