with Ada.Text_IO;
use Ada.Text_IO;

procedure Isbntest is
   type Str_Array is array(Natural range <>) of String;

   function Checksum( input : in String; checksum : out Integer) return Boolean
   is
      for i in input'Range loop
         Put_Line( inpit(i) );
      end loop;

   end checksum;

begin
   Put("Enter Name: ");
   declare
      s : String := Get_Line;
   begin
      Put( "You name is: " & s );
      Checksum( s
   end;
end Isbntest;
