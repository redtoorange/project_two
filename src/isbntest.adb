with Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Text_IO;
use Ada.Integer_Text_IO;


procedure Isbntest is
   Multiple_Wilds : exception;
   Invalid_Chars  : exception;
   Invalid_Length : exception;


   subtype Digit_Int  is Natural range 0..9;
   subtype Digit_Char is Character range '0'..'9';

   type ISBN is array(1..10) of Character;
   type ISBN_Array is array(Natural range <>) of ISBN;
   type ISBN_Col is record
      isbns         : ISBN_Array(1..50);
      current_index : Integer := 1;
   end record;

   procedure Add_ISBN( in_isbn : ISBN; rec : in out ISBN_Col )
   is
   begin
      if rec.current_index <= rec.isbns'Length then
         rec.isbns(rec.current_index) := in_isbn;
         rec.current_index := rec.current_index + 1;
      end if;
   end Add_ISBN;


   function Digit_To_Char( input : in Integer) return Character
   is
      char : Character;
   begin
      if input < 10 then
         char := Character'Val( Character'Pos('0') + input );
      elsif input = 10 then
         char := 'X';
      else
         char := '?';
      end if;

      return char;
   end Digit_To_Char;


   function Get_Char_Value( char : in Character; wild_card : out Boolean ) return Integer
   is
     value : Integer := 0;
   begin
      wild_card := False;

      case char is
         when '0'..'9' =>
            value := Integer'Value( (1 => char) );
         when 'X' | 'x' =>
            value := 10;
         when '?' =>
            value := 0;
            wild_card := True;
         when others =>
            value := 0;
      end case;


      return value;
   end Get_Char_Value;


   procedure Checksum( current_ISBN : in out ISBN; checksum : out Integer)
   is
      sum : Integer := 0;

      has_wild : Boolean := False;
      wild_pos : Integer := -1;

   begin
      for i in current_ISBN'Range loop
         sum := sum + (i * Get_Char_Value( current_ISBN(i), has_wild ) );

         if has_wild and wild_pos = -1 then
            wild_pos := i;
         elsif has_wild and wild_pos /= -1 then
            raise Multiple_Wilds;
         end if;
      end loop;


      if wild_pos /= -1 then
         for i in 0..10 loop
            declare
               val : Integer := i * wild_pos;
            begin
               if (sum + val) mod 11 = 0 then
                  sum := sum + val;
                  current_ISBN(wild_pos) := Digit_To_Char( i );
               end if;
            end;
         end loop;
      end if;

      checksum := sum mod 11;
   end checksum;



   --
   --  Check the given character to ensure it a valid character
   --
   function Valid_Char( char : Character) return Boolean is
      valid : Boolean;
   begin
      case char is
         when '0'..'9' | 'x' | 'X' | '?' =>
            valid := True;
         when '-' =>
            valid := False;
         when others =>
            raise Invalid_Chars;
      end case;

      return valid;
   end Valid_Char;


   function Valid_Char_Count( input : in String) return Integer
   is
      count : Integer := 0;
   begin

      for char of input loop
         if Valid_Char(char) then
            count := count + 1;
         end if;
      end loop;

      return count;
   end Valid_Char_Count;


   --  Convert a valid string into an ISBN
   function Convert_To_ISBN(input : in String) return ISBN
   is
      new_ISBN : ISBN;
      index : Integer := 1;
   begin
      for char of input loop
         if Valid_Char(char) then
            new_ISBN(index) := char;
            index := index + 1;
         end if;
      end loop;

      return new_ISBN;
   end Convert_To_ISBN;



   procedure ParseInput(
                        in_str        : in String;
                        valid_isbns   : in out ISBN_Col;
                        invalid_isbns : in out ISBN_Col
                       )
   is
      current_isbn : ISBN;
      check_digit : Integer;
   begin
      --  Check Length
      if Valid_Char_Count(in_str) /= 10 then
         raise Invalid_Length;
      end if;

      current_isbn := Convert_To_ISBN( in_str );
      Checksum( current_isbn, check_digit );



      if check_digit = 0 then
         Add_ISBN( current_isbn, valid_isbns );
      else
         Add_ISBN( current_isbn, invalid_isbns );
      end if;

   exception
      when Multiple_Wilds =>
         Put_Line( "Too many Wild Cards in: <" & in_str & ">" );

      when Invalid_Length =>
         Put_Line( "Invalid ISBN Length: <" & in_str & ">" );

      when Invalid_Chars =>
         Put_Line( "Invalid Chars in: <" & in_str & ">" );
   end ParseInput;


   valid_isbns   : ISBN_Col;
   invalid_isbns : ISBN_Col;
begin
   while not End_Of_File loop
      declare
         s : String := Get_Line;
      begin
         ParseInput( s, valid_isbns, invalid_isbns );
      end;
   end loop;

   Put_Line("Valid Check Sums:");
   for i of valid_isbns.isbns loop
      for j in i'Range loop
         Put( current_isbn(j) );
      end loop;

      Put( " " ); Put( check_digit'Image );
      New_Line;
   end loop;

   New_Line;

   Put_Line("Invalid Check Sums:");
   for i of invalid_isbns.isbns loop
   end loop;

end Isbntest;
