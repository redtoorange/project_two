-- Name: Andrew J. McGuiness
-- Date: September 26, 2017
-- Course: ITEC 320 Procedural Analysis and Design

-- Purpose: The program accepts a series of ISBNs from the user and presnets
--    errors if the input is invalid.  After an EOF is reached, the ISBNs are
--    grouped and printed according to their checksums.  The ones with a 0
--    checksum are valid, and those /= 0 are invalid.  If there are wildcards
--    in the ISBN, the program will automatically replace them with the digit
--    required to make the ISBN have a passing checksum.
--
--  **Sample Input**
--  -1111111111
--  2222222222
--  33x3333333
--  44----44444444
--  5515555555
--  66?6666666
--  77?77?7777
--  88333?88?88
--  9939999
--  --03-0?0--
--  ?XXXXXXXXX
--  ?23456789X
--  ?987654321
--   1122334455
--
--
--  **Expected Output**
--  ERROR in Line 3. Invalid character in string: <33x3333333>
--  ERROR in Line 7. Too many question marks: <77?77?7777>
--  ERROR in Line 8. String too long: <88333?88?88>
--  ERROR in Line 9. String too short: <9939999>
--  ERROR in Line 10. String too short: <--03-0?0-->
--  ERROR in Line 14. Invalid character in string: < 1122334455 >
--
--  Valid Check Sums:
--     1111111111 0
--     2222222222 0
--     4444444444 0
--     6666666666 0
--     XXXXXXXXXX 0
--     123456789X 0
--     X987654321 0
--
--  Invalid Check Sums:
--     5515555555 10


with Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Text_IO;
use Ada.Integer_Text_IO;


procedure Isbntest is
   Multiple_Wilds   : exception;
   Invalid_Chars    : exception;
   Invalid_Length_S : exception;
   Invalid_Length_L : exception;


   subtype Digit_Int  is Natural   range 0..10;
   subtype Digit_Char is Character range '0'..'9';


   type ISBN           is array(1..10)            of Character;
   type ISBN_Array     is array(Natural range <>) of ISBN;
   type checksum_Array is array(Natural range <>) of Natural;


   ----------------------------------------------------------
   -- Purpose: Convert an Integer into it's Character respresentation
   -- Parameters: input: Integer to convert
   -- Precondition: input <= 10
   -- Postcondition: Returns '0'..'9' or 'X' for ten
   ----------------------------------------------------------
   function Digit_To_Char( input : in Digit_Int) return Character
   is
      char : Character;      -- Hold the translated Character
   begin
      if input < 10 then
         -- Determine the Character based on position.
         char := Character'Val( Character'Pos('0') + input );
      elsif input = 10 then
         char := 'X';
      end if;

      return char;
   end Digit_To_Char;


   ----------------------------------------------------------
   -- Purpose: Convert a Character into it's Integer value.
   -- Parameter:  char        : Character to convert.
   --             is_wild_card: Flag set to true if char = '?'.
   -- Precondition: char is in '0'..'9', 'X' or '?'
   -- Postcondition: Returns the value of char.  If char was '?',
   --     is_wild_card flag is set and value is 0.
   ----------------------------------------------------------
   function Get_Char_Value(char         : in  Character;
                           is_wild_card : out Boolean) return Integer
   is
     value : Integer := 0;
   begin
      is_wild_card := False;

      case char is
         when '0'..'9' =>
            value := Digit_Int'Value( (1 => char) );

         when 'X'      =>
            value := 10;

         when '?'      =>
            value := 0;
            is_wild_card := True;

         when others   =>
            value := 0;

      end case;

      return value;
   end Get_Char_Value;


   ----------------------------------------------------------
   -- Purpose: Calculate the required value of a wildcard to have a valid
   --    checkdigit.
   -- Parameters: current_isbn: ISBN with the wildcard
   --                 wild_pos: position of the wildcard
   --                      sum: Total value of the ISBN before the wildcard
   --                           is calculated.
   -- Precondition: wild_pos > 0
   -- Postcondition: The wildcard in the ISBN will be replaced by the digit
   --    required to have a valid ISBN.  Sum will contain the new sum such that:
   --    sum mod 11 = 0
   ----------------------------------------------------------
   procedure Calculate_Wildcard(current_ISBN : in out ISBN;
                                wild_pos     : in Integer;
                                sum          : in out Integer)
   is
      value : Integer := 0;
   begin
      for i in 0..10 loop
         value := i * wild_pos;

         if (sum + value) mod 11 = 0 then
            sum := sum + value;
            current_ISBN(wild_pos) := Digit_To_Char( i );
         end if;
      end loop;
   end Calculate_Wildcard;


   ----------------------------------------------------------
   -- Purpose: Calculate the check digit of an ISBN.
   -- Parameters: current_ISBN: ISBN to find the check digit for.
   --                 checksum: ISBN's checksum.
   -- Postcondition: If current_ISBN contained a wildcard, it will be
   --    calculated and replaced.
   -- Exception: Raises Multiple_Wilds exception if there are multiple
   --    wildcards.
   ----------------------------------------------------------
   procedure Calculate_Checksum( current_ISBN : in out ISBN;
                                     checksum : out Integer)
   is
      sum          : Integer := 0;
      char_is_wild : Boolean := False;    -- Flag to check for wildcards
      wild_pos     : Integer := -1;

   begin
      for i in current_ISBN'Range loop
         sum := sum + (i * Get_Char_Value( current_ISBN(i), char_is_wild ) );

         --  Cache position of first wild, raise exception if another is found
         if char_is_wild and wild_pos = -1 then
            wild_pos := i;
         elsif char_is_wild and wild_pos /= -1 then
            raise Multiple_Wilds;
         end if;
      end loop;


      if wild_pos /= -1 then
         Calculate_Wildcard( current_ISBN, wild_pos, sum );
      end if;

      checksum := sum mod 11;
   end Calculate_Checksum;


   ----------------------------------------------------------
   -- Purpose: Check the given Character to ensure it is valid.
   -- Parameters: char: Character to check
   -- Postcondition: Returns True if char is '0'..'9' | 'X' | '?' and
   --    False if char is '-'.
   -- Exception: Raises Invalid_Chars if there is a character outside of the
   --    valid range.
   ----------------------------------------------------------
   function Valid_Char( char : in Character) return Boolean is
      valid : Boolean;
   begin
      case char is
         when '0'..'9' | 'X' | '?' =>
            valid := True;
         when '-' =>
            valid := False;
         when others =>
            raise Invalid_Chars;
      end case;

      return valid;
   end Valid_Char;


   ----------------------------------------------------------
   -- Purpose: Count valid Characters in a String.
   -- Parameters: input: User's input String
   -- Postcondition: Returns the number of valid Characters.
   ----------------------------------------------------------
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


   ----------------------------------------------------------
   -- Purpose: Convert a valid String into an ISBN
   -- Parameters:    input: User's input String
   --             new_ISBN: newly formed ISBN
   -- Precondition:  The input String contains 10 valid ISBN digits.
   -- Postcondition: new_ISBN will contain a valid ISBN.
   ----------------------------------------------------------
   procedure Convert_To_ISBN( input : in String; new_ISBN : out ISBN )
   is
      index : Integer := 1;
   begin
      for char of input loop
         if Valid_Char(char) then
            new_ISBN(index) := char;
            index := index + 1;
         end if;
      end loop;
   end Convert_To_ISBN;


   ----------------------------------------------------------
   -- Purpose: Validate a User's input into an ISBN, calculate the checksum and
   --    insert them into the designated arrays.
   -- Parameters: in_str: User's input String
   --              isbns: array of ISBNs
   --             checks: array of Checksums
   --              index: index of next free slot in both arrays
   --               line: line number of the input
   -- Postcondition: index is incremented if the input was accepted.  isbns and
   --    checks will contain the matching ISBN and Checksum pair.
   -- Exception:  Raises Invalid_Length_S or Invalid_Length_L if there are
   --    not enough or too many ISBN digits.  All Exceptions for invalid input
   --    are caught here.
   ----------------------------------------------------------
   procedure ParseInput(in_str : in     String;
                        isbns  : in out ISBN_Array;
                        checks : in out checksum_Array;
                        index  : in out Integer;
                        line   : in     Integer)
   is
      current_isbn : ISBN;
      check_digit  : Integer;
   begin
      declare
         length : Integer := Valid_Char_Count(in_str);
      begin
         if length < 10 then
            raise Invalid_Length_S;
         elsif length > 10 then
            raise Invalid_Length_L;
         end if;
      end;

      Convert_To_ISBN( in_str, current_isbn );
      Calculate_Checksum( current_isbn, check_digit );

      isbns(index)  := current_isbn;
      checks(index) := check_digit;
      index         := index + 1;

   exception
      when Multiple_Wilds =>
         Put( "Error in Line " ); Put( line, 0); Put(".  ");
         Put_Line( "Too many question marks: <" & in_str & ">" );

      when Invalid_Length_S =>
         Put( "Error in Line " ); Put( line, 0); Put(".  ");
         Put_Line( "String too short: <" & in_str & ">" );

      when Invalid_Length_L =>
         Put( "Error in Line " ); Put( line, 0); Put(".  ");
         Put_Line( "String too long: <" & in_str & ">" );

      when Invalid_Chars =>
         Put( "Error in Line " ); Put( line, 0); Put(".  ");
         Put_Line( "Invalid characters in string: <" & in_str & ">" );
   end ParseInput;


   ----------------------------------------------------------
   -- Purpose: Print a single ISBN
   -- Parameters: isbn_to_put: ISBN that should be printed
   ----------------------------------------------------------
   procedure Put_ISBN( isbn_to_put : in ISBN )
   is
   begin
      for i in isbn_to_put'range loop
         Put( isbn_to_put(i) );
      end loop;
   end Put_ISBN;


   ----------------------------------------------------------
   -- Print an ISBN formatted according to specifications
   -- Parameters:  isbn_to_put: ISBN to print
   --                    check: check digit to print
   ----------------------------------------------------------
   procedure ISBN_PrettyPrint( isbn_to_put : in ISBN; check : in Integer )
   is
   begin
      Set_Col(4);
      Put_ISBN( isbn_to_put );
      Put( " " );
      Put( check, 0 );
      New_Line;
   end ISBN_PrettyPrint;


   ----------------------------------------------------------
   -- Purpose: Print all ISBNs followed by their check digits.
   -- Parameters: isbns: array of ISBNs to print
   --         checksums: array of corresponding checksums
   ----------------------------------------------------------
   procedure Print_ISBNS(    isbns : in ISBN_Array;
                         checksums : in checksum_Array;
                        last_index : in Natural)
   is
   begin
      New_Line;
      -- Print all ISBNs with valid Checksums.
      Put_Line("Valid Check Sums:");
      for i in checksums'Range loop
         exit when i = last_index;

         if checksums(i) = 0 then
            ISBN_PrettyPrint( isbns(i), checksums(i) );
         end if;
      end loop;

      New_Line;

      --  Print all ISBNs with invalid Checksums.
      Put_Line("Invalid Check Sums:");
      for i in checksums'Range loop
         exit when i = last_index;

         if checksums(i) > 0 then
            ISBN_PrettyPrint( isbns(i), checksums(i) );
         end if;
      end loop;
   end Print_ISBNS;


   ----------------------------------------------------------
   -- Purpose: Read input until end of file, then print ISBNs in two groups:
   --    Valid and Invalid.
   ----------------------------------------------------------
   isbns      : ISBN_Array(1..1000);
   checksums  : checksum_Array(1..1000);
   next_index : Integer := 1;    -- next free index in arrays
   line       : Integer := 0;    -- current line number of input
begin
   while not End_Of_File loop
      declare
         next_line : String := Get_Line;
      begin
         line := line + 1;
         ParseInput( next_line, isbns, checksums, next_index, line );
      end;
   end loop;

   Print_ISBNS( isbns, checksums, next_index );
end Isbntest;
