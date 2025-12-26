with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Generic_Constrained_Array_Sort;

procedure Day5 is
   Ingredients_File_Name : constant String := "test.txt";

   type Cardinality is
      record
         N_Ranges, N_Ingredients : Natural := 0;
      end record;

   function Read_Cardinality (File_Name : String) return Cardinality is
      Ingredients_File : File_Type;
   begin
      Open (Ingredients_File, In_File, File_Name);
      return Count : Cardinality do
         while Get_Line (Ingredients_File) /= "" loop
            Count.N_Ranges := Count.N_Ranges + 1;
         end loop;
         while not End_Of_File (Ingredients_File) loop
            Skip_Line (Ingredients_File);
            Count.N_Ingredients := Count.N_Ingredients + 1;
         end loop;
         Close (Ingredients_File);
      end return;
   end Read_Cardinality;

   C : constant Cardinality := Read_Cardinality (Ingredients_File_Name);

   type Id_Range is
      record
         Low, High : Long_Integer := 0;
      end record;

   function "<" (Left, Right : Id_Range) return Boolean is
   begin
      return Left.Low < Right.Low;
   end "<";

   subtype Id_Range_Index is Positive range 1 .. C.N_Ranges;
   type Id_Range_Array is array (Id_Range_Index) of Id_Range;
   Id_Ranges : Id_Range_Array;

   procedure Sort_Id_Ranges is new
      Ada.Containers.Generic_Constrained_Array_Sort (
         Index_Type => Id_Range_Index,
         Element_Type => Id_Range,
         Array_Type => Id_Range_Array);

   Ingredients : array (1 .. C.N_Ingredients) of Long_Integer;
   Fresh : Natural := 0;
begin
   declare
      Ingredients_File : File_Type;
   begin
      Open (Ingredients_File, In_File, Ingredients_File_Name);
      for R of Id_Ranges loop
         declare
            Line : constant String := Get_Line (Ingredients_File);
            Dash_Pos : constant Natural := Index (Line, "-");
         begin
            R.Low := Long_Integer'Value (Line (1 .. Dash_Pos - 1));
            R.High := Long_Integer'Value (Line (Dash_Pos + 1 .. Line'Last));
         end;
      end loop;
      Skip_Line (Ingredients_File); -- empty line
      for I of Ingredients loop
         I := Long_Integer'Value (Get_Line (Ingredients_File));
      end loop;
      Close (Ingredients_File);
   end;

   for I of Ingredients loop
      for R of Id_Ranges loop
         if I in R.Low .. R.High then
            Fresh := Fresh + 1;
            exit;
         end if;
      end loop;
   end loop;
   Put_Line ("Number of fresh ingredients:" & Fresh'Image);

   Sort_Id_Ranges (Id_Ranges);
   declare
      Union_Range : Id_Range;
      Valid_Ids : Long_Integer := 0;
   begin
      for R of Id_Ranges loop
         if R.Low > Union_Range.High then
            Valid_Ids := Valid_Ids + R.High - R.Low + 1;
            Union_Range := R;
         elsif R.Low <= Union_Range.High then
            if R.High > Union_Range.High then
               Valid_Ids := Valid_Ids + R.High - Union_Range.High;
               Union_Range.High := R.High;
            end if;
         end if;
      end loop;
      Put_Line ("Total valid ingredient IDs:" & Valid_Ids'Image);
   end;
end Day5;
