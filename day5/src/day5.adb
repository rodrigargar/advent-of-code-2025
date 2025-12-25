with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day5 is
   Ingredients_File_Name : constant String := "input.txt";

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
   Id_Ranges : array (1 .. C.N_Ranges) of Id_Range;
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

   Put_Line ("The number of fresh ingredients:" & Fresh'Image);
end Day5;
