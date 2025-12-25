with Ada.Text_IO; use Ada.Text_IO;

procedure Day4 is
   type Dimension is
      record
         X, Y : Positive;
      end record;

   function Get_Grid_Size (File_Name : String) return Dimension is
      Rolls_File : File_Type;
   begin
      Open (Rolls_File, In_File, File_Name);
      return Size : Dimension do
         declare
            Line : String := Get_Line (Rolls_File);
         begin
            Size.X := Line'Length;
            Size.Y := 1;
            while not End_Of_File (Rolls_File) loop
               Line := Get_Line (Rolls_File);
               Size.Y := Size.Y + 1;
            end loop;
         end;
         Close (Rolls_File);
      end return;
   end Get_Grid_Size;

   Grid_Size : constant Dimension := Get_Grid_Size ("test.txt");
begin
   Put_Line ("Size of Grid is : " & 
             Positive'Image (Grid_Size.X) &
             " - " &
             Positive'Image (Grid_Size.Y));
end Day4;
